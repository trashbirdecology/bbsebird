#' @title Manipulate the BBS route shapefiles
#' @description This function is meant to intake two shapefiles, one per cws and usgs munges them such that they conform to the USGS data release for BBS observations and route metadata.
#' @param df A data frame with columns c(latitude, longitude) and optional attributes (columns).
#' @param crs.target the integer representing the target CRS.
#' @param grid  a spatial grid over which the eBird data will be overlaid.
#' @param dir.out path to where the resulting spatial data frame will be saved. If NULL will not save to file.
#' @param overwrite logical if TRUE will overwrite any exsiting file named "ebird_spatial.rds" in path dir.out
#' @param cws.routes.dir Directory for where the CWS (Canadian BBS) shapefiles are stored.
#' @param cws.layer Name of the layer to import. Defaults to "ALL_ROUTES"
#' @param usgs.routes.dir Directory for where the USGS (USA BBS) shapefiles are stored.
#' @param plot.dir Directory path for where to save the resultng exploratory pdf (if print.plots==TRUE)
#' @param print.plots logical if TRUE will print exploratory figures to device
#' @param keep.empty.cells logical if FALSE will remove any grid cells with which BBS data do not align. Do not recommend doing this.
#' @param usgs.layer Name of the layer to import.
#' @importFrom sp CRS spTransform
#' @importFrom tidyr expand separate
#' @importFrom sf st_as_sf st_drop_geometry st_transform
#' @importFrom dplyr mutate full_join select mutate group_by
#' @export make_bbs_spatial
make_bbs_spatial <- function(df,
                             ## observations data frame. must contain at least var rteno
                             cws.routes.dir,
                             usgs.routes.dir,
                             #name of cws layer in cws.routes.dir
                             cws.layer = "ALL_ROUTES",
                             # usgs.layer="bbsrte_2012_alb", # this one is from Sauer//outdated, not tested well yet
                             # this was gift by Dave and Danny-DO NT SHARE WITHOUT PERMISSION
                             usgs.layer = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020",
                             crs.target = 4326,
                             grid = NULL,
                             print.plots = TRUE,
                             keep.empty.cells = TRUE,
                             plot.dir = NULL,
                             overwrite = TRUE,
                             dir.out=NULL
                             ) {
  # first, if overwrite is false and this file exists. import and return asap.
  f <- paste0(dir.out, "bbs_spatial.rds")
  if(file.exists(f) & !overwrite){
    cat("File ", f," exists and overwrite.ebird = FALSE. Importing spatial bbs data.")
    bbs_spatial <-readRDS(f)
    return(bbs_spatial)
  }


  # force colanms to lower just in case
  names(df) <- tolower(names(df))

  ## set CRS
  crs.string <- sp::CRS(SRS_string = paste0("EPSG:", crs.target))

## Import and merge CAN and USA BBS routes -------------
  cat("importing route layers")
  # LOAD DATA
  ## CWS route shapefiles
  #### Becauset eh shapfile/gdb sent to me is really old, I cant use sf to import and st_ transform. So, I have to import usign sp readOGR, sp::spTransform, and then convert to sf before merign wtih bbs.
  # because the CWS layer was created using an old geodatabase, we cannot easily use st_transform to re-project the layer.
  cws.gdb <-
    list.files(cws.routes.dir, pattern = ".gdb", full.names = TRUE) %>% stringr::str_remove(".zip") %>% unique()
  cws_routes <- rgdal::readOGR(dsn = cws.gdb, layer = cws.layer)


  # cws_routes <- sf::st_read(dsn=cws.gdb, layer=cws.layer)
  cws_routes <- sp::spTransform(cws_routes, crs.string)
  #coerce to sf
  cws_routes <- sf::st_as_sf(cws_routes)
  ## USGS route shapefiles (circa. 2012)
  ### USGS BBS routes layer obtained from John Sauer.
  ### Indexing by state-route combination
  ### Ex:  state 46 and route 029, rteno==46029
  # usgs_routes <- rgdal::readOGR(dsn=usgs.routes.dir,layer=usgs.layer)
  usgs_routes <- sf::st_read(dsn = usgs.routes.dir, layer = usgs.layer)
  suppressWarnings(usgs_routes <- sf::st_transform(usgs_routes, crs = crs.string))

  # Housekeeping for data inside USGS and CWS routes to match BBS dataset release
  # These fields are applicable only to the Sauer shapefile.
  if (usgs.layer == "bbsrte_2012_alb") {
    usgs_routes <- usgs_routes %>%
      dplyr::mutate(
        countrynum = 840,
        statenum = stringr::str_sub(rteno, start = 1, end = 2),
        Route = stringr::str_sub(rteno, start = 3, end = 5)
      ) %>%
      dplyr::rename(rteno = rteno,
              RouteName = RTENAME)
    # usgs_routes@data$countrynum=840
    # usgs_routes@data$statenum= substr(usgs_routes@data$rteno, 1, 2)
    # usgs_routes@data$Route= substr(usgs_routes@data$rteno,3,5)
    # usgs_routes@data$ = usgs_routes@data$RTENAME
    # usgs_routes@data$routelength = usgs_routes@data$rte_length
    # usgs_routes@data <- make.rteno(usgs_routes@data)
  }
  # this is for the layer shared by Dave Z. and Danny L.
  if (usgs.layer == "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020") {
    # extract rteno and
    usgs_routes <- usgs_routes %>%
      tidyr::separate(
        RouteName,
        into = c("rteno", "RouteName"),
        sep = "_",
        remove = TRUE
      ) %>%
      dplyr::select(-FID_1)
  }

  # Deal with Canadian shapefile (shared by V. Aponte)
  cws_routes <- cws_routes %>%
    dplyr::mutate(
      CountryNum = 124,
      StateNum = stringr::str_sub(ProvRoute, start = 1, end = 2),
      Route = stringr::str_sub(ProvRoute, start = 3, end = 5),
      RouteName = trimws(stringr::str_replace_all(Nbr_FullNa, "[:digit:]|-", ""), "left")
    )
  cws_routes <- bbsAssistant::make.rteno(cws_routes)

  # merge the CA and USA data
  ## keep just a few of same cols-we can delete this but theyre not too useful.
  keep <-
    intersect(tolower(names(usgs_routes)), tolower(names(cws_routes)))
  cws_routes <- cws_routes[, tolower(names(cws_routes)) %in% keep]
  usgs_routes <-
    usgs_routes[, tolower(names(usgs_routes)) %in% keep]

  ### join CWS and USGS routes
  bbs_routes <- dplyr::bind_rows(usgs_routes, cws_routes) %>%
    # Keep only the necessary information.
    # We definitely wnat to remove the following before proceeding:
      ## 1. RouteName: they don't always match the published observations data
      ## 2. ShapeLength or variations thereof: we need to calc route/line length within our desired projections.
    dplyr::select(rteno, geometry) %>%
    #### sometimes when I get to this poitnt when running within a notebook/rmd i get this error: https://github.com/rstudio/rstudio/issues/6260
    ## calculate lengths of lines (may be multiples for one rteno)
    dplyr::mutate(segmentlength = sf::st_length(.)) %>%
    dplyr::group_by(rteno) %>%
    dplyr::mutate(routelength = sum(segmentlength)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-segmentlength) # we don't really gaf about these lines, they're just segments because of the drawings


## Export combined routes layer if no grid is provided...-------------
  # stop here if no grid was provided...
  if (is.null(grid)) {
    return(bbs_routes)
  }


# Project/reproject grid to match bbs_routes layer --------------------------------
# match grid projection/crs to target
grid <- sf::st_transform(grid, crs = crs.string)


# Clip bbs_routes to grid extend ------------------------------------------
# append original (projected) grid to bbs_routes spatial lines layer
cat("overlaying bbs routes and study area grid. this may take a minute or three...\n\n")
bbs.grid.lines <- sf::st_intersection(grid, bbs_routes) # this produces a sf as LINES with grid cell ids appended as attributes.

# Calculate total lengths of routes within a grid cell. -------------------
bbs.grid.lines.df <- bbs.grid.lines %>%
  # This calculate line segments for each row (segment)
  dplyr::mutate(segmentlength = st_length(.)) %>%
  # Calculate the total length of the entire route (regardless of whether the rteno is clipped by the study area..)
  dplyr::group_by(rteno) %>%
  dplyr::mutate(totalroutelength = sum(segmentlength)) %>%
  ## calc proportion as total segment lengths over total route length (within the study area)
  dplyr::group_by(gridcellid, rteno) %>%
  dplyr::mutate(proprouteincell = sum(segmentlength) / totalroutelength) %>%
  dplyr::ungroup()

# create an object describing the rtenos as lines if we want to plot later on
route.line.geometry <- bbs.grid.lines %>%
  dplyr::select(rteno, geometry) %>%
  mutate(route.geometry=geometry) %>%
  st_drop_geometry()


# Expand the grid/study area to include all years and cell combos  -------------
  # expand the grid to include all years and  grid cell ids
grid.expanded <- grid %>%
  as.data.frame() %>%
  ## add years to the grid layer
  tidyr::expand(year = unique(df$year), gridcellid) %>%
# add these to grid attributes attributes
  dplyr::full_join(grid) %>%
  sf::st_as_sf()


# Create BBS Routes as GRIDDED object (not lines) -------------------------
# Join the expanded grid with the BBS routes information
## first, let's remove redundant information (we don't care about the segment lengths../)
bbs.temp <- bbs.grid.lines.df %>%
  st_drop_geometry() %>%
  select(-segmentlength) %>%
  distinct(rteno, gridcellid, proprouteincell, totalroutelength, routelength)

## overlay the bbs routes to the grid
bbs.grid  <- left_join(grid.expanded, bbs.temp)


# Add attributes and obs to BBS gridded layer -----------------------------
## append the route line geometry
bbs.grid <- left_join(bbs.grid, route.line.geometry)
## add the BBS observations to the BBS spatial object
bbs_spatial <- left_join(bbs.grid, df)


# if empty cells not desired, will remove them.
if (!keep.empty.cells){bbs_spatial <-  bbs_spatial %>% filter(!is.na(rteno))}



### return dfs with lowerase
names(cws_routes) <- tolower(names(cws_routes))
names(usgs_routes) <- tolower(names(usgs_routes))



# plot if wanted
  if (print.plots) {
    cat('Printing some plots to:\n')
    if (!is.null(plot.dir)) {
      p.fn=paste0(plot.dir, "/bbs_spatial_exploratory.pdf")
      pdf(file = p.fn)
      cat(plot.dir, " \n")
    }
    # exploratory plots (should move elsewhere.....)
    # plot(bbs.grid[4])
    plot(
      bbs_spatial %>%
        dplyr::filter(!is.na(rteno)) %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(`max num counted` = log(max(c, na.rm = TRUE))) %>%
        dplyr::ungroup() %>%
        dplyr::select(`max num counted`),
      main="maximum number (log-scale) counted in a route-year"
    )
    plot(
      bbs_spatial %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(x =
                           dplyr::n_distinct(obsn)) %>%
        dplyr::select(x),
      main="total # unique observers in cell"
    )
    plot(
      bbs_spatial  %>% group_by(gridcellid) %>%
        dplyr::summarise(x=
                           dplyr::n_distinct(rteno, na.rm = TRUE)) %>%
        dplyr::select(x), main="# routes in datasets"
    )

   suppressWarnings(plot(
      bbs_spatial  %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(x =
                           max(totalspp, na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(x),
      main="max # species detected in single route"
    ))
    plot((
      bbs_spatial %>% dplyr::group_by(gridcellid) %>%
        dplyr::summarise(nRoutesPerCell = dplyr::n_distinct(rteno))
    )["nRoutesPerCell"], main="# bbs routes in cell")


    plot(bbs.grid.lines[6])

    if (!is.null(plot.dir)) {
      dev.off()
      browseURL(p.fn)
    } # end writing to pdf if print.plots specified
  }


# to be safe.
if(dplyr::is_grouped_df(bbs_spatial)) bbs_spatial <- bbs_spatial %>% dplyr::ungroup()

# remove rownames
rownames(bbs_spatial) <- NULL



# Outputs -----------------------------------------------------------------
cat("Writing to file: ", f, "\n")
saveRDS(bbs_spatial, file=f)


return(bbs_spatial)
} ## end function


## side note for jlb: check out this example on identifying distance between points and nearest line
# https://gis.stackexchange.com/questions/269746/find-nearest-line-segment-to-each-point-in-r
  # combo.grid <- grid %>%,%>%  %>%    class()grid
