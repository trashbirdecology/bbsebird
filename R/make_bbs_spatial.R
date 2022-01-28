#' @title Manipulate the BBS route shapefiles
#' @description This function is meant to intake two shapefiles, one per cws and usgs munges them such that they conform to the USGS data release for BBS observations and route metadata.
#' @param cws.routes.dir Directory for where the CWS (Canadian BBS) shapefiles are stored.
#' @param cws.layer Name of the layer to import. Defaults to "ALL_ROUTES"
#' @param usgs.routes.dir Directory for where the USGS (USA BBS) shapefiles are stored.
#' @param usgs.layer Name of the layer to import.
#' @export
make_bbs_spatial <- function(bbs.obs,
                             ## observations data frame. must contain at least var RTENO
                             cws.routes.dir,
                             usgs.routes.dir,
                             routes.keep = NULL,
                             cws.layer = "ALL_ROUTES",
                             #name of cws layer in cws.routes.dir
                             # usgs.layer="bbsrte_2012_alb", # this one is from Sauer//outdated, not tested well yet
                             usgs.layer = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020",
                             # this was gift by Dave and Danny-DO NT SHARE
                             crs.target = 4326,
                             grid = NULL,
                             overwrite = TRUE,
                             print.plots = TRUE,
                             keep.empty.cells = TRUE,
                             plot.dir = NULL) {
  # if plot.dir is NULL and print.plots is TRUE, will just print plots to session and not to file..
  # Warning for proceeding when objects already exist in the workspace
  if (exists("bbs_spatial") & overwrite == FALSE) {
    ind = menu(title = "bbs_routes may already exist and `overwrite=FALSE`.\n This function may take 1-2 minutes.Are you sure you want to proceed?",
               choices = c("Yes!", "No."))
    if (ind == 2)
      cat("Function cancelled.")
  }

  # create as string for the crs.target
  # crs.string=sp::CRS(paste0("+init=epsg:", crs.target))
  crs.string = sp::CRS(paste0("+init=epsg:", crs.target))

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
  ### Ex:  state 46 and route 029, RTENO==46029
  # usgs_routes <- rgdal::readOGR(dsn=usgs.routes.dir,layer=usgs.layer)
  usgs_routes <- sf::st_read(dsn = usgs.routes.dir, layer = usgs.layer)
  suppressWarnings(usgs_routes <- sf::st_transform(usgs_routes, crs = crs.string))

  # Housekeeping for data inside USGS and CWS routes to match BBS dataset release
  # These fields are applicable only to the Sauer shapefile.
  if (usgs.layer == "bbsrte_2012_alb") {
    usgs_routes <- usgs_routes %>%
      dplyr::mutate(
        CountryNum = 840,
        StateNum = stringr::str_sub(rteno, start = 1, end = 2),
        Route = stringr::str_sub(rteno, start = 3, end = 5)
      ) %>%
      dplyr::rename(RTENO = rteno,
              RouteName = RTENAME)
    # usgs_routes@data$CountryNum=840
    # usgs_routes@data$StateNum= substr(usgs_routes@data$rteno, 1, 2)
    # usgs_routes@data$Route= substr(usgs_routes@data$rteno,3,5)
    # usgs_routes@data$ = usgs_routes@data$RTENAME
    # usgs_routes@data$RouteLength = usgs_routes@data$rte_length
    # usgs_routes@data <- make.rteno(usgs_routes@data)
  }
  # this is for the layer shared by Dave Z. and Danny L.
  if (usgs.layer == "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020") {
    # extract RTENO and
    usgs_routes <- usgs_routes %>%
      tidyr::separate(
        RouteName,
        into = c("RTENO", "RouteName"),
        sep = "_",
        remove = TRUE
      ) %>%
      dplyr::select(-FID_1)
  }

  # Deal with Canadian shapefile (shared by V. Aponte)
  cws_routes <- cws_routes %>%
    mutate(
      CountryNum = 124,
      StateNum = stringr::str_sub(ProvRoute, start = 1, end = 2),
      Route = stringr::str_sub(ProvRoute, start = 3, end = 5),
      RouteName = trimws(stringr::str_replace_all(Nbr_FullNa, "[:digit:]|-", ""), "left")
    )
  cws_routes <- bbsAssistant::make.rteno(cws_routes)

  ## Keep routes if provided to save time on the intersection.
  if (!is.null(routes.keep)) {
    if (!class(cws_routes$RTENO) == class(routes.keep)) {
      stop("Classes of cws routes RTENO don't match. Fix in munge_bbs_shapefiles.R pls")
    }
    cws_routes <- cws_routes %>% dplyr::filter(RTENO %in% routes.keep)
    if (!class(usgs_routes$RTENO) == class(routes.keep)) {
      stop("Classes of cws routes RTENO don't match. Fix in munge_bbs_shapefiles.R pls")
    }
    usgs_routes <- usgs_routes %>% dplyr::filter(RTENO %in% routes.keep)
  }

  # merge the CA and USA data
  ## keep just a few of same cols-we can delete this but theyre not too useful.
  keep <-
    intersect(tolower(names(usgs_routes)), tolower(names(cws_routes)))
  cws_routes <- cws_routes[, tolower(names(cws_routes)) %in% keep]
  usgs_routes <-
    usgs_routes[, tolower(names(usgs_routes)) %in% keep]

  ### join CWS and USGS routes
  bbs_routes <- bind_rows(usgs_routes, cws_routes) %>%
    # Keep only the necessary information.
    # We definitely wnat to remove the following before proceeding:
      ## 1. RouteName: they don't always match the published observations data
      ## 2. ShapeLength or variations thereof: we need to calc route/line length within our desired projections.
    dplyr::select(RTENO, geometry) %>%
    ## calculate lengths of lines (may be multiples for one RTENO)
    dplyr::mutate(SegmentLength = sf::st_length(.)) %>%
    group_by(RTENO) %>%
    mutate(RouteLength = sum(SegmentLength)) %>%
    ungroup() %>%
    dplyr::select(-SegmentLength) # we don't really gaf about these lines, they're just segments because of the drawings


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
  mutate(SegmentLength = st_length(.)) %>%
  # Calculate the total length of the entire route (regardless of whether the rteno is clipped by the study area..)
  group_by(RTENO) %>%
  mutate(TotalRouteLength = sum(SegmentLength)) %>%
  ## calc proportion as total segment lengths over total route length (within the study area)
  group_by(gridcellid, RTENO) %>%
  dplyr::mutate(PropRouteInCell = sum(SegmentLength) / TotalRouteLength) %>%
  dplyr::ungroup()

# create an object describing the RTENOs as lines if we want to plot later on
route.line.geometry <- bbs.grid.lines %>%
  dplyr::select(RTENO, geometry) %>%
  mutate(route.geometry=geometry) %>%
  st_drop_geometry()


# Expand the grid/study area to include all years and cell combos  -------------
  # expand the grid to include all years and  grid cell ids
grid.expanded <- grid %>%
  as.data.frame() %>%
  ## add years to the grid layer
  tidyr::expand(Year = unique(bbs.obs$Year), gridcellid) %>%
# add these to grid attributes attributes
  full_join(grid) %>%
  sf::st_as_sf()


# Create BBS Routes as GRIDDED object (not lines) -------------------------
# Join the expanded grid with the BBS routes information
## first, let's remove redundant information (we don't care about the segment lengths../)
bbs.temp <- bbs.grid.lines.df %>%
  st_drop_geometry() %>%
  select(-SegmentLength) %>%
  distinct(RTENO, gridcellid, PropRouteInCell, TotalRouteLength, RouteLength)

## overlay the bbs routes to the grid
bbs.grid  <- left_join(grid.expanded, bbs.temp)


# Add attributes and obs to BBS gridded layer -----------------------------
## append the route line geometry
bbs.grid <- left_join(bbs.grid, route.line.geometry)
## add the BBS observations to the BBS spatial object
bbs_spatial <- left_join(bbs.grid, bbs.obs)


# if empty cells not desired, will remove them.
if (!keep.empty.cells){bbs_spatial <-  bbs_spatial %>% filter(!is.na(RTENO))}

# plot if wanted
  if (print.plots) {
    cat('Printing some plots to:\n')
    if (!is.null(plot.dir)) {
      pdf(file=paste0(plot.dir, "/bbs_spatial_exploratory.pdf"))
      cat(plot.dir, " \n")
    }
    # exploratory plots (should move elsewhere.....)
    plot(bbs.grid[4])
    plot(
      bbs_spatial %>%
        dplyr::filter(!is.na(RTENO)) %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(`max num counted` = max(RouteTotal, na.rm = TRUE)) %>%
        dplyr::select(`max num counted`)
    )
    plot(
      bbs_spatial %>%
        dplyr::filter(!is.na(RTENO)) %>%
        dplyr::group_by(RTENO) %>% dplyr::summarise(`num years bbs data in grid cell` =
                                                                    dplyr::n_distinct(Year)) %>%
        dplyr::select(`num years bbs data in grid cell`)
    )
    plot(
      bbs_spatial %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(`total # observers in cell` =
                           dplyr::n_distinct(ObsN)) %>%
        dplyr::select(`total # observers in cell`)
    )
    plot(
      bbs_spatial  %>% group_by(gridcellid) %>%
        dplyr::summarise(`num routes in cell` =
                           dplyr::n_distinct(RTENO, na.rm = TRUE)) %>%
        dplyr::select(`num routes in cell`)
    )
    plot(
      bbs_spatial  %>%
        dplyr::filter(!is.na(TotalSpp)) %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(`max # species detected in single route` =
                           max(TotalSpp)) %>%
        dplyr::select(`max # species detected in single route`)
    )
    plot((
      bbs_spatial %>% dplyr::group_by(gridcellid) %>%
        dplyr::summarise(nRoutesPerCell = dplyr::n_distinct(RTENO))
    )["nRoutesPerCell"])


    if (!is.null(plot.dir)) {
      dev.off()
    } # end writing to pdf if print.plots specified
  }


  # to be safe.
  if(dplyr::is_grouped_df(bbs_spatial)) bbs_spatial <- bbs_spatial %>% ungroup()




return(bbs_spatial)
} ## end function


## side note for jlb: check out this example on identifying distance between points and nearest line
# https://gis.stackexchange.com/questions/269746/find-nearest-line-segment-to-each-point-in-r
  # combo.grid <- grid %>%,%>%  %>%    class()grid
