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
                             print.plots = FALSE,
                             keep.empty.cells = TRUE,
                             plot.dir = NULL) {
  # if plot.dir is NULL and print.plots is TRUE, will just print plots to session and not to file..
  # Warning for proceeding when objects already exist in the workspace
  if (exists("bbs_routes") & overwrite == FALSE) {
    ind = menu(title = "bbs_routes may already exist and `overwrite=FALSE`.\n This function may take 1-2 minutes.Are you sure you want to proceed?",
               choices = c("Yes!", "No."))
    if (ind == 2)
      cat("Function cancelled.")
  }

  # create as string for the crs.target
  # crs.string=sp::CRS(paste0("+init=epsg:", crs.target))
  crs.string = sp::CRS(paste0("+init=epsg:", crs.target))

## Import and merge CAN and USA BBS routes -------------
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
  usgs_routes <- sf::st_transform(usgs_routes, crs = crs.string)


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
        ,
        into = c("RTENO", ""),
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
    ## calculate length of each RTENO (route)
    dplyr::mutate(StringLength = sf::st_length(.)) %>%
    # go ahead and remove route name because they dont match the original data
    dplyr::select(-RouteName)

  cat(
      "Number of unique routes in the CWS and USGS merged routes spatial layer: ",
      length(unique(bbs_routes$RTENO))
    )

## Export combined routes layer if no grid is provided...-------------
  # stop here if no grid was provided...
  if (is.null(grid)) {
    return(bbs_routes)
  }

## Overlay routes and grid/study area  -------------
  # match grid projection/crs to target
  grid <- sf::st_transform(grid, crs = crs.string)
  # expand the grid to include all years and  grid cell ids
  grid.expanded <- grid %>%
    ## add years to the grid layer
    tidyr::expand(Year = unique(bbs_obs$Year), gridcellid)
  # add these to grid attributes attributes
  grid.expanded <- full_join(grid, grid.expanded)

  ## calculate the lengths of Routes and Route Segments within grid cells/ids
  cat(
    "BBS routes overlying grid/study area.
    Takes some many minutes for more than 3 states/provinces.\n\n"
  )

### Calculate route and segment lengths -----------
# create line and polygon overlay sof grid and bbs_routes
lines.bbs.intrsct <-  sf::st_intersection(grid.expanded, bbs_routes) # this produces a sf as LINES with grid cell ids appended as attributes.


# calculate segment lengths inside each grid
lines.bbs.intrsct2 <- lines.bbs.intrsct %>%
  # calculate length of route inside a grid cell length
  dplyr::group_by(RTENO, Year, gridcellid) %>%
  dplyr::mutate(SegmentLength = sf::st_length(.))

  # calculate total route length
  dplyr::group_by(RTENO, Year) %>%
  dplyr::mutate(RouteLength = sf::st_length(.)) %>%
  ungroup()


## create a data frame to append the RTENO geoemtry if you want to plot the route lines later on for some reason..
route.geometry <- lines.bbs.intrsct %>%
  dplyr::select(gridcellid, Year, RTENO, RouteLength) %>%
  mutate(route.geometry=lines.bbs.intrsct$geometry) %>%
  st_drop_geometry()

### Add line geometry and route lengths to gridded BBS observations---------
# Append the RTENO geometry/lengths to the grid/bbs overlay
grid.bbs.intrsct  <-  sf::st_join(grid.expanded, bbs_routes) # this produces a sf as LINES with grid cell ids appended as attributes.

bbs.df <- full_join(grid.bbs.intrsct, route.geometry)


bbs_sf.df2 <- bbs.df %>%
    dplyr::group_by(gridcellid, RTENO, Year) %>%
    ### turn seg length into proportion of the route per cell
    dplyr::mutate(PropRouteInCell = SegmentLength / RouteLength) %>%
    dplyr::ungroup() %>%
    sf::st_drop_geometry() %>% # doing this drops removes it as a line feature
    dplyr::select( -RouteName)# remove route name: minor issue, but Route Names in route shapefiles do not match the BBS observations data route names.
  ## Add the BBS observations to the BBS spatial object
  bbs_sf <- full_join(bbs_sf.df2, bbs_obs)


  ## Add the observations tot he spatial layer.
  # append the empty cells if keep.empty.cells is TRUE, or use merge to leave them out.
  # imputes data for each cell where no BBS exists in a year.
  bbs_spatial <-
      dplyr::left_join(bbs_sf, bbs_obs)


  # if empty cells not desired, will remove them.
  if (!keep.empty.cells){bbs_spatial <-  bbs_spatial %>% filter(!is.na(RTENO))}

  # plot if wanted
  if (print.plots) {
    cat('Making some plots...\n')
    if (!is.null(plot.dir)) {
      pdf(file=paste0(dir.plots, "/bbs_spatial_exploratory.pdf"))
      cat("plots printing to: ", dir.plots, " \n")
    }

    # exploratory plots (should move elsewhere.....)
    plot(
      bbs_spatial %>%
        dplyr::filter(!is.na(RTENO)) %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::summarise(`max num counted` = max(RouteTotal, na.rm = TRUE)) %>%
        dplyr::select(`max num counted`)
    )
    plot(
      bbs_spatial %>% dplyr::group_by(RTENO) %>% dplyr::summarise(`num years bbs data in grid cell` =
                                                                    dplyr::n_distinct(Year)) %>%
        dplyr::select(`num years bbs data in grid cell`)
    )
    plot(
      bbs_spatial %>% dplyr::group_by(gridcellid) %>% dplyr::summarise(`total # observers in cell` =
                                                                  dplyr::n_distinct(ObsN)) %>%
        dplyr::select(`total # observers in cell`)
    )
    plot(
      bbs_spatial  %>% group_by(gridcellid) %>%  dplyr::summarise(`num routes in cell` = dplyr::n_distinct(RTENO, na.rm =
                                                                                       TRUE)) %>%
        dplyr::select(`num routes in cell`)
    )
    plot(
      bbs_spatial  %>% dplyr::group_by(gridcellid) %>% dplyr::summarise(`max # species detected in single route` =
                                                    max(TotalSpp)) %>%
        dplyr::select(`max # species detected in single route`)
    )
    plot((
      bbs_spatial %>% dplyr::group_by(gridcellid) %>%
        dplyr::summarise(nRoutesPerCell = dplyr::n_distinct(RTENO))
    )
    ["nRoutesPerCell"])

    plot(
      bbs_spatial %>%
        dplyr::filter(!is.na(RTENO)) %>%
        dplyr::group_by(gridcellid) %>%
        dplyr::mutate(num_obs = n()) %>%
        dplyr::summarise(`% obs == 0` = sum(RouteTotal == 0) / n()) %>%
        dplyr::select(`% obs == 0`)
    )

    if (!is.null(plot.dir)) {
      dev.off()
    } # end writing to pdf if print.plots specified
  }


  # to be safe.
  bbs_spatial <- bbs_spatial %>% ungroup()
  return(bbs_spatial)

}


## side note for jlb: check out this example on identifying distance between points and nearest line
# https://gis.stackexchange.com/questions/269746/find-nearest-line-segment-to-each-point-in-r
  # combo.grid <- grid %>%,%>%  %>%    class()grid
