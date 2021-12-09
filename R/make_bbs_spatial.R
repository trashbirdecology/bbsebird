#' @title Manipulate the BBS route shapefiles
#' @description This function is meant to intake two shapefiles, one per cws and usgs munges them such that they conform to the USGS data release for BBS observations and route metadata.
#' @param cws.routes.dir Directory for where the CWS (Canadian BBS) shapefiles are stored.
#' @param cws.layer Name of the layer to import. Defaults to "ALL_ROUTES"
#' @param usgs.routes.dir Directory for where the USGS (USA BBS) shapefiles are stored.
#' @param usgs.layer Name of the layer to import.
#' @export
### I NEED TO CHANGE NAEM TO LIKE, CREATE BBS SPATIAL
#### ALso, prob include an optin to define the pre=existuing grid
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
  if (exists("bbs_routes") & overwrite == FALSE) {
    ind = menu(title = "bbs_routes may already exist and `overwrite=FALSE`.\n This function may take 1-2 minutes.Are you sure you want to proceed?",
               choices = c("Yes!", "No."))
    if (ind == 2)
      cat("Function cancelled.")
  }

  # create as string for the crs.target
  # crs.string=CRS(paste0("+init=epsg:", crs.target))
  crs.string = CRS(paste0("+init=epsg:", crs.target))


  # LOAD DATA
  ## CWS route shapefiles
  #### Becauset eh shapfile/gdb sent to me is really old, I cant use sf to import and st_ transform. So, I ahve to import usign sp readOGR, spTransform, and then convert to sf before merign wtih bbs.
  # because the CWS layer was created using an old geodatabase, we cannot easily use st_transform to re-project the layer.
  cws.gdb <-
    list.files(cws.routes.dir, pattern = ".gdb", full.names = TRUE) %>% str_remove(".zip") %>% unique()
  cws_routes <- readOGR(dsn = cws.gdb, layer = cws.layer)
  # cws_routes <- sf::st_read(dsn=cws.gdb, layer=cws.layer)
  cws_routes <- spTransform(cws_routes, crs.string)
  #coerce to sf
  cws_routes <- st_as_sf(cws_routes)
  ## USGS route shapefiles (circa. 2012)
  ### USGS BBS routes layer obtained from John Sauer.
  ### Indexing by state-route combination
  ### Ex:  state 46 and route 029, RTENO==46029
  # usgs_routes <- readOGR(dsn=usgs.routes.dir,layer=usgs.layer)
  usgs_routes <- st_read(dsn = usgs.routes.dir, layer = usgs.layer)
  usgs_routes <- st_transform(usgs_routes, crs = crs.string)


  # Housekeeping for data inside USGS and CWS routes to match BBS dataset release
  # These fields are applicable only to the Sauer shapefile.
  if (usgs.layer == "bbsrte_2012_alb") {
    usgs_routes <- usgs_routes %>%
      mutate(
        CountryNum = 840,
        StateNum = str_sub(rteno, start = 1, end = 2),
        Route = str_sub(rteno, start = 3, end = 5)
      ) %>%
      rename(RTENO = rteno,
             RouteName = RTENAME,)
    # usgs_routes@data$CountryNum=840
    # usgs_routes@data$StateNum= substr(usgs_routes@data$rteno, 1, 2)
    # usgs_routes@data$Route= substr(usgs_routes@data$rteno,3,5)
    # usgs_routes@data$RouteName = usgs_routes@data$RTENAME
    # usgs_routes@data$RouteLength = usgs_routes@data$rte_length
    # usgs_routes@data <- make.rteno(usgs_routes@data)
  }
  # this is for the layer shared by Dave Z. and Danny L.
  if (usgs.layer == "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020") {
    # extract RTENO and RouteName
    usgs_routes <- usgs_routes %>%
      separate(
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
      StateNum = str_sub(ProvRoute, start = 1, end = 2),
      Route = str_sub(ProvRoute, start = 3, end = 5),
      RouteName = trimws(str_replace_all(Nbr_FullNa, "[:digit:]|-", ""), "left")
    )
  cws_routes <- make.rteno(cws_routes)

  ## Keep routes if provided to save time on the intersection.
  if (!is.null(routes.keep)) {
    if (!class(cws_routes$RTENO) == class(routes.keep)) {
      stop("Classes of cws routes RTENO don't match. Fix in munge_bbs_shapefiles.R pls")
    }
    cws_routes <- cws_routes %>% filter(RTENO %in% routes.keep)
    if (!class(usgs_routes$RTENO) == class(routes.keep)) {
      stop("Classes of cws routes RTENO don't match. Fix in munge_bbs_shapefiles.R pls")
    }
    usgs_routes <- usgs_routes %>% filter(RTENO %in% routes.keep)
  }


  # merge the CA and USA data
  ## keep just a few of same cols-we can delete this but theyre not too useful.
  keep <-
    intersect(tolower(names(usgs_routes)), tolower(names(cws_routes)))
  cws_routes <- cws_routes[, tolower(names(cws_routes)) %in% keep]
  usgs_routes <-
    usgs_routes[, tolower(names(usgs_routes)) %in% keep]

  ### join CWS and USGS routes
  bbs_sf <- bind_rows(usgs_routes, cws_routes)
  message(
    paste0(
      "Number of unique routes in the CWS and USGS merged routes spatial layer: ",
      length(unique(bbs_sf$RTENO))
    )
  )

  # stop here if no grid was provided...
  if (is.null(grid)) {
    return(bbs_sf)
  }



  # if grid was provided, overlay and calculate lengths and areas
  grid <- st_transform(grid, crs = crs.string)

  ## clip bbs_sf to grid extent
  # and calculating Route and Segment lengths. May take a minute or three.")
  ## calculate the lengths of Routes and Route Segments within grid cells/ids
  cat(
    "Clipping BBS routes to grid extent. Takes some hot minutessss for more than 3 states/provinces.\n\n"
  )
  lengths <- st_intersection(grid, bbs_sf)

  cat("Calculating route and segment lengths..\n\n")
  lengths <- lengths %>%
    ## length of segment within a cell
    mutate(SegmentLength = st_length(.)) %>%
    group_by(RTENO) %>%
    ## total length of routes
    mutate(RouteLength = sum(SegmentLength)) %>%
    ungroup() %>%
    group_by(id, RTENO) %>%
    ### turn seg length into proportion of the route per cell
    mutate(PropRouteInCell = SegmentLength / RouteLength) %>%
    ungroup() %>%
    st_drop_geometry() # complicates things in joins later on
  ## calculate grid cell areas
  cat("Calculating grid cell areas..\n\n")
  areas <- grid %>%
    mutate(CellArea = st_area(.)) %>%
    st_drop_geometry() # complicates things in joins later on

  ## combine bbs routes + area + lengthss
  cat("Polishing the BBS routes and grid layer.\n\n")
  bbs_sf <- grid %>%
    left_join(lengths, by = "id") %>%
    left_join(areas, by = "id")


  ### Minor issue, but Route Names in route shaepfiles do not match the BBS observations data route names.
  if ("routename" %in% tolower(names(bbs_obs))) {
    bbs_obs <-  bbs_obs %>%
      dplyr::select(-RouteName)
  }

  ## Add the observations tot he spatial layer.
  # append the empty cells if keep.empty.cells is TRUE, or use merge to leave them out.
  if (keep.empty.cells) {
    bbs_spatial <-
      left_join(bbs_sf, bbs_obs, by = "RTENO") %>%
      # create var with percent route in grid cell
      mutate(PercSegmentInCell = SegmentLength / RouteLength)

  } else{
    bbs_spatial <-
      merge(bbs_sf, bbs_obs, by = "RTENO") %>%
      # create var with percent route in grid cell
      mutate(PercSegmentInCell = SegmentLength / RouteLength)

  }


  # plot if wanted
  if (print.plots) {
    cat('Making some plots...\n')
    if (!is.null(plot.dir)) {
      pdf(file=paste0(dir.plots, "/bbs_spatial_exploratory.pdf"))
      cat("plots printing to: ", dir.plots, " \n")
    }

    # exploratory plots (should move elsewhere.....)
    plot(
      bbs_spatial %>% group_by(id) %>% summarise(`max num counted` = max(RouteTotal)) %>%
        dplyr::select(`max num counted`)
    )
    plot(
      bbs_spatial %>% group_by(RTENO) %>% summarise(`num years bbs data in grid cell` =
                                                      n_distinct(Year)) %>%
        dplyr::select(`num years bbs data in grid cell`)
    )
    plot(
      bbs_spatial %>% group_by(id) %>% summarise(`total # observers in cell` =
                                                   n_distinct(ObsN)) %>%
        dplyr::select(`total # observers in cell`)
    )
    plot(
      bbs_spatial  %>% group_by(id) %>%  summarise(`num routes in cell` = n_distinct(RTENO, na.rm =
                                                                                       TRUE)) %>%
        dplyr::select(`num routes in cell`)
    )
    plot(
      bbs_spatial  %>% group_by(id) %>% summarise(`max # species detected in single route` =
                                                    max(TotalSpp)) %>%
        dplyr::select(`max # species detected in single route`)
    )
    plot(bbs_spatial %>% group_by(id) %>% summarise(nRoutesPerCell = n_distinct(RTENO)))
    plot(
      bbs_spatial %>% group_by(id) %>%
        mutate(num_obs = n()) %>%
        summarise(`% obs == 0` = sum(RouteTotal == 0) / n()) %>% dplyr::select(`% obs == 0`)
    )

    if (!is.null(plot.dir)) {
      dev.off()
    } # finish writing to pdf if print.plots specified
  }



  return(bbs_spatial)

}


## side note for jlb: check out this example on identifying distance between points and nearest line
# https://gis.stackexchange.com/questions/269746/find-nearest-line-segment-to-each-point-in-r
