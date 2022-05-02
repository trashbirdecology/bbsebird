#' @title Overlay all BBS Routes On a Spatial Grid for Use in Multiple Projects
#' @description This function is meant to intake two shapefiles, one per cws and usgs munges them such that they conform to the USGS data release for BBS observations and route metadata.
#' @param crs.target the integer representing the target CRS.
#' @param grid  a spatial grid over which the eBird data will be overlaid.
#' @param dir.out path to where the resulting spatial data frame will be saved. If NULL will not save to file.
#' @param overwrite logical if TRUE will overwrite any existing file named "ebird_spatial.rds" in path dir.out
#' @param cws.routes.dir Directory for where the CWS (Canadian BBS) shapefiles are stored.
#' @param cws.layer Name of the layer to import. Defaults to "ALL_ROUTES"
#' @param usgs.routes.dir Directory for where the USGS (USA BBS) shapefiles are stored.
#' @param keep.empty.cells logical if FALSE will remove any grid cells with which BBS data do not align. Do not recommend doing this.
#' @param usgs.layer Name of the layer to import.
#' @param ncores max number of cores to engage for parallel data processing. Defaults to one fewer CPUs than the machine has. Parallel processing is used only when a high number of routes and/or grid cells are in the data.
#' @param save.route.lines logical. If TRUE (default) will save the BBS routes segments as .RDS to dir.out
#' @importFrom foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster detectCores makeCluster
#' @importFrom sp CRS spTransform
#' @importFrom stringr str_sub
#' @importFrom tidyr expand separate
#' @importFrom sf st_as_sf st_drop_geometry st_transform st_length st_area st_read
#' @importFrom dplyr mutate full_join select mutate group_by ungroup distinct rename left_join summarise filter
#' @export make_bbs_spatial
make_base_grid_bbs <- function(grid,
                             ## observations data frame. must contain at least var rteno
                             cws.routes.dir,
                             usgs.routes.dir,
                             #name of cws layer in cws.routes.dir
                             cws.layer = "ALL_ROUTES",
                             # usgs.layer="bbsrte_2012_alb", # this one is from Sauer//outdated, not tested well yet
                             # this was gift by Dave and Danny-DO NT SHARE WITHOUT PERMISSION
                             usgs.layer = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020",
                             crs.target = 4326,
                             ncores = parallel::detectCores() - 1,
                             keep.empty.cells = TRUE,
                             overwrite = FALSE,
                             dir.out = NULL,
                             save.route.lines = TRUE) {
  # first, if overwrite is false and this file exists. import and return asap.
  f <- paste0(dir.out, "bbs_base_grid.rds")
  if (file.exists(f) & !overwrite) {
    cat("File ",
        f,
        " exists and overwrite = FALSE. Importing existing spatial bbs data.\n")
    bbs_spatial <- readRDS(f)
    return(bbs_spatial)
  }
  
  ## set CRS
  crs.string <- sp::CRS(SRS_string = paste0("EPSG:", crs.target))
  
  ## Import and merge CAN and USA BBS routes -------------
  cat("importing route layers")
  # LOAD DATA
  ## CWS route shapefiles
  #### Becauset eh shapfile/gdb sent to me is really old, I cant use sf to import and st_ transform. So, I have to import usign sp readOGR, sp::spTransform, and then convert to sf before merign wtih bbs.
  # because the CWS layer was created using an old geodatabase, we cannot easily use st_transform to re-project the layer.
  cws.gdb <-
    list.files(cws.routes.dir, pattern = ".gdb", full.names = TRUE) |> stringr::str_remove(".zip") |> unique()
  suppressWarnings(cws_routes <-
                     rgdal::readOGR(
                       dsn = cws.gdb,
                       layer = cws.layer,
                       verbose = FALSE
                     ))#suppress a warning about dropping Z-dimension
  
  # cws_routes <- sf::st_read(dsn=cws.gdb, layer=cws.layer)
  cws_routes <- sp::spTransform(cws_routes, crs.string)
  #coerce to sf
  cws_routes <- sf::st_as_sf(cws_routes)
  ## USGS route shapefiles (circa. 2012)
  ### USGS BBS routes layer obtained from John Sauer.
  ### Indexing by state-route combination
  ### Ex:  state 46 and route 029, rteno==46029
  # usgs_routes <- rgdal::readOGR(dsn=usgs.routes.dir,layer=usgs.layer)
  usgs_routes <-
    sf::st_read(dsn = usgs.routes.dir, layer = usgs.layer)
  usgs_routes <- sf::st_transform(usgs_routes, crs = crs.string)
  
  # Housekeeping for data inside USGS and CWS routes to match BBS dataset release
  # These fields are applicable only to the Sauer shapefile.
  if (usgs.layer == "bbsrte_2012_alb") {
    usgs_routes <- usgs_routes |>
      dplyr::mutate(
        countrynum = 840,
        statenum = stringr::str_sub(rteno, start = 1, end = 2),
        Route = stringr::str_sub(rteno, start = 3, end = 5)
      ) |>
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
    usgs_routes <- usgs_routes |>
      tidyr::separate(
        RouteName,
        into = c("rteno", "RouteName"),
        sep = "_",
        remove = TRUE
      ) |>
      dplyr::select(-FID_1)
  }
  
  # Deal with Canadian shapefile (shared by V. Aponte)
  cws_routes <- cws_routes |>
    dplyr::mutate(
      CountryNum = 124,
      StateNum = stringr::str_sub(ProvRoute, start = 1, end = 2),
      Route = stringr::str_sub(ProvRoute, start = 3, end = 5),
      RouteName = trimws(
        stringr::str_replace_all(Nbr_FullNa, "[:digit:]|-", ""),
        "left"
      )
    )
  
  cws_routes <- bbsAssistant::make.rteno(cws_routes)
  names(cws_routes) <- tolower(names(cws_routes))
  names(usgs_routes) <- tolower(names(usgs_routes))
  
  # merge the CA and USA BBS routes
  ## keep just a few of same cols-we can delete this but theyre not too useful.
  keep <-
    intersect(tolower(names(usgs_routes)), tolower(names(cws_routes)))
  cws_routes  <- cws_routes[, tolower(names(cws_routes)) %in% keep]
  usgs_routes <-
    usgs_routes[, tolower(names(usgs_routes)) %in% keep]
  
  ### join CWS and USGS routes
  bbs_routes <- dplyr::bind_rows(usgs_routes, cws_routes) |>
    # Keep only the necessary information.
    # We definitely wnat to remove the following before proceeding:
    ## 1. RouteName: they don't always match the published observations data
    ## 2. ShapeLength or variations thereof: we need to calc route/line length within our desired projections.
    dplyr::select(rteno, geometry)
  
  
  ## Calculate segment lengths and then add up to grab route lengths
  ## (some routes have gaps and are therefore represented on different line objects/rows)
  bbs_routes$segmentlength <-  bbs_routes |> sf::st_length() # this is not currently incompatible when called inside using (.) native pipe operator,
  ## so i had to break it out of this workflow chunk..
  bbs_routes <- bbs_routes |>
    #### sometimes when I get to this poitnt when running within a notebook/rmd i get this error: https://github.com/rstudio/rstudio/issues/6260
    ## calculate lengths of lines (may be multiples for one rteno)
    dplyr::group_by(rteno) |>
    dplyr::mutate(routelength = sum(segmentlength)) |>
    dplyr::ungroup() |>
    dplyr::select(-segmentlength) # we don't really gaf about these lines, they're just segments because of the drawings
  ## Drop the z-dimension from data. Exists in USGS but not in CWS
  bbs_routes <- sf::st_zm(bbs_routes, drop = TRUE, what = "ZM")
  
  
  
  # Project/reproject grid to match bbs_routes layer --------------------------------
  # match grid projection/crs to target
  grid <- sf::st_transform(grid, crs = crs.string)
  
  # Clip bbs_routes to grid extent and overlay grid cells ------------------------------------------
  # append original (projected) grid to bbs_routes spatial lines layer
  cat(
    "overlaying bbs routes and study area grid. this may take a minute or three...or ten....sorry bruh\n\n"
  )
  
  ### chunk up processing of st_intersection to speed up overlay
  # add process for:: if ngrids>X and chunks > Y then parallel, else just run straight up
  message(
    "[note] overlaying bbs route and grid (study area). This may take a few minutes depending on size of grid cells and extent of study area..\n"
  )
  nroutes  <- nrow(bbs_routes)
  r_vec    <- 1:nroutes
  r_chunks <- split(r_vec, f = ceiling(seq_along(r_vec) / round(len / ncores) + 1 ))
  
  ngrids    <- nrow(grid)
  g_vec    <- 1:nroutes
  g_chunks    <- split(g_vec, f = ceiling(seq_along(g_vec) / round(len / ncores) + 1 ))
  ## this needs to be parallellized..
  ## loop over all the BBS route chunks
  mylist <- list()
  for(ii in 1:length(r_chunks)){
    r.temp <- bbs_routes[r_chunks[[ii]],] 
    ### loop over all the grid chunks
    for(jj in 1:length(g_chunks)){ 
    print(paste(ii, "-", jj))
      if(jj==1) temp <- list()
      g.temp <- grid[g_chunks[[jj]],]
      # temp[[jj]] <- g.temp
      temp[[jj]] <- sf::st_intersection(g.temp, r.temp)
    }
    mylist[[ii]] <- dplyr::bind_rows(temp)
    rm(temp)
  }
  ### not sure if I can go ahead and just bind the rows...so if the calculate segment length doesnt work then try st_join again
  
  # ## bind rows...
  # for(i in seq_along(mylist)){
  #   print(paste0("spatial joining: ", i, " of ", length(mylist)))
  #   if(i == 1) bbs.grid.lines <- mylist[[1]]  else bbs.grid.lines <- sf::st_join(mylist[[i]], new)
  # }
  bbs.grid.lines <- dplyr::bind_rows(mylist)
  message("[note]...overlay was great success! jagshemash \n")
  # rm(mylist)
  
  # Calculate total lengths of routes WITHIN EAcH GRID CELL -------------------
  bbs.grid.lines.df <- bbs.grid.lines
  bbs.grid.lines.df$segmentlength  <- sf::st_length(bbs.grid.lines)
  bbs.grid.lines.df <- bbs.grid.lines.df |>
    # Calculate the total length of the entire route (regardless of whether the rteno is clipped by the study area..)
    dplyr::group_by(rteno) |>
    dplyr::mutate(totalroutelength = sum(segmentlength)) |>
    ## calc proportion as total segment lengths over total route length (within the study area)
    dplyr::group_by(gridcellid, rteno) |>
    dplyr::mutate(proprouteincell = sum(segmentlength) / totalroutelength) |>
    dplyr::ungroup()
  
  # create an object describing the rtenos as lines if we want to plot later on
  geom  <- sf::st_geometry(bbs.grid.lines)
  route.line.geometry <- bbs.grid.lines |> select(rteno) |>
    sf::st_drop_geometry() |>
    mutate(geometry  = geom)
  rm(geom)
 
  # Create BBS Routes as GRIDDED object (not lines) -------------------------
  # Join the expanded grid with the BBS routes information
  ## first, let's remove redundant information (we don't care about the segment lengths../)
  bbs.temp <- bbs.grid.lines.df |>
    sf::st_drop_geometry() |>
    dplyr::select(-segmentlength) |>
    dplyr::distinct(rteno,
                    gridcellid,
                    proprouteincell,
                    totalroutelength,
                    routelength)
  
  ## overlay the bbs routes to the grid, again.
  bbs_spatial  <- dplyr::left_join(grid, bbs.temp, by="gridcellid")
  
  # Add attributes and obs to BBS gridded layer -----------------------------
  
  ## add the BBS observations to the BBS spatial object
  # bbs_spatial <- left_join(bbs.grid, df)
  
  # if empty cells not desired, will remove them.
  if (!keep.empty.cells) {
    bbs_spatial <-  bbs_spatial |> dplyr::filter(!is.na(rteno))
  }
  
  
  
  # just to be safe I guess
  if (dplyr::is_grouped_df(bbs_spatial))
    bbs_spatial <- bbs_spatial |> dplyr::ungroup()
  
  # remove rownames
  rownames(bbs_spatial) <- NULL
  
 
  # Outputs -----------------------------------------------------------------
  if(save.route.lines){
    f2 <- paste0(dir.out, "bbs_route_lines.rds")
    cat("`save.route.lines == TRUE`; saving the BBS routes as linefiles to :\n  ", f2, "\n")
    saveRDS(bbs.grid.lines.df, file = f2)
  }
  
  cat("Writing bbs_spatial to file: ", f, "\n")
  saveRDS(bbs_spatial, file = f)
  return(bbs_spatial)
} ## end function


## side note for jlb: check out this example on identifying distance between points and nearest line
# https://gis.stackexchange.com/questions/269746/find-nearest-line-segment-to-each-point-in-r
# combo.grid <- grid |>,|>  |>    class()grid
