#' Munge and Output a List or List of Lists for Use in JAGS
#'
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param dat.names character string or vector of strings for names of input data objects. If not supplied, the function will guess whether the data is ebird, bbs, or grid.
#' @param scale.vars Logical If TRUE will scale variables. This needs to be improved/checked.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param max.C.ebird if TRUE will drop all checklists where number of observed birds is greater than 100. This is an arbitrary number.
#' @param fn.out Filename of the object output as a .RDS file
#' @export make_jags_list
make_jags_list <-
  function(dat,
           dat.names = NULL,
           max.C.ebird=100,
           scale.vars = TRUE,
           dir.out,
           fn.out = "jdat") {
    # Force object(s) in dat to a list, despite length
    if (!is.list(dat))
      dat <- list(dat)
    if (!is.null(dat.names))
      names(dat) <- tolower(dat.names)

    # Name the list objects
    if (is.null(dat.names)) {
      for (i in 1:length(dat)) {
        ind <-  names(dat[[i]])
        if ("checklist_id" %in% ind)
          dat.names[i] <- "ebird"
        if ("rteno" %in% ind)
          dat.names[i] <- "bbs"

        if (!("rteno" %in% ind) &
            !("checklist_id" %in% ind))
          dat.names[i] <- "grid"
      }#end naming for loop
      names(dat) <- dat.names
    }# end dat.names is null

    # Initialize empty objects in case they aren't filled later we can still recall them without asking ls() or exists("")
    objs.bbs <- objs.grid <- objs.ebird <- NULL


# Munge Data --------------------------------------------------------------
    for (i in seq_along(dat)) {
      ind <-
        names(dat)[i] # make lazy indicator for which data we are munging

# EBIRD LOOP --------------------------------------------------------------
    if (ind == "ebird") {
          cat("building ebird objects..\n")
          ebird <- dat[[i]] %>%
            filter(C <= max.C.ebird)
          if ("sf" %in% class(ebird)){ebird <- ebird %>% sf::st_drop_geometry()}
          names(ebird) <- tolower(names(ebird))
          ebird <- ebird %>%
            # Drop unused variables
            select(-all_species_reported, -group_identifier) %>%
            arrange(gridcellid, checklist_id, year)



          ## Observed counts as 2D matrix (dims: checklist_id by year)
          Ne   <-
            make_mat(
              ebird %>%
                # since we dont use grid cells here, drop where checklist_id==NA
                distinct(checklist_id, year, c) %>% filter(!is.na(checklist_id)),
              row = "checklist_id",
              col = "year",
              val = "c"
            )
          Ne   <-
            Ne %>% select(sort(names(Ne))) # use select to ensure the colnames(years) are in order...


          nobs      <-
            make_mat(
              ebird %>% distinct(checklist_id, year, number_observers),
              row = "checklist_id",
              col = "year",
              val = "number_observers"
            )
          nmins <-
            make_mat(
              ebird %>% distinct(checklist_id, year, duration_minutes),
              row = "checklist_id",
              col = "year",
              val = "duration_minutes"
            )
          doy <-
            make_mat(
              ebird %>% distinct(checklist_id, year, yday),
              row = "checklist_id",
              col = "year",
              val = "yday"
            )
          effort_ha <-
            make_mat(
              ebird %>% distinct(checklist_id, year, effort_area_ha),
              row = "checklist_id",
              col = "year",
              val = "effort_area_ha"
            )
          start_time <-
            make_mat(
              ebird %>% distinct(checklist_id, year, time_observations_started),
              row = "checklist_id",
              col = "year",
              val = "time_observations_started"
            )
          obs_id <-
            make_mat(
              ebird %>% distinct(checklist_id, year, observer_id),
              row = "checklist_id",
              col = "year",
              val = "observer_id"
            )

          # Package all detection covariates
          Xp <- list(nobs=nobs,
                     nmins=nmins,
                     doy=doy, # day of year
                     effort_ha=effort_ha,
                     obs_id = obs_id

          )
          for(j in seq_along(Xp)){
            Xp[[j]][Xp[[j]] == "NULL"] <- NA}


          # Grid-level covariates(2D, since grid/route locations don't change over time.)
          grid.ebird <-
            ebird %>%
            distinct(gridcellid, checklist_id, .keep_all = TRUE)  %>%
            units::drop_units()
          ## scale area if necessary
          if (scale.vars){grid.ebird$area <- scale(grid.ebird$area)}

          ## grab area
          Xg.area <-
            make_mat(grid.ebird, val = "area", replace.na = TRUE, row="checklist_id")
          Xg.area[is.na(Xg.area)] <- 0

          Xg.XY <- grid.ebird %>%
            distinct(gridcellid, cell.lon.centroid, cell.lat.centroid) %>%
            arrange(gridcellid) %>%
            tibble::column_to_rownames("gridcellid")
          names(Xg.XY) <- c("X","Y")

          Xg <- list(area=Xg.area, XY=Xg.XY)

          # Loop indexes for JAGS
          temp.ebird <-
            ebird %>% filter(!is.na(c)) # to ensure we remove the NA checklist_id
          idsChecklistsE <- sort(unique(temp.ebird$checklist_id))
          idsGridsE <- sort(unique(temp.ebird$gridcellid))
          idsYearsE <- sort(unique(temp.ebird$year))

          nCheckListsE <- length(idsChecklists)
          nYearsE  <- length(idsYears)
          nGridsE  <- length(idsGrids)


          ### IDENTIFY DESIRED ebird OBJS AS CHARACTER STRING
          objs <-
            c(
              "Xg", # grid cell level covariates (area, proportion route in cell)
              ## observed counts
              "nCheckListsE",
              "nYearsE",
              "nGridsE",
              "Xp",
              "Ne",
              "nGridsByRouteYear", # numb opf grids per rte/year
              "nMaxGrid", # max number of grids a route falls in across all years (indexing scalar)
              "idsChecklists",
              "idsGridsE",
              "idsYearsE",
              "idsGridsByRouteYear" # array [nroutes by nyear by nmaxgrid] values == grid cell ids
            )

          ebird.list <- list()
          keep<-names<-NULL
          for (j in seq_along(objs)) {
            if (j == 1) {
              ebird.list <- list()
              keep = NULL
            }
            if (exists(objs[j])) {
              keep <- c(keep, j)
              ebird.list[[j]] <- get(objs[j])
              names <- c(names, objs[j])
            }
          } # end j grid loop
          names(ebird.list) <- names
          rm(names, keep)

          rm(i,j)
        }#end ebird i loop

# BBS LOOP --------------------------------------------------------------------
if (ind == "bbs") {
        bbs <- dat[[i]] %>%
          # units::drop_units() %>%
          arrange(gridcellid, rteno, year)
        if ("sf" %in% class(bbs)){bbs <- bbs %>% sf::st_drop_geometry()}
        names(bbs) <- tolower(names(bbs))
        cat("building bbs objects..\n")

        ## Observed counts as 2D matrix (dims: rteno by year)
        Nb   <-
          make_mat(
            bbs %>%
              # since we dont use grid cells here, drop where rteno==NA
              distinct(rteno, year, c) %>% filter(!is.na(rteno)),
            row = "rteno",
            col = "year",
            val = "c"
          )
        Nb   <-
          Nb %>% select(sort(names(Nb))) # use select to ensure the colnames(years) are in order...

        # Calculate mean values for detection covariates (some were already done elsewhere -- need to add these to that location at some point)
        bbs <- bbs %>%
          group_by(rteno, year) %>%
          mutate(windmean = abs(startwind - endwind) / 2) %>%
          mutate(skymean = abs(startsky - endsky) / 2)
        # Scale detection covariates if specified
        if (scale.vars) {
          bbs <- bbs %>%
            ### create a variable for the wind "average"
            group_by(rteno, year) %>%
            mutate(windmean = abs(startwind - endwind) / 2) %>%
            ungroup() %>%
            ### z-scale covariates
            mutate(
              windmean  = (windmean - mean(windmean, na.rm = TRUE)) / sd(windmean, na.rm =
                                                                           TRUE),
              noisemean = (noisemean - mean(noisemean, na.rm = TRUE)) /
                sd(noisemean, na.rm = TRUE),
              # skymean   = (skymean - mean(skymean, na.rm=TRUE))/sd(skymean, na.rm=TRUE),
              carmean   = (carmean - mean(carmean, na.rm = TRUE)) / sd(carmean, na.rm =
                                                                         TRUE)
            )
        }#end scale.vars bbs

        wind      <-
          make_mat(
            bbs %>% distinct(rteno, year, windmean),
            row = "rteno",
            col = "year",
            val = "windmean"
          )
        car       <-
          make_mat(
            bbs %>% distinct(rteno, year, carmean),
            row = "rteno",
            col = "year",
            val = "carmean"
          )
        noise     <-
          make_mat(
            bbs %>% distinct(rteno, year, noisemean),
            row = "rteno",
            col = "year",
            val = "noisemean"
          )
        fyrbbs    <-
          make_mat(
            bbs %>% distinct(rteno, year, obsfirstyearbbs),
            row = "rteno",
            col = "year",
            val = "obsfirstyearbbs"
          )
        fyrroute  <-
          make_mat(
            bbs %>% distinct(rteno, year, obsfirstyearroute),
            row = "rteno",
            col = "year",
            val = "obsfirstyearroute"
          )
        assistant <-
          make_mat(
            bbs %>% distinct(rteno, year, assistant),
            row = "rteno",
            col = "year",
            val = "assistant"
          )
        # Package all detection covariates
        Xp <- list(assistant=assistant,
                   fyrroute=fyrroute,
                   fyrbbs = fyrbbs,
                   noise=noise,
                   car=car,
                   wind=wind
        )
        for(j in seq_along(Xp)){
          Xp[[j]][Xp[[j]] == "NULL"] <- NA}


        # Grid-level covariates(2D, since grid/route locations don't change over time.)
        grid.bbs <-
          bbs %>%
          distinct(gridcellid, rteno, .keep_all = TRUE)  %>%
          units::drop_units()
        ## scale area if necessary
        if (scale.vars){grid.bbs$area <- scale(grid.bbs$area)}

        ## grab area
        Xg.area <-
          make_mat(grid.bbs, val = "area", replace.na = TRUE)
        Xg.area[is.na(Xg.area)] <- 0

        Xg.XY <- grid.bbs %>%
          distinct(gridcellid, cell.lon.centroid, cell.lat.centroid) %>%
          arrange(gridcellid) %>%
          tibble::column_to_rownames("gridcellid")
        names(Xg.XY) <- c("X","Y")

        ## grab the  ortion of rteno in cell
        Xg.prop <-
          make_mat(grid.bbs, val = "proprouteincell", replace.na = TRUE)
        Xg.prop[is.na(Xg.prop)] <-
          0 # these must be zero for jags model

        Xg <- list(prop=Xg.prop, area=Xg.area, XY=Xg.XY)

        # Loop indexes for JAGS
        temp.bbs <-
          bbs %>% filter(!is.na(c)) # to ensure we remove the NA rteno
        idsRoutesB <- sort(unique(temp.bbs$rteno))
        idsGridsB <- sort(unique(temp.bbs$gridcellid))
        idsYearsB <- sort(unique(temp.bbs$year))

        nRoutesB <- length(idsRoutes)
        nYearsB  <- length(idsYears)
        nGridsB  <- length(idsGrids)


        nGridsByRouteYear <- temp.bbs %>%
          group_by(rteno, year) %>%
          mutate(n=n_distinct(gridcellid)) %>%
          distinct(n, year, rteno) %>%
          reshape2::acast(
            rteno~year, value.var = "n")
        nGridsByRouteYear[is.na(nGridsByRouteYear)] <- 0



        #create an array with dimensions [nRoutes by nYears by nMaxNumberGridsASingleRteFallsInto (nMaxGrid)]
        nMaxGrid <-max((temp.bbs %>%
                          distinct(gridcellid, rteno, year) %>%
                          group_by(rteno, year) %>%
                          summarise(n=n_distinct(gridcellid)))["n"]) # index used in JAGS
        idsGridsByRouteYear <- temp.bbs %>%
          distinct(gridcellid, rteno, year) %>%
          group_by(rteno, year) %>%
          mutate(ng = 1:n()) %>%
          arrange(gridcellid) %>%
          acast(rteno~year~ng, value.var = "gridcellid")

        ### IDENTIFY DESIRED BBS OBJS AS CHARACTER STRING
        objs <-
          c(
            "Xg", # grid cell level covariates (area, proportion route in cell)
            "Xp",
            ## observed counts
            "Nb",
            "nRoutesB",
            "nYearsB",
            "nGridsB",
            "idsGridsB",
            "idsRoutesB",
            "idsYearsB",
            ##
            "nRoutesByGridYear",
            "nRoutesByYear",
            ##
            "nGridsByRouteYear", # numb opf grids per rte/year
            "idsGridsByRouteYear", # array [nroutes by nyear by nmaxgrid] values == grid cell ids
            "nMaxGrid" # max number of grids a route falls in across all years (indexing scalar)
          )

        bbs.list <- list()
        keep<-names<-NULL
        for (j in seq_along(objs)) {
          if (j == 1) {
            bbs.list <- list()
            keep = NULL
          }
          if (exists(objs[j])) {
            keep <- c(keep, j)
            bbs.list[[j]] <- get(objs[j])
            names <- c(names, objs[j])
          }
        } # end j grid loop
        names(bbs.list) <- names
        rm(names, keep)



      }#end bbs loop


# GRID LOOP ---------------------------------------------------------------
      if (ind == "grid") {
        grid <- dat[[i]] %>%
          # units::drop_units() %>%
          arrange(gridcellid) %>%
          distinct(gridcellid, .keep_all = TRUE)

        if ("sf" %in% class(grid))
          grid <- grid %>% sf::st_drop_geometry()  %>%
            units::drop_units()
        cat("building grid objects..\n")


        names(grid) <- tolower(names(grid))
        nGrids <- length(unique(grid$gridcellid))
        XY <- data.frame(X=grid$cell.lon.centroid, Y=grid$cell.lat.centroid)
        area <- grid$area


        objs <- c("nGrids", "XY", "area")
        grid.list <- list()
        keep<-names<-NULL
        for (j in seq_along(objs)) {
          if (j == 1) {
            grid.list <- list()
            keep = NULL
          }
          if (exists(objs[j])) {
            keep <- c(keep, j)
            grid.list[[j]] <- get(objs[j])
            names <- c(names, objs[j])
          }
        } # end j grid loop
        names(grid.list) <- names
        rm(names, keep)
      }#end grid loop


    }#end i loop for munging `dat`


# Create Return Object ----------------------------------------------------
objs <- c("ebird.list", "bbs.list", "grid.list")
names <- c("ebird", "bbs", "grid")
for (i in seq_along(objs)) {
  if (i == 1) {
    list.out <- list()
    keep = NULL
  }
  if (exists(objs[i])) {
    keep <- c(keep, i)
    list.out[[i]] <- get(objs[i])
  }
}

# drop empty lists
names(list.out) <- names[keep]
list.out <- list.out[!sapply(list.out, is.null)]


# Export to file ----------------------------------------------------------
    fn = paste0(paste0(dir.out, "/", fn.out, ".RDS"))
    cat("Saving output to file: ", fn)
    saveRDS(list.out, file = fn)

    # export obj from function
    return(list.out)

  } # END FUN
