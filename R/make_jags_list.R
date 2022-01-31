#' Munge and Output a List or List of Lists for Use in JAGS
#'
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param scale.vars Logical If TRUE will scale variables. This needs to be improved/checked.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param max.C.ebird if TRUE will drop all checklists where number of observed birds is greater than 100. This is an arbitrary number.
#' @param fn.out Filename of the list object output as a .RDS file
#' @param jagam.args List of arguments used in \code{mgcv::jagam()}. Arguments include c(bs, k, family, sp.prior, m, ...)
#' @param dir.models Location for where to store the GAM portion of the JAGS model
#' @export make_jags_list
make_jags_list <-
  function(dat,
           jagam.args,
           max.C.ebird=100,
           scale.vars = TRUE,
           dir.out,
           fn.out = "jdat") {
    # Force object(s) in dat to a list, despite length
    stopifnot(all(c("bs","k","family","sp.prior","diagonalize") %in% names(jagam.args)))

    if (!is.list(dat)){dat <- list(dat)}

    cat("creating a list of objects for use in JAGS...this will take a few to many minutes\n")
    # Name the list objects
    dat.names<-NA
      for (i in 1:length(dat)) {
        ind <-  names(dat[[i]])
        if ("checklist_id" %in% ind){dat.names[i] <- "ebird"; next()}
        if ("rteno" %in% ind){dat.names[i] <- "bbs"; next()}
        if (!("rteno" %in% ind) &
            !("checklist_id" %in% ind) &
            !any(stringr::str_detect(ind,  "dir."))){
          dat.names[i] <- "grid"
          next()
        }
        if(any(stringr::str_detect(ind,  "dir."))){dat.names[i]  = "dirs"; next()}
      }#end naming for loop
      names(dat) <- dat.names


    # initialize am empty obj to store max values of C for use in jagam data
    maxN <- data.frame()

    # specify all the possible objects to go into a single list of listout
    objs <-
      c(
        "C", # observec counts
        "Xg", # grid cell level covariates (area, proportion route in cell)
        "Xp", # site-level detection covariates
        "nYears",
        "idsYears",
        "nSites",
        "idsSites",
        "idsSitesInd",
        "nGrids",
        "idsGrids",
        "nGridsBySiteByYear",
        "idsGridsbySiteYear",
        "nMaxGrid", # max number of grids a route falls in across all years (indexing scalar)--only avail for BBS data
        "XY", # centroid coords for grid cell
        "area" # area of the grid cell (m^2 unless shapefiles changed..)
      )
# Munge Data --------------------------------------------------------------
    for (i in seq_along(dat)) {
      ind <-
        names(dat)[i] # make lazy indicator for which data we are munging

# EBIRD LOOP --------------------------------------------------------------
    if (ind == "ebird") {
          cat("building ebird objects..\n")
          ebird <- dat[[i]]
          names(ebird) <- tolower(names(ebird))

          if("observation_count" %in% names(ebird)) ebird <- ebird %>% dplyr::rename(c = observation_count)

          if ("sf" %in% class(ebird)){ebird <- ebird %>% sf::st_drop_geometry()}
          ebird <- ebird %>%
            filter(c <= max.C.ebird)  %>%
            # Drop unused variables
            select(-all_species_reported, -group_identifier) %>%
            arrange(gridcellid, checklist_id, year)

          ## Observed counts as 2D matrix (dims: checklist_id by year)
          C   <-
            make_mat(
              ebird %>%
                # since we dont use grid cells here, drop where checklist_id==NA
                distinct(checklist_id, year, c) %>% filter(!is.na(checklist_id)),
              row = "checklist_id",
              col = "year",
              val = "c"
            )
          C   <-
            C %>% select(sort(names(C))) # use select to ensure the colnames(years) are in order...

          nobs      <-
            make_mat(
              ebird %>% distinct(checklist_id, year, number_observers),
              row = "checklist_id",
              col = "year",
              val = "number_observers"
            )

          nmins     <-
            make_mat(
              ebird %>% distinct(checklist_id, year, duration_minutes),
              row = "checklist_id",
              col = "year",
              val = "duration_minutes"
            )
          doy       <-
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

          start_time[start_time=="<NA>"] <- NA
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
          ## remove rownames
          rownames(grid.ebird) <- NULL

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
          rownames(ebird) <- NULL
          temp.ebird <-
            ebird %>% filter(!is.na(c)) # to ensure we remove the NA checklist_id
          idsSites <- sort(unique(temp.ebird$checklist_id))
          idsSitesInd <- seq_along(idsSites)
          idsGrids <- sort(unique(temp.ebird$gridcellid))
          idsYears <- sort(unique(temp.ebird$year))

          nSites  <- length(idsSites)
          nYears  <- length(idsYears)
          nGrids  <- length(idsGrids)

        ### IDENTIFY DESIRED ebird OBJS AS CHARACTER STRING
          ## Grab max values for ebird in each grid cell for use in JAGAM
         maxN <- rbind(ebird %>%
            group_by(gridcellid) %>%
              filter(!is.na(c)) %>%
            summarise(N.max = max(c, na.rm=TRUE)), maxN)

         ## create the list of ebird elements
         objs.in <- objs[objs %in% ls()] %>% as.vector()
         list.out <- vector(mode='list', length=length(objs.in))
         names(list.out) <- objs.in
           for (z in seq_along(objs.in)) {
             new = eval(parse(text = objs.in[z]))# this is necessary for some reason idk why
             list.out[[objs.in[z]]] <- new
           }
           ebird.list <- list.out
           #remove all objects to be sure they arent put into other lists
           suppressWarnings(rm(list=objs))
      }#end ebird i loop

# BBS LOOP --------------------------------------------------------------------
if (ind == "bbs") {
        cat("building bbs objects..\n")
        bbs <- dat[[i]] %>%
          # units::drop_units() %>%
          arrange(gridcellid, rteno, year)
        if ("sf" %in% class(bbs)){bbs <- bbs %>% sf::st_drop_geometry()}
        names(bbs) <- tolower(names(bbs))
        ## Observed counts as 2D matrix (dims: rteno by year)

        C   <-
          make_mat(
            bbs %>% distinct(year, rteno,c) %>%
              filter(!is.na(rteno)),
            row = "rteno",
            col = "year",
            val = "c"
          )
        C   <-
          C %>% select(sort(names(C))) # use select to ensure the colnames(years) are in order...

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
        idsSites <- sort(unique(temp.bbs$rteno))
        idsSitesInd <- seq_along(idsSites)
        idsGrids <- sort(unique(temp.bbs$gridcellid))
        idsYears <- sort(unique(temp.bbs$year))

        nSites <- length(idsSites)
        nYears  <- length(idsYears)
        nGrids  <- length(idsGrids)

        nGridsBySiteByYear <- temp.bbs %>%
          group_by(rteno, year) %>%
          mutate(n=n_distinct(gridcellid)) %>%
          distinct(n, year, rteno) %>%
          reshape2::acast(
            rteno~year, value.var = "n")
        nGridsBySiteByYear[is.na(nGridsBySiteByYear)] <- 0

        #create an array with dimensions [nRoutes by nYears by nMaxNumberGridsASingleRteFallsInto (nMaxGrid)]
        nMaxGrid <-max((temp.bbs %>%
                          distinct(gridcellid, rteno, year) %>%
                          group_by(rteno, year) %>%
                          summarise(n=n_distinct(gridcellid)))["n"]) # index used in JAGS
        idsGridsbySiteYear <- temp.bbs %>%
          distinct(gridcellid, rteno, year) %>%
          group_by(rteno, year) %>%
          mutate(ng = 1:n()) %>%
          arrange(gridcellid) %>%
          reshape2::acast(rteno~year~ng, value.var = "gridcellid")

        ## Grab max values for BBS in each grid cell for use in JAGAM
        maxN <- rbind(bbs %>%
                        group_by(gridcellid) %>%
                        filter(!is.na(c)) %>%
                        summarise(N.max = max(c, na.rm=TRUE)), maxN)

        ## create the list of bbs elements
        objs.in <- objs[objs %in% ls()] %>% as.vector()
        list.out <- vector(mode='list', length=length(objs.in))
        names(list.out) <- objs.in
        for (z in seq_along(objs.in)) {
          new = eval(parse(text = objs.in[z]))# this is necessary for some reason idk why
          list.out[[objs.in[z]]] <- new
        }
        bbs.list <- list.out
        #remove all objects to be sure they arent put into other lists
        suppressWarnings(rm(list=objs))
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

        ## create the list of grid elements
        objs.in <- objs[objs %in% ls()] %>% as.vector()
        list.out <- vector(mode='list', length=length(objs.in))
        names(list.out) <- objs.in
        for (z in seq_along(objs.in)) {
          new = eval(parse(text = objs.in[z]))# this is necessary for some reason idk why
          list.out[[objs.in[z]]] <- new
        }
        grid.list <- list.out
        #remove all objects to be sure they arent put into other lists
      suppressWarnings(rm(list=objs))

    }#end grid loop
}#end i loop for munging `dat`

# Create JAGAM Data ---------------------------------------------------
# grab max values of N across ebird and bbs for each grid cell
maxN <- maxN %>% filter(N.max >= 0) %>%
            full_join(data.frame(gridcellid = grid$gridcellid, N.max = 0)) %>% # ensure all grid cells are represented
            group_by(gridcellid) %>%
            summarise(N.max = max(N.max, na.rm=TRUE)) %>%
            distinct() %>%
            arrange(gridcellid)

if(exists("grid.list")){
jagam.data <- data.frame(
  X=grid.list$XY$X,
  Y=grid.list$XY$Y,
  N=maxN$N.max
  # N.test=dpois(1:length(grid.list$XY$Y), lambda=0.5)
)
# ensure k < num grid cells
stopifnot(jagam.args[['k']] < nrow(jagam.data))

gam.fn <- paste0(dir.out, "/jagam_UNEDITED.jags")
gam.list <-
  mgcv::jagam(
    formula = N ~ s(X, Y,
                    bs=jagam.args[['bs']],
                    k=as.integer(jagam.args[['k']])
                    ),
    file = gam.fn,
    sp.prior = jagam.args[['sp.prior']],
    data = jagam.data,
    diagonalize = jagam.args[['diagonalize']],
    family=tolower(tolower(jagam.args[['family']]))
  )

gam.list$jags.fn = gam.fn

cat("GAM jags model specification saved:\n\t", gam.fn,"\n")
}else{cat("grid data not provided. currently functionality of `make_jags_list()` requires this to be provided. \nfuture functionality will allow option to infer grid information from BBS or eBird data inuputs.","\n")}

# Create Return Object ----------------------------------------------------
objs.out <- c("ebird.list", "bbs.list", "grid.list", "gam.list")
names <- c("ebird", "bbs", "grid", "gam")
for (i in seq_along(objs.out)) {
  if (i == 1) {
    list.out <- list()
    keep = NULL
  }
  if (exists(objs.out[i])) {
    keep <- c(keep, i)
    list.out[[i]] <- get(objs.out[i])
  }
}

# drop empty lists
names(list.out) <- names[keep]
list.out <- list.out[!sapply(list.out, is.null)]

# Add metadata to list
list.out$metadata <- dubcorms:::jdat.contents

# Export to file ----------------------------------------------------------
fn = paste0(paste0(dir.out, "/", fn.out, ".RDS"))
cat("Saving output to file. This may take a minute or two depending on size of eBird data:\n\t", fn)
saveRDS(list.out, file = fn)
cat("\t\t\t\t\t....done saving\n")
# export obj from function
return(list.out)


# END FUN -----------------------------------------------------------------

} # END FUN
