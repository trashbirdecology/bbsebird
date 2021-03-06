#' @title Bundle Data for Use in JAGS
#' @param bbs BBS data table
#' @param ebird eBird data table
#' @param grid spatial sampling grid/study area table
#' @param drop.na.cov.obs logical if TRUE will remove ALL data where any specified covariate does not exist. If TRUE, suggest examining the data and covariates prior to specifying site.covs and grid.covs.
#' @param scale.covs logical if TRUE will automatically scale the numeric/integer covariates.
#' @param X variable name associated with the x-coordinate (e.g., long, longitude, Easting, X) across 'grid', 'bbs', and 'ebird'
#' @param Y variable name associated with the x-coordinate (e.g., latitude, Northing, Y) across 'grid', 'bbs', and 'ebird'
#' @param cellid column name(s) of the grid cell identifier
#' @param siteid column name(s) of the site  identifier (e.g., BBS route, eBird checklists)
#' @param yearid column name of the temporal identifier
#' @param dir.outputs directory location of where to save the JAGAM.bugs model file created by mgcv::jagam(). Defaults to ./models/
#' @param obsid  column name(s) of the observer identifier
#' @param drop.empty.cells IN DEVELOPMENT NOT RELIABLE FOR MODELING PURPOSES!!! logical Default FALSE If TRUE will remove all grid cells that have no count data (neither eBird nor bbs).
#' @param cell.covs column name(s) of the grid-level covariates
#' @param EN.arg if "max" will use the maximum value of observed birds at each grid cell to produce matrix of expected number of birds at the grid cell level. Alternatives include "min", "mean".
#' @param use.ebird.in.EN logical if TRUE will use data across both eBird and BBS observations to create basis functions.
#' @param site.covs column name(s) of the site-level covariates
#' @param max.ebird integer maximum number of eBird checklists within a single grid cell and year to keep in the data. Does not apply to dev.mode. If an integer is not provided (e.g., NULL, FALSE), all checklists will be returned in the resulting data object.
#' @param mins.to.hours logical if TRUE will convert covariates on the minute scale to the hour scale.
#' @param dev.mode logical if TRUE will return a reduced data set to use in development/debugging purposes. This method reduces the number of time units to 2, the maximum number of grid cells to 10, and 2 sites from each data source
#' @param save.neighborhood logical if TRUE will save the neighborhood network to file as "neighborhood.rds" at 'dir.outputs'.
#' @param fill.cov.nas value with with to fill missing covariate values. User can specify value as FALSE if no fill is requested.
#' @importFrom dplyr group_by mutate select distinct arrange filter slice
#' @importFrom spdep poly2nb nb2WB
#' @export make_bundle

make_bundle <- function(bbs,
                        ebird,
                        grid,
                        drop.na.cov.obs = TRUE,
                        mins.to.hours = TRUE,
                        scale.covs  = TRUE,
                        fill.cov.nas = NA,
                        max.ebird = 25,
                        use.ebird.in.EN = TRUE,
                        EN.arg    = "max",
                        X           = "cell.lon.centroid",
                        Y           = "cell.lat.centroid",
                        cellid     = "gridcellid",
                        yearid     = "year",
                        siteid     = c("checklist_id", "rteno", "sampling_event_identifier"),
                        obsid      = c("obsn", "observer_id"),
                        cell.covs   = c("area"),
                        drop.empty.cells = FALSE,
                        site.covs   = c(
                          "starttime",
                          "endtime",
                          "wind",
                          "noise",
                          "cars",
                          "minute",
                          "assistant",
                          "obsfirstyearbbs",
                          "obsfirstyearonbbs",
                          "obsfirstyearroute",
                          "obsfirstyearonroute",
                          "time_observations_started_hsm",
                          "time_observations_started",
                          "duration_minutes",
                          # "effort_distance_km",
                          # "effort_area_ha",
                          "protocol_type",
                          # "protocol_code",
                          "number_observers"
                        ),
                        dev.mode    = FALSE,
                        dir.outputs = "/outputs/",
                        save.neighborhood = FALSE) {

dir.create(dir.outputs, showWarnings=FALSE, recursive=TRUE)

# ### for dEV convenience
#   drop.na.cov.obs = TRUE
#   mins.to.hours = TRUE
#   scale.covs  = TRUE
#   fill.cov.nas = NA
#   max.ebird = 25
#   use.ebird.in.EN = TRUE
#   EN.arg    = "max"
#   X           = "cell.lon.centroid"
#   Y           = "cell.lat.centroid"
#   cellid     = "gridcellid"
#   yearid     = "year"
#   siteid     = c("checklist_id", "rteno", "sampling_event_identifier")
#   obsid      = c("obsn", "observer_id")
#   cell.covs   = c("area")
# site.covs   = c(
#   "starttime",
#   "endtime",
#   "wind",
#   "noise",
#   "cars",
#   "minute",
#   "assistant",
#   "obsfirstyearbbs",
#   "obsfirstyearonbbs",
#   "obsfirstyearroute",
#   "obsfirstyearonroute",
#   "time_observations_started_hsm",
#   "time_observations_started",
#   "duration_minutes",
#   # "effort_distance_km",
#   # "effort_area_ha",
#   "protocol_type",
#   # "protocol_code",
#   "number_observers"
# )
# dev.mode    = FALSE
# dir.outputs = "/outputs/"
# save.neighborhood = FALSE

# EVALUATE ARGS -----------------------------------------------------------
  ## first, test and evaluate args as necessary.
  EN.arg <- tolower(EN.arg)
  stopifnot(EN.arg %in% c("mean", "max", "min"))
  stopifnot(length(fill.cov.nas) == 1)
  stopifnot(is.logical(use.ebird.in.EN))
  stopifnot(is.logical(scale.covs))
  stopifnot(is.logical(drop.na.cov.obs))

  # Munge Data Frames a Little Prior to Data Munging ---------------------------------------------------
  bbs   <- as.data.frame(bbs)
  ebird <- as.data.frame(ebird)
  ## force colnames to lower
  names(bbs)   <- tolower(names(bbs))
  names(ebird) <- tolower(names(ebird))
  names(grid)  <- tolower(names(grid))


  if("grid.study.area." %in% names(bbs)) bbs <- bbs |> dplyr::select(-"grid.study.area.")
  if("grid.study.area" %in% names(ebird)) ebird <- ebird |> dplyr::select(-"grid.study.area")
  if("grid_study_area_" %in% names(bbs)) bbs <- bbs |> dplyr::select(-"grid_study_area_")
  if("grid_study_area_" %in% names(ebird)) ebird <- ebird |> dplyr::select(-"grid_study_area_")
  if("geometry" %in% names(bbs)) bbs <- bbs |> dplyr::select(-"geometry")
  if("geometry" %in% names(ebird)) ebird <- ebird |> dplyr::select(-"geometry")

  # Rename Data Table Colnames ----------------------------------------------
  L <- list(bbs = bbs,
            ebird = ebird,
            grid = grid)
  for (i in seq_along(L)) {
    n <- ns <- nt <- nc <- no <- nx <- ny <- NULL
    ### find overlap in things i want to rename
    ## this can be improved yes but not rn lol
    n  <- names(L[[i]])
    if (any(yearid %in% n))
      nt <- data.frame(old = n[n %in% c(yearid)], new = "yearid")
    if (any(obsid %in% n))
      no <- data.frame(old = n[n %in% c(obsid)], new = "obsid")
    if (any(siteid %in% n))
      ns <- data.frame(old = n[n %in% c(siteid)], new = "siteid")
    if (any(cellid %in% n))
      nc <- data.frame(old = n[n %in% c(cellid)], new = "cellid")
    if (any(X %in% n))
      nx <- data.frame(old = n[n %in% c(X)], new = "X")
    if (any(Y %in% n))
      ny <- data.frame(old = n[n %in% c(Y)], new = "Y")

    nn <- rbind(nt, ns, nc, nx, ny, no)
    if (is.null(nn))
      next()
    # if(!any(n %in% nn$old))next()
    for (j in 1:nrow(nn)) {
      colind <- which(names(L[[i]]) == nn$old[j])
      names(L[[i]])[colind] <- nn$new[j]
    }
  }#end L loop

  bbs   <- L$bbs
  ebird <- L$ebird
  grid  <- L$grid
  rm(L)

# Ensure no duplicates exist ----------------------------------------------
  bbs   <-
    bbs |> dplyr::distinct(yearid, siteid, cellid, c, .keep_all = TRUE)
  ## (duplicates in ebird, though it might be because we have multiple observers. huge amount of excess data being imported...)
  ebird <-
    ebird |> dplyr::distinct(yearid, siteid, cellid, c, .keep_all = TRUE)


# Subset for dev.mode=TRUE ------------------------------------------------
  if (dev.mode) {
    message(
      paste0(
        "[notice] `dev.mode` is TRUE. Output object will be a small subset of the original data.\n"
      )
    )
    # keep 3 years data max
    maxyr <- max(unique(bbs$yearid), na.rm = TRUE)
    totalyrs <- maxyr - min(unique(bbs$yearid), na.rm = TRUE)
    T.keep <- (maxyr - min(5, totalyrs)):maxyr
    ## keep max 30 grid cells,
    ## sample 5 and grab potentially adjacent neighbors.
    G.keep <-
      sample(sort(unique(grid$cellid)) - 1, min(0.5*nrow(grid), 30)) # but dont grab the last one
    G.keep  <- c(G.keep + 1, G.keep)
    grid <- grid[grid$cellid %in% G.keep, ]

    bbs  <-   bbs |> filter(cellid %in% G.keep &
                              yearid %in% T.keep)
    ebird  <- ebird |> filter(cellid %in% G.keep &
                                yearid %in% T.keep)
    ## keep random sample of 10 routes or checklists per grid cell
    ### this will likely not reduce bbs by much (possible zero reduction) but will reduce ebird
    #### slice_max orders by site and takes 'top' n
    b.samp.keep <- bbs |>
      dplyr::distinct(yearid, cellid, siteid, .keep_all = TRUE) |>
      dplyr::group_by(yearid, cellid) |>
      dplyr::mutate(maxn = n_distinct(siteid))
    bbs <- b.samp.keep |>
      dplyr::slice_sample(n = min(b.samp.keep$maxn, 5)) |>
      dplyr::ungroup()
    e.samp.keep <- ebird |>
      dplyr::distinct(yearid, cellid, siteid, .keep_all = TRUE) |>
      dplyr::group_by(yearid, cellid) |>
      dplyr::mutate(maxn = n_distinct(siteid))
    ebird <- e.samp.keep |>
      dplyr::slice_sample(n = min(e.samp.keep$maxn, 5)) |>
      ungroup()

    rm(T.keep, G.keep, e.samp.keep, b.samp.keep)
  }


# KEEP N CHECKLISTS IF SPECIFIED ------------------------------------------
if (!dev.mode & (is.numeric(max.ebird)|is.integer(max.ebird))){
  cat(
    "`dev.mode` is FALSE & max.ebird is",
    max.ebird,
    ". Keeping a maximum of",
    max.ebird,
    "checklists per grid cell per year. If you wish to keep all, specify `max.ebird = NULL`" ,
    "\n"
  )
  temp.ebird <- ebird |>
    dplyr::distinct(yearid, cellid, siteid, .keep_all = TRUE) |>
    dplyr::group_by(yearid, cellid) |>
    dplyr::mutate(maxn = n_distinct(siteid)) |>
    dplyr::ungroup()
  temp.ebird <- temp.ebird |>
    dplyr::group_by(yearid, cellid) |>
    dplyr::mutate(nkeep = min(maxn, max.ebird))    |>
    dplyr::ungroup()
  # unique(temp.ebird$nkeep)
  ### randomly shuffle all the rows to improve chances of randomly selecting rows to keep
  temp.ebird <- temp.ebird[sample(1:nrow(temp.ebird)), ]

  ### next, assign row numbers within each group then drop any below N
  ebird <- temp.ebird |>
    dplyr::group_by(yearid, cellid) |>
    dplyr::mutate(rownum = row_number()) |>
    dplyr::filter(rownum <= max.ebird) |>
    dplyr::ungroup() |>
    dplyr::select(-rownum, -nkeep, -rownum)
  # test=temp.ebird |>
  #   dplyr::group_by(yearid, cellid) |>
  # mutate(n=n_distinct(siteid))
  # hist(test$n)
  #
}

# DROP COLS WHERE ALL ROWS == NA ------------------------------------------
  ### some covariates may have all NA. prior to subsetting, let's remove those columns.
  all_na <- function(x) {
    any(!is.na(x))
  }
  LLL <- list(ebird = ebird, bbs = bbs)
  newlist <- list(ebird = NULL, bbs = NULL)
  for (i in seq_along(LLL)) {
    newlist[[i]] <- LLL[[i]] |> select_if(all_na)
    diff <- setdiff(colnames(LLL[[i]]), colnames(newlist[[i]]))
    if (length(diff) > 0)
      message(
        "[notice] the following columns in the ",
        names(LLL)[i],
        " data had all NA values and were removed:\n\t",
        paste(diff, collapse = ", "),
        "."
      )
    rm(diff)
  }
  bbs   <- newlist$bbs
  ebird <- newlist$ebird
  rm(all_na, LLL, newlist)

# DROP DATA WITH MISSING COVS IF drop.na.cov.obs TRUE -------------------------------------
  if (drop.na.cov.obs) {
    # for ebird and for bbs...
    # cols.to.index <- colnames(bbs)[colnames(bbs) %in% c("c",site.covs)]
    bbs   <-
      bbs[complete.cases(bbs[, c(colnames(bbs)[colnames(bbs) %in% c("c", site.covs)])]), ]
    ebird <-
      ebird[complete.cases(ebird[, c(colnames(ebird)[colnames(ebird) %in% c("c", site.covs)])]), ]

    if (nrow(bbs) == 0)
      stop(
        "after removing all NA observations without associated covariate information, no BBS data exists. \nConsider removing some site.covs from data subsetting/model and/or specifying dev.mode=FALSE..\n"
      )
    if (nrow(ebird) == 0)
      stop(
        "after removing all NA observations without associated covariate information, no eBird data exists. \nConsider removing some site.covs from data subsetting/model and/or specifying dev.mode=FALSE..\n"
      )
  }


# MAKE INDEXES ------------------------------------------------------------
  ## Global Indexes-----------------------------------------------------------------
  cat("  [note] creating global indexes\n")
  ## STUDY AREA GRID CELL INDEX
  cellindex <- grid |>
    units::drop_units() |>
    arrange(cellid) |>
    distinct(cellid, X, Y, .keep_all = TRUE) |>
    # create grid cell id as index and not identity (though they may be the)
    mutate(cellind = 1:n())
  ### ie drop covs not specified in grid.covs
  cellindex <-
    cellindex[names(cellindex) %in% c("cellid", "cellind", "X", "Y", cell.covs)]
  ## scale if specified
  if (scale.covs) {
    cellindex <- cellindex |>
      mutate(area = standardize(area)) |>
      mutate(X  = standardize(X)) |>
      mutate(Y  = standardize(Y))
  }
  ## YEAR INDEX
  yearindex <-
    data.frame(yearid = min(c(ebird$yearid, bbs$yearid), na.rm = TRUE):max(c(ebird$yearid, bbs$yearid), na.rm =
                                                                                TRUE)) |> mutate(yearind = 1:n())
  # ### YEAR-GRID INDEX
  # # make a version of all years and grid index combinations, regardless data availability.
  # # this is used here, internally, but can also be called in certain jags model components
  yg.index <-
    expand.grid(yearind = yearindex$yearind,
                cellind = cellindex$cellind) |>
    merge(yearindex) |>
    merge(cellindex) |>
    dplyr::select(cellind, cellid, yearind, yearid)

  ## BBS-EBIRD INDEXES ---------------------------------------------------------------
  ## for bbs and ebird data, create site (route, checklist_id) and observer indexes
  LL <- list(bbs = bbs, ebird = ebird)
  cat("  [note] creating indexes for sampling events and sites\n")
  for (i in seq_along(LL)) {
    ## first, drop grid-level covariates and metadata since it iwll be stored there
    LL[[i]] <-
      LL[[i]][!names(LL[[i]]) %in% c("X", "Y", cell.covs)]
    ## second, drop NA count values, since we don't want to include non-sampled site-year combinations
    LL[[i]] <- LL[[i]] |>
      units::drop_units() |>
      filter(!is.na(siteid), !is.na(c))
    ## create index each for SITE and OBSERVER, then append to the data frame
    LL[[i]] <-
      LL[[i]] |>
      mutate(obsind  = obsid  |> as.factor() |> as.integer()) |>
      mutate(siteind = siteid |> as.factor() |> as.integer())
    ## just to be safe, ensure all join by vars are same type
    LL[[i]] <- LL[[i]] |>
      mutate(yearid = as.integer(yearid),
             cellid = as.integer(cellid))

    ## this is a shitty workaround, but currently (other than using data.table)
    ## is the only tractable way I know for dealing with the large eBird data tables...
    ## using a direct left join or a merge is not possible, even on my 60+GB RAM machine..
    ## very happy to receive changes
    #### extract the columsn on which to join

    LL.sub <- LL[[i]] |> dplyr::select(cellid, yearid)
    LL.sub <-
      LL.sub |> left_join(yg.index, by = c("cellid", "yearid"))
    stopifnot(nrow(LL.sub) == nrow(LL[[i]]))
    ### remove from the LL.sub extracted columns from data and then append new (four ) columns
    LL[[i]] <- LL[[i]] |> dplyr::select(-cellid, -yearid)

    LL[[i]] <- LL[[i]] |>  dplyr::bind_cols(LL.sub)

    stopifnot(!any(is.na(LL[[i]]$cellind)))
    stopifnot(!any(is.na(LL[[i]]$yearind)))

    ## finally, drop the NA observations on bbs and ebird..
    LL[[i]] <-     LL[[i]][!is.na(LL[[i]]$siteind),]

  } # end LL loop
  ##extract from list
  bbs   <- LL$bbs
  ebird <- LL$ebird
  rm(LL)

# PROP MATRICES (% siteind in cellind) ----------------------------------------------------
  ### all values for prope will be either 0, 1, since a checklist always falls only within one cell..
  ### but we need the prope as a conveneicne for matrix multiplication in models.
  ### values for prop (for bbs ) will be between 0 and 1.
  LL <- list(bbs = bbs, ebird = ebird)
  prop <- list()
  for(i in seq_along(LL)){
    if(names(LL)[i]=="ebird") LL[[i]]$proprouteincell <- 1
    stopifnot(nrow(LL[[i]] |> distinct(siteind, cellind, proprouteincell)) ==
                nrow(LL[[i]] |> distinct(siteind, cellind)))
    prop[[i]] <- LL[[i]] |>
      distinct(siteind, cellind, proprouteincell) |>
      full_join(cellindex |> dplyr::select(cellind), by = "cellind") |>
      dplyr::select(proprouteincell, siteind, cellind) # remove geometry
    prop[[i]]$proprouteincell[is.na(prop[[i]]$proprouteincell)] <-
      0  #


  # cat("  [note] creating prop matrix for", names(LL[i])  , "sites and grid cells\n")
  # stopifnot(nrow(bbs |> distinct(siteind, cellind, proprouteincell)) ==
  #             nrow(bbs |> distinct(siteind, cellind)))
  # ## grab all cell ids and site inds
  # prop <- bbs |>
  #   distinct(siteind, cellind, proprouteincell) |>
  #   full_join(cellindex |> dplyr::select(cellind), by = "cellind") |>
  #   dplyr::select(proprouteincell, siteind, cellind) # remove geometry
  # prop$proprouteincell[is.na(prop$proprouteincell)] <-
  #   0  #
  # ## if in development mode, impute values such that rowsums prop == 1;
  # # browser()
  # if (dev.mode) {
  #   warning(
  #     "  [importante] dev.mode == TRUE. Output 'prop' will have made-up values to ensure all(rowsums(output$prop) == 1)!!\n "
  #   )
  #   ## A very messy workaround but tis for the dev version so whatever...
  #   ## make a table of the site indexes and the total missing proportion we need to impute
  #   toadd <- prop |> dplyr::group_by(siteind, cellind) |>
  #     summarise(propsum = sum(proprouteincell , na.rm = TRUE)) |>
  #     dplyr::group_by(siteind) |>
  #     dplyr::summarise(totalsum = sum(propsum , na.rm = TRUE)) |>
  #     dplyr::filter(totalsum < 1.0) |>
  #     dplyr::ungroup() |>
  #     dplyr::mutate(proprouteincell = 1 - totalsum) |>
  #     dplyr::select(-totalsum)
  #   # toadd == list of siteindex with associated MISSING proportions
  #
  #   ## choose the grid cell to add the toadd prop to as min
  #   sitegridsaddtome <- prop |>
  #     select(siteind, cellind) |>
  #     filter(siteind %in% toadd$siteind) |>
  #     group_by(siteind) |>
  #     slice_sample(n = 1)  |>
  #     full_join(toadd)
  #   prop <- dplyr::bind_rows(sitegridsaddtome, prop) |>
  #     group_by(siteind) |>
  #     mutate(proprouteincell = sum(proprouteincell, na.rm = TRUE)) |>
  #     distinct(siteind, cellind, proprouteincell)
  #   rm(sitegridsaddtome, toadd)
  #   # prop.impute <-
  #   #   reshape2::acast(prop.impute,
  #   #                   siteind ~ cellind,
  #   #                   # formula =
  #   #                   value.var = "proprouteincell",
  #   #                   fill = 0)
  #   # stopifnot(max(prop.impute)==1)
  # }
  ## AS grid sizes get smaller, the probabilty of routes being cut off from the regions is likely.
  prop[[i]] <-
    reshape2::acast(prop[[i]],
                    siteind ~ cellind,
                    value.var = "proprouteincell",
                    fill = 0)

  ### this is hacky and really needs to be fixed before production.
  ### something must be up with the BBS data spatial because should equal one automatically.
  ## need to check make_bbs_spatial soon
  ### but actually this is likely correct and will just need to improve documenattion to eflect that
  ### proportion of route that falls within the study area grids may not equal absolute length of route IRL
  for(k in 1:nrow(prop[[i]])){
    prop[[i]][k,]<- prop[[i]][k,]/sum(prop[[i]][k,])
  }

  # remove the rownames==NA (the last row usually..)
  if (any(rownames(prop[[i]]) %in% c("NA", NA))) {
    prop[[i]] <-
      prop[[i]][-which(rownames(prop[[i]]) %in% c(NA, "NA")), ]
  }
}# end  LL for PROP CREATION
  names(prop) <- names(LL)
  names(prop)
  ##extract from list
  bbs   <- LL$bbs
  ebird <- LL$ebird
  rm(LL)


# SITE-LEVEL COVARS -------------------------------------------------------
  ## create arrays for covariates
  ### dimensions are n.sites by n.years
  LL    <- list(bbs = bbs,  ebird = ebird) # data
  Xsite <-
    list(bbs = NULL, ebird = NULL) # empty list for storing site-cov matrices for bbs and ebird data
  cat("  [note] creating site-level covariate matrices....\n")
  for (i in seq_along(LL)) {
    temp <- site.covs[site.covs %in% names(LL[[i]])]
    if (length(temp) == 0)
      next()
    # remove the extra data (for routes w/>1 grid cell)
    LL[[i]] <-
      LL[[i]] |> dplyr::distinct(siteind, yearind, .keep_all = TRUE)
    for (j in seq_along(temp)) {
      cov.name  <- temp[j]

      cov.dat   <- data.frame(as.vector(LL[[i]][cov.name]),
                              LL[[i]]["siteind"],
                              LL[[i]]["yearind"])
      ## scale the covariate if scale.covs==TRUE'###
      #### NEED TO ADD OPTION TO OUTPUT COV>DAT IN LONG FORM (PERHAPS JUST APPEND AS SCALED VERSIONS TO EBIRd/BBS.DF..)
      names(cov.dat)[1] <- "cov"
      if (all(is.na(cov.dat$cov))) {
        next()
      }
      is.time   <- ## index for POSIX/LUBRIDATE TIMES
        class(cov.dat$cov) %in% c("period", "time", "Period")
      is.binary <-  if (is.time) {
        FALSE
      } else{
        !is.character(cov.dat$cov) && !(max(cov.dat$cov, na.rm = TRUE) > 1)
      }
      if (cov.name %in% c("endtime", "starttime")) {
        # browser()
        cov.dat$cov <- as.integer(cov.dat$cov)
      }

      is.factor <-
        ifelse(is.factor(cov.dat$cov),
               TRUE,
               FALSE)
      # if (is.time)
      #   cov.dat$cov <- cov.dat$cov@hour * 60 + cov.dat$cov@minute
      # if (is.time &
      #     grepl("minute|time_observations_started", cov.name)) {
      #   message("[notice] converting covariate ",
      #           cov.name ,
      #           " from minutes to hours")
      #   cov.dat$cov <- cov.dat$cov / 60
      # }
      if (scale.covs &
          is.binary)
        cat("  [note] site covariate ",
            cov.name,
            " is binary and was not standardized.\n",
            sep = "")
      if (scale.covs &
          is.factor) {
        cat(
          "  [note] site covariate ",
          cov.name,
          " is a factor and was not standardized (but was converted to integer) \n",
          sep = ""
        )
        cov.dat$cov <- as.integer(as.factor(cov.dat$cov))

      }

      if (scale.covs & (!is.binary & !is.character(cov.dat$cov))) {
        cov.dat$cov <- standardize(cov.dat$cov)
      }

      ### specify the fill value
      fill.value <- ifelse(!fill.cov.nas, NA, fill.cov.nas)

      ## create a matrix for the site covariates and add to the Xsite list
      cov.mat  <-  reshape2::acast(cov.dat,
                                   siteind ~ yearind,
                                   value.var = "cov",
                                   fill = fill.cov.nas)
      ## to ensure rownames and colnames match the indexes..
      stopifnot(all(as.integer(rownames(cov.mat)) == as.integer(sort(
        unique(cov.dat$siteind)
      ))))
      stopifnot(all(as.integer(colnames(cov.mat)) == as.integer(sort(
        unique(cov.dat$yearind)
      ))))

      Xsite[[i]][[j]] <- cov.mat
      names(Xsite[[i]])[j] <- cov.name
      ### finally, replace the orgiinal data with the transformed vectors.
      cov.dat$cov[is.na(cov.dat$cov)] <- fill.value
      LL[[i]][cov.name]  <- cov.dat$cov
    } # j loop
  }# end Xsite i loop
  cat("  [note] done munging site-level covariates\n")

  ## rename LL elements
  bbs   <- LL$bbs
  ebird <- LL$ebird
  rm(LL)


 # BBS rte-observer index --------------------------------------------------
 ### create an index for route-observer combination effect
 bbs <- bbs |>
    dplyr::distinct(siteind, obsind) |>
    dplyr::mutate(rteobsind = 1:n()) |>
  full_join(bbs, by = c("obsind", "siteind"))


  # GRID-LEVEL COVARIATES ---------------------------------------------------
  ## create arrays for grid cell-level covariates
  ### dimensions are ngrid by nyear
  Xgrid <- list(NULL)
  cellindex <- cellindex |> arrange(cellind)
  cellind  <- as.data.frame(cellindex)
  cell.covs  <- cell.covs[cell.covs %in% colnames(cellindex)]
  if (!length(cell.covs) == 0) {
    for (i in seq_along(cell.covs)) {
      Xgrid[[i]] <- cellind[, cell.covs[i]]
      names(Xgrid)[i] <- cell.covs[i]
    }# end Xgrid loop
  }# end if cell.covs


# BASIS FUNCTIONS -------------------------------------------------------------
  # create basis functions and data for GAM model components
  ## first, we need to grab the maximum number of birds per grid cell/year
  ## argument use.ebird.in.EN lets user ignore the eBird data when producing the GAM data
  ## this is important when modeling only the BBS data, as eBird observations
  ## are typically >>> BBS observations for some (many?) species.
  if (use.ebird.in.EN) {
    EN <- rbind(
      bbs |> dplyr::distinct(cellind, yearind, c),
      ebird |> dplyr::distinct(cellind, yearind, c)
    )
  } else{
    EN <-  bbs |> dplyr::distinct(cellind, yearind, c)
  }

  # evaluate according to specified argument.
  ### id prefer to just evaluate the argument inside the
  ### the filter call, but not sure rn how to do that.
  ### using filter(c==eval(parse(text=paste0(EN.arg,"(c,na.rm=TRUE)")))))
  ### did not work. Nor does bang bang !!EN.arg(c,na.rm=TRUE)
  ### have yet to test out !!rlang::sym(EN.arg)(c,na.rm=TRUE)..

  myfun = paste0(
    "test <- EN |>\n
      dplyr::group_by(cellind, yearind) |>\n
      dplyr::filter(c ==",
    EN.arg,
    "(c, na.rm=TRUE)) |>\n
      dplyr::ungroup()\n"
  )
  EN <- eval(str2expression(myfun))

  ## next, add the missing grid cells and fill in with grand MEAN
  gy.all <- expand.grid(cellind = cellindex$cellind,
                        yearind = yearindex$yearind)

  if(drop.empty.cells){
    remove <- setdiff(gy.all$cellind, c(bbs$cellind, ebird$cellind))
    if(length(remove) > 0){
        gy.all <- gy.all[!gy.all$cellind %in% remove,]
    }
  }

  EN <-
    full_join(gy.all, EN, by = c("cellind", "yearind"))
  EN$c[is.na(EN$c)] <-
    round(mean(EN$c, na.rm = TRUE), 0)

  ## make a matrix of EN for use in JAGS model as the expected abundance in grid cell.
  ### for casting into a matrix, first sum C within each cell and year.
  EN <- EN |> dplyr::group_by(cellind, yearind) |>
    dplyr::mutate(c = sum(c, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::distinct()
  EN.mat   <-  reshape2::acast(EN,
                               cellind ~ yearind,
                               value.var = "c",
                               fill = mean(EN$c, na.rm = TRUE))


  # create data for use in creating spatial basis functions
  bf.in <- data.frame(merge(EN, cellindex))
  bf.in <-
    bf.in |>
    dplyr::group_by(cellind) |>
    dplyr::filter(c == max(c, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::distinct(cellind, .keep_all = TRUE)

  ## 'scale' down the XY coordinates until max is under 10 (10 is an arbitrary choice sorta)
  while (abs(max(c(bf.in$X, bf.in$Y), na.rm = TRUE)) > 10) {
    warning(
      "Values of X and Y coordinates are > 10, scaling down by factors of 5 until max value < 10.\n"
    )
    bf.in <- bf.in |>
      dplyr::mutate(X = X / 5,
                    Y = Y / 5)
  }

  ## SPATIAL NEIGHBORHOOD ----------------------------------------------------
  ## For now just using default values for building neighborhood...
  # fnb <-
  #   paste0(dir.outputs,
  #          "/neighborhood",
  #          ifelse(dev.mode, "-dev", ""),
  #          ".rds")
  # dir.create(paste0(dir.outputs, "/neighborhood"), showWarnings = FALSE)
  # cat("  [note] creating spatial neighborhood and saving output to ",
  #     fnb,
  #     "\n")
  # browser()
  # nb.coords <- sf::st_coordinates(sf::st_geometry(grid))
  nb.coords <- as.matrix(cbind(grid$X, grid$Y))
  nb <-
    spdep::poly2nb(as(grid, "Spatial"), row.names = cellindex$cellind)
  # spdep::is.symmetric.nb(nb, verbose = FALSE, force = TRUE)
  N = length(nb)
  (num = sapply(nb, length))
  adj = unlist(nb)
  sumNumNeigh = length(unlist(nb))
  nbWB <- spdep::nb2WB(nb)
  nbWB$sumNumNeigh = sumNumNeigh
  # saveRDS(nb, fnb)

  rm(nb, N, adj, sumNumNeigh)
# browser()

# BUNDLE UP DATA ----------------------------------------------------------
  # make index logical for whether or
  # # if return.orig.dat == TRUE then we want to keep all
  bundle.out <- list(
    bbs.df      = bbs   |> dplyr::distinct(yearind, siteind, .keep_all = TRUE),
    ebird.df    = ebird |> dplyr::distinct(yearind, siteind, .keep_all = TRUE),
    grid.df     = cellindex |> dplyr::distinct(cellind, cellid, X, Y, .keep_all = TRUE) |>
      dplyr::mutate(X = nb.coords[,1], Y = nb.coords[,2]),
    coords      = nb.coords,
    EN          = EN.mat,
    # max C per grid per year  (zero-filled)
    Xb = as.array(Xsite$bbs),
    Xe = as.array(Xsite$ebird),
    # Xgrid      = Xgrid,
    Xg         = as.array(Xgrid),
    # proportion of routes in grid cell as matrix (dim <nsites by ngrids>)
    prop       = as.matrix(prop$bbs), ## keeping this here during dev phase.
    propb      = as.matrix(prop$bbs),
    prope      = as.matrix(prop$ebird),
    # % BBS route per grid cell (dims <ngrid nroutes>)
    T.ind = yearindex,
    ref.year   = round(median(1:length(
      unique(yearindex$yearind)
    ))),
    # lookup table for year
    # create indexes here
    T          = length(unique(yearindex$yearind)),
    # number of years across all data
    G          = length(unique(cellindex$cellid)),
    # number of grid cells in study area
    Mb         = length(unique(bbs$siteid)),
    # number of routes bbs
    Me         = length(unique(ebird$siteid)),
    # number of checklists bbs
    gy         = as.matrix(gy.all),
    ngy        = nrow(gy.all),
    ## more indexes
    # GTb        = bbs   |> dplyr::distinct(cellind, yearind) |> dplyr::arrange(cellind, yearind),
    # grid-years sampled bbs
    # GTe        = ebird |> dplyr::distinct(cellind, yearind) |> dplyr::arrange(cellind, yearind),
    # grid-years sampled ebird
    # all JAGAM output
    # Z.mat      = Z.mat,
    # dims <ncells  by nbfs/knots
    # K       = nbfs,
    # number of basis functions/knots
    #### neighborhood stuff
    adj         = nbWB$adj,
    wts         = nbWB$weights,
    num         = nbWB$num,
    NN          = sum(nbWB$num)
  )



  # RETURNED OBJECT ---------------------------------------------------------
  cat("  [note] packaging the data into a single list\n")

  return(bundle.out)

}# END FUNCTION
