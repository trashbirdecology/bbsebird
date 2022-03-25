#' @title Bundle Data for Use in JAGS
#' @param bbs BBS data table
#' @param ebird eBird data table
#' @param grid spatial sampling grid/study area table
#' @param drop.na.cov.obs logical if TRUE will remove ALL data where any specified covariate does not exist. If TRUE, suggest examining the data and covariates prior to specifying site.covs and grid.covs.
#' @param scale.covs logical if TRUE will automatically scale the numeric/integer covariates.
#' @param K the maximum number of basis functions that JAGAM will produce. Used for code development purposes, mostly. Do not change unless you know what you're doing.
#' @param X variable name associated with the x-coordinate (e.g., long, longitude, Easting, X) across 'grid', 'bbs', and 'ebird'
#' @param Y variable name associated with the x-coordinate (e.g., latitude, Northing, Y) across 'grid', 'bbs', and 'ebird'
#' @param cell.id column name(s) of the grid cell identifier
#' @param site.id column name(s) of the site  identifier (e.g., BBS route, eBird checklists)
#' @param year.id column name of the temporal identifier
#' @param bf.method default value "cubic2d". Alternatives include cubic splines using one of c("mgcv", "jagam").
#' @param dir.jagam directory location of where to save the JAGAM.bugs model file created by mgcv::jagam(). Defaults to ./models/
#' @param obs.id  column name(s) of the observer identifier
#' @param use.ebird.in.ENgrid logical if TRUE will use data across both eBird and BBS observations to create basis functions.
#' @param cell.covs column name(s) of the grid-level covariates
#' @param ENgrid.arg if "max" will use the maximum value of observed birds at each grid cell. Alternatives include "min", "mean".
#' @param site.covs column name(s) of the site-level covariates
#' @param mins.to.hours logical if TRUE will convert covariates on the minute scale to the hour scale.
#' @param dev.mode logical if TRUE will return a reduced data set to use in development/debugging purposes. This method reduces the number of time units to 2, the maximum number of grid cells to 10, and 2 sites from each data source
#' @param fill.cov.nas value with with to fill missing covariate values. User can specify value as FALSE if no fill is requested.
#' @importFrom dplyr group_by mutate select distinct arrange filter
#' @export make_bundle
#' @param return.orig.dat logical if TRUE will return the bbs, ebird, and grid data as data.frames in returned list. If FALSE, returned list will include lookup tables to link cell.ind, site.ind, and year.ind to cell.id, site.id, and year.id for data.

make_bundle <- function(bbs,
                        ebird,
                        grid,
                        drop.na.cov.obs = TRUE,
                        mins.to.hours = TRUE,
                        scale.covs  = TRUE,
                        fill.cov.nas = NA,
                        return.orig.dat = FALSE,
                        bf.method   = "cubic2D",
                        use.ebird.in.ENgrid = TRUE,
                        ENgrid.arg    = "max",
                        X           = "cell.lon.centroid",
                        Y           = "cell.lat.centroid",
                        cell.id     = "gridcellid",
                        year.id     = "year",
                        site.id     = c("checklist_id", "rteno"),
                        obs.id      = c("obsn", "observer_id"),
                        cell.covs   = c("area"),
                        site.covs   = c(
                          "starttime",# bbs
                          "endtime",  # bbs
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
                        K = NULL,
                        dev.mode    = FALSE,
                        dir.jagam   = "models/") {
  # EVALUATE ARGS -----------------------------------------------------------
  ## first, test and evaluate args as necessary.
  ENgrid.arg <- tolower(ENgrid.arg)
  bf.method  <- tolower(bf.method)
  stopifnot(ENgrid.arg %in% c("mean", "max", "min"))
  stopifnot(length(fill.cov.nas) == 1)
  stopifnot(is.logical(use.ebird.in.ENgrid))
  stopifnot(is.logical(scale.covs))
  stopifnot(is.logical(drop.na.cov.obs))
  stopifnot(bf.method %in% c("mgcv", "jagam", "cubic2d"))

  # Munge Data Frames a Little Prior to Data Munging ---------------------------------------------------
  bbs   <- as.data.frame(bbs)
  ebird <- as.data.frame(ebird)
  ## force colnames to lower
  names(bbs)   <- tolower(names(bbs))
  names(ebird) <- tolower(names(ebird))
  names(grid)  <- tolower(names(grid))

  # Rename Data Table Colnames ----------------------------------------------
  L <- list(bbs = bbs,
            ebird = ebird,
            grid = grid)
  for (i in seq_along(L)) {
    n <- ns <- nt <- nc <- no <- nx <- ny <- NULL
    ### find overlap in things i want to rename
    n  <- names(L[[i]])
    if (any(year.id %in% n))
      nt <- data.frame(old = n[n %in% c(year.id)], new = "year.id")
    if (any(obs.id %in% n))
      no <- data.frame(old = n[n %in% c(obs.id)], new = "obs.id")
    if (any(site.id %in% n))
      ns <- data.frame(old = n[n %in% c(site.id)], new = "site.id")
    if (any(cell.id %in% n))
      nc <- data.frame(old = n[n %in% c(cell.id)], new = "cell.id")
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
    bbs |> distinct(year.id, site.id, cell.id, c, .keep_all = TRUE)
  ## (duplicates in ebird, though it might be because we have multiple observers. huge amount of excess data being imported...)
  ebird <-
    ebird |> distinct(year.id, site.id, cell.id, c, .keep_all = TRUE)

  # Subset for dev.mode=TRUE ------------------------------------------------
  if (dev.mode) {
    message(
      paste0(
        "[notice] `dev.mode` is TRUE. Output object will be a small subset of the original data.\n"
      )
    )
    # ebird=ebird_spatial;bbs=bbs_spatial;grid=study_area
    # keep 3 years data max
    maxyr <- max(unique(bbs$year.id), na.rm = TRUE)
    totalyrs <- maxyr - min(unique(bbs$year.id), na.rm = TRUE)
    T.keep <- (maxyr - min(5, totalyrs)):maxyr
    ## keep max 30 grid cells,
    ## sample 5 and grab potentially adjacent neighbors.
    G.keep <- sample(sort(unique(grid$cell.id))-1, 30) # but dont grab the last one
    G.keep  <- c(G.keep+1, G.keep)
    grid <- grid[grid$cell.id %in% G.keep,]

    bbs  <-   bbs |> filter(cell.id %in% G.keep &
                              year.id %in% T.keep)
    ebird  <- ebird |> filter(cell.id %in% G.keep &
                                year.id %in% T.keep)
    ## keep random sample of 10 routes or checklists per grid cell
    ### this will likely not reduce bbs by much (possible zero reduction) but will reduce ebird
    #### slice_max orders by site and takes 'top' n
    b.samp.keep <- bbs |>
      dplyr::distinct(year.id, cell.id, site.id, .keep_all = TRUE) |>
      group_by(year.id, cell.id) |>
      mutate(maxn = n_distinct(site.id))
    bbs <- b.samp.keep |>
      slice_sample(n = min(b.samp.keep$maxn, 10)) |>
      ungroup()
    e.samp.keep <- ebird |>
      dplyr::distinct(year.id, cell.id, site.id, .keep_all = TRUE) |>
      group_by(year.id, cell.id) |>
      mutate(maxn = n_distinct(site.id))
    ebird <- e.samp.keep |>
      slice_sample(n = min(e.samp.keep$maxn, 10)) |>
      ungroup()

    rm(T.keep, G.keep, e.samp.keep, b.samp.keep)
  }


  # DROP COLS WHERE ALL ROWS == NA ------------------------------------------
  ### some covariates may have all NA. prior to subsetting, let's remove those columns.
  all_na <- function(x){
    any(!is.na(x))
        }
  LLL <- list(ebird = ebird, bbs = bbs)
  newlist <- list(ebird = NULL, bbs = NULL)
  for (i in seq_along(LLL)) {
    newlist[[i]] <- LLL[[i]] |> select_if(all_na)
    diff <- setdiff(colnames(LLL[[i]]), colnames(newlist[[i]]))
    if (length(diff) > 0)
      message(
        "[note] the following columns in the ",
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
      bbs[complete.cases(bbs[, c(colnames(bbs)[colnames(bbs) %in% c("c", site.covs)])]),]
    ebird <-
      ebird[complete.cases(ebird[, c(colnames(ebird)[colnames(ebird) %in% c("c", site.covs)])]),]

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
  cell.index <- grid |>
    units::drop_units() |>
    arrange(cell.id) |>
    distinct(cell.id, X, Y, .keep_all = TRUE) |>
    # create grid cell id as index and not identity (though they may be the)
    mutate(cell.ind = 1:n())
  # keep only useful information....
  ### ie drop covs not specified in grid.covs
  cell.index <-
    cell.index[names(cell.index) %in% c("cell.id", "cell.ind", "X", "Y", cell.covs)]
  ## scale if specified
  if (scale.covs) {
    cell.index <- cell.index |>
      mutate(area = standardize(area)) |>
      mutate(X  = standardize(X)) |>
      mutate(Y  = standardize(Y))
  }
  ## YEAR INDEX
  year.index <-
    data.frame(year.id = min(c(ebird$year.id, bbs$year.id), na.rm = TRUE):max(c(ebird$year.id, bbs$year.id), na.rm =
                                                                                TRUE)) |> mutate(year.ind = 1:n())
  # ### YEAR-GRID INDEX
  # # make a version of all years and grid index combinations, regardless data availability.
  # # this is used here, internally, but can also be called in certain jags model components
  yg.index <-
    expand.grid(year.ind = year.index$year.ind,
                cell.ind = cell.index$cell.ind) |>
    merge(year.index) |>
    merge(cell.index) |>
    dplyr::select(cell.ind, cell.id, year.ind, year.id)

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
      filter(!is.na(site.id),!is.na(c))
    ## create index each for SITE and OBSERVER, then append to the data frame
    LL[[i]] <-
      LL[[i]] |>
      mutate(obs.ind  = obs.id  |> as.factor() |> as.integer()) |>
      mutate(site.ind = site.id |> as.factor() |> as.integer())
    ## just to be safe, ensure all join by vars are same type
    LL[[i]] <- LL[[i]] |>
      mutate(year.id = as.integer(year.id),
             cell.id = as.integer(cell.id))

    ## this is a shitty workaround, but currently (other than using data.table)
    ## is the only tractable way I know for dealing with the large eBird data tables...
    ## using a direct left join or a merge is not possible, even on my 60+GB RAM machine..
    ## very happy to receive changes
    #### extract the columsn on which to join

    LL.sub <- LL[[i]] |> dplyr::select(cell.id, year.id)
    LL.sub <-
      LL.sub |> left_join(yg.index, by = c("cell.id", "year.id"))
    stopifnot(nrow(LL.sub) == nrow(LL[[i]]))
    ### remove from the LL.sub extracted columns from data and then append new (four ) columns
    LL[[i]] <- LL[[i]] |> dplyr::select(-cell.id,-year.id)

    LL[[i]] <- LL[[i]] |>  dplyr::bind_cols(LL.sub)

    stopifnot(!any(is.na(LL[[i]]$cell.ind)))
    stopifnot(!any(is.na(LL[[i]]$year.ind)))

    ## finally, drop the NA observations on bbs and ebird..
    LL[[i]] <-     LL[[i]][!is.na(LL[[i]]$site.ind), ]

  } # end LL loop
  # browser()
  ##extract from list
  bbs   <- LL$bbs
  ebird <- LL$ebird
  rm(LL)


  # BBS-SPECIFIC DATA : PROP (% site.ind in cell.ind) ----------------------------------------------------
  cat("  [note] creating prop matrix for bbs routes/grid cells\n")
  stopifnot(nrow(bbs |> distinct(site.ind, cell.ind, proprouteincell)) ==
              nrow(bbs |> distinct(site.ind, cell.ind)))
  ## grab all cell ids and site inds
  prop <- bbs |>
    distinct(site.ind, cell.ind, proprouteincell) |>
    full_join(cell.index |> dplyr::select(cell.ind), by = "cell.ind")
  prop$proprouteincell[is.na(prop$proprouteincell)] <-
    0  ##supply prop with zero if route
  prop <-
    reshape2::acast(prop,
                    site.ind ~ cell.ind,
                    value.var = "proprouteincell",
                    fill = 0)
  # remove the rownames==NA (the last row usually..)
  if (any(rownames(prop) %in% c("NA", NA))) {
    prop <-
      prop[-which(rownames(prop) %in% c(NA, "NA")),]

    if (dev.mode)
      message(
        "[notice] `dev.mode`is TRUE. Please expect all(rowSums(data$prop)!=1). Route segments are calculated in make_bbs_spatial(). \n"
      )
  }


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
      LL[[i]] |> dplyr::distinct(site.ind, year.ind, .keep_all = TRUE)
    for (j in seq_along(temp)) {
      cov.name  <- temp[j]

      cov.dat   <- data.frame(as.vector(LL[[i]][cov.name]),
                              LL[[i]]["site.ind"],
                              LL[[i]]["year.ind"])
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
      } else{!is.character(cov.dat$cov) && !(max(cov.dat$cov, na.rm = TRUE) > 1)
      }
      if(cov.name %in% c("endtime", "starttime")){
        cov.dat$cov <- as.integer(cov.dat$cov)
      }

      is.factor <-
        ifelse(is.factor(cov.dat$cov),
               TRUE,
               FALSE)
      if (is.time)
        cov.dat$cov <- cov.dat$cov@hour * 60 + cov.dat$cov@minute
      if (is.time &
          grepl("minute|time_observations_started", cov.name)) {
        message("[note] converting covariate ",
                cov.name ,
                " from minutes to hours")
        cov.dat$cov <- cov.dat$cov / 60
      }
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
                                   site.ind ~ year.ind,
                                   value.var = "cov",
                                   fill = fill.cov.nas)
      ## to ensure rownames and colnames match the indexes..
      stopifnot(all(as.integer(rownames(cov.mat)) == as.integer(sort(
        unique(cov.dat$site.ind)
      ))))
      stopifnot(all(as.integer(colnames(cov.mat)) == as.integer(sort(
        unique(cov.dat$year.ind)
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

  # GRID-LEVEL COVARIATES ---------------------------------------------------
  ## create arrays for grid cell-level covariates
  ### dimensions are ngrid by nyear
  Xgrid <- list(NULL)

  cell.index <- cell.index |> arrange(cell.ind)
  cell.covs  <- cell.covs[cell.covs %in% colnames(cell.index)]
  if (!length(cell.covs)==0) {
    for (i in seq_along(cell.covs)) {
      Xgrid[[i]] <- cell.index[, cell.covs[i]]
      names(Xgrid)[i] <- cell.covs[i]
    }# end Xgrid loop
  }# end if cell.covs

  # BASIS FUNCTIONS -------------------------------------------------------------
  # create basis functions and data for GAM model components
  ## first, we need to grab the maximum number of birds per grid cell/year
  ## argument use.ebird.in.ENgrid lets user ignore the eBird data when producing the GAM data
  ## this is important when modeling only the BBS data, as eBird observations
  ## are typically >>> BBS observations for some (many?) species.
  # cat("  [note] creating basisi functions")
  if (use.ebird.in.ENgrid) {
    ENgrid <- rbind(
      bbs |> dplyr::distinct(cell.ind, year.ind, c),
      ebird |> dplyr::distinct(cell.ind, year.ind, c)
    )
  } else{
    ENgrid <-  bbs |> dplyr::distinct(cell.ind, year.ind, c)
  }

  # evaluate according to specified argument.
  ### id prefer to just evaluate the argument inside the
  ### the filter call, but not sure rn how to do that.
  ### using filter(c==eval(parse(text=paste0(ENgrid.arg,"(c,na.rm=TRUE)")))))
  ### did not work. Nor does bang bang !!ENgrid.arg(c,na.rm=TRUE)
  ### have yet to test out !!rlang::sym(Engrid.arg)(c,na.rm=TRUE)..

  myfun = paste0(
    "test <- ENgrid |>\n
      dplyr::group_by(cell.ind, year.ind) |>\n
      dplyr::filter(c ==",
    ENgrid.arg,
    "(c, na.rm=TRUE)) |>\n
      dplyr::ungroup()\n"
  )
  ENgrid <- eval(str2expression(myfun))

  ## next, add the missing grid cells and fill in with grand MEAN
  gy.all <- expand.grid(cell.ind = cell.index$cell.ind,
                        year.ind = year.index$year.ind)
  ENgrid <-
    full_join(gy.all, ENgrid, by = c("cell.ind", "year.ind"))
  ENgrid$c[is.na(ENgrid$c)] <-
    round(mean(ENgrid$c, na.rm = TRUE), 0)

  ## make a matrix of ENgrid for use in JAGS model as the expected abundance in grid cell.
  ### for casting into a matrix, first sum C within each cell and year.
  ENgrid <- ENgrid |> dplyr::group_by(cell.ind, year.ind) |>
    dplyr::mutate(c = sum(c, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::distinct()
  ENgrid.mat   <-  reshape2::acast(
    ENgrid,
    cell.ind ~ year.ind,
    value.var = "c",
    fill = mean(ENgrid$c, na.rm = TRUE)
  )


  # create data for use in creating spatial basis functions
  bf.in <- data.frame(merge(ENgrid, cell.index))
  bf.in <-
    bf.in |>
    dplyr::group_by(cell.ind) |>
    dplyr::filter(c == max(c, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::distinct(cell.ind, .keep_all = TRUE)

  ### 'scale' down the XY coordinates until max is under 10 (10 is an arbitrary choice sorta)
  while (abs(max(c(bf.in$X, bf.in$Y), na.rm = TRUE)) > 10) {
    warning("Values of X and Y coordinates are > 10, scaling down by factors of 5 until max value < 10.\n")
    bf.in <- bf.in |>
      dplyr::mutate(X = X / 5,
                    Y = Y / 5)
  }


  ## DEFINE K  ---------------------------------------------------------------
  # if not specified, K is defined as:
  ### this is kind of arbitrary. based on some Wood, Simon paper about min 20
  ### and anything > like 100 will crash most systems.
  ### also need to consider compute time for use in Bayesian param estiamtion
  if (!is.null(K) && K > length(unique(ENgrid$cell.ind))) {
    message(
      "[important] you defined K as a value higher than the unique number of available grid cells. Resetting K automatically. See notes following. \n"
    )
    K <- NULL
  }
  if (is.null(K)) {
    ###logic for K selection borrowed from AHM2 book , which cites Giminez et al. 2009 and Rupert et al. 2003
    K <- max(20, min(round(length(
      unique(ENgrid$cell.ind)
    ) / 4), 150))
  }

  ## JAGAM -------------------------------------------------------------------------
  cat(
    "  [note] K is currently set to ",
    K,
    ". If you have memory errors, try defining K in arguments as a lower value\n"
  )
  if (bf.method %in% c("mgcv", "jagam")) {
    cat("  [note] creating 2D duchon splines using `mgcv::jagam()`\n")
    jagam.fn <- paste0(dir.jagam, "/gam-UNEDITED.txt")
    jagam.mod <- mgcv::jagam(
      c ~ s(
        # note the c doesn't matter, it's just for show
        X,
        Y,
        bs = "ds",
        k = K,
        m = c(1, 0.5)
      ),
      file = jagam.fn,
      sp.prior = "log.uniform",
      data = bf.in,
      diagonalize = TRUE,
      # parallell = TRUE,
      # modules = "glm"
      family = "poisson"
    )
    jagam.mod$fn <- jagam.fn
    ## specify some output scalars and data
    Z.mat <-
      jagam.mod$jags.data$X               # dims <ncells  by nknots>
    nbfs  <-
      dim(jagam.mod$jags.data$X)[2]        # number of basis functions/knots

  } else{
    jagam.mod <- NULL
  }

  ##STREBEL ET AL METHOD --------------------------------------------------
  ### follow the methods of Strebel et al. (which follows methods of Royle and Kery AHM)
  if (bf.method %in% c("cubic2d")) {
    # browser()
    cat("  [note] creating 2D cubic splines following Strebel et al. \n")
    XY <- bf.in[c("X", "Y")] ### the "scaled down" coordinates
    XY.orig <- cell.index[c("X", "Y")]
    # Define the omega and Z.k matrices for the random effects
    omega.all <-
      fields::rdist(XY, XY) ^ 3 # 2D cubic splines on "reduced coords
    svd.omega.all <- svd(omega.all)
    sqrt.omega.all <-
      t(svd.omega.all$v %*% (t(svd.omega.all$u) * sqrt(svd.omega.all$d)))
    ##
    Z.k   <- (fields::rdist(XY.orig, XY)) ^ 3
    Z.mat <- t(solve(sqrt.omega.all, t(Z.k)))
    nbfs  <- dim(Z.mat)[2]
  }


  # THIS IS BEING MADE OUTSIDE MAKE_BUNDLE FOR NOW....
  ## SPATIAL NEIGHBORHOOD ----------------------------------------------------
  ## For now just using default values for building neighborhood...
  xy <- sf::st_coordinates(grid)
  xx <-
    spdep::poly2nb(as(grid, "Spatial"), row.names = cell.index$cell.ind)
  # spdep::is.symmetric.nb(xx, verbose = FALSE, force = TRUE)
  N = length(xx)
  (num = sapply(xx, length))
  adj = unlist(xx)
  sumNumNeigh = length(unlist(xx))
  nbWB <- spdep::nb2WB(xx)
  nbWB$sumNumNeigh = sumNumNeigh
  rm(xx, N, adj, sumNumNeigh)


  # BUNDLE UP DATA ----------------------------------------------------------
  # make index logical for whether or
  # if return.orig.dat == TRUE then we want to keep all
  keep.ind <- ifelse(return.orig.dat, TRUE, FALSE)
  bundle.out <- list(
    bbs.df      = bbs   |> distinct(year.ind, site.ind, .keep_all = keep.ind),
    ebird.df    = ebird |> distinct(year.ind, site.ind, .keep_all = keep.ind),
    grid.df     = cell.index |> distinct(cell.ind, cell.id, .keep_all = keep.ind),
    # "all" data as data tables
    # max C per grid per year  (zero-filled)
    Cmax        = ENgrid.mat,
    # covariate matrices as lists
    # Xsite      = Xsite,
    # can run simplify2array here to output an array...
    Xb = simplify2array(Xsite$bbs),
    Xe = simplify2array(Xsite$ebird),
    # Xgrid      = Xgrid,
    Xg         = simplify2array(Xgrid),
    # proportion of routes in grid cell as matrix (dim <nroutes by ngrids>)
    prop       = as.matrix(prop),
    # % BBS route per grid cell (dims <ngrid nroutes>)
    G.ind = dat.dev$G.ind[c("X", "Y")] |> sf::st_drop_geometry(),
    # lookup table for grid cells
    T.ind = year.index,
    # lookup table for year
    # create indexes here
    T          = length(unique(year.index$year.ind)),
    # number of years across all data
    G          = length(unique(cell.index$cell.id)),
    # number of grid cells in study area
    Mb         = length(unique(bbs$site.id)),
    # number of routes bbs
    Me         = length(unique(ebird$site.id)),
    # number of checklists bbs
    gy         = gy.all,
    ngy        = nrow(gy.all),
    ## more indexes
    # GTb        = bbs   |> dplyr::distinct(cell.ind, year.ind) |> dplyr::arrange(cell.ind, year.ind),
    # grid-years sampled bbs
    # GTe        = ebird |> dplyr::distinct(cell.ind, year.ind) |> dplyr::arrange(cell.ind, year.ind),
    # grid-years sampled ebird
    # all JAGAM output
    Z.mat      = Z.mat,
    # dims <ncells  by nbfs/knots
    K       = nbfs,
    # number of basis functions/knots
    #### neighborhood stuff
    adj         = nbWB$adj,
    wts         = nbWB$weights,
    num         = nbWB$num,
    NN          = nbWB$sumNumNeigh,
    #### GRID AND YEAR LOOKUP TABLES
    cell.index  = cell.index |> distinct(cell.ind, cell.id),
    year.index  = year.index


  )



  # RETURNED OBJECT ---------------------------------------------------------
  cat("  [note] packaging the data into a single list\n")

  return(bundle.out)

}# END FUNCTION
