#' Munge and Output a List or List of Lists for Use in JAGS
#'
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param scale.vars Logical If TRUE will scale variables. This needs to be improved/checked.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param max.C.ebird if TRUE will drop all checklists where number of observed birds is greater than 100. This is an arbitrary number.
#' @param fn.out Filename of the list object output as a .RDS file
#' @param jagam.args List of arguments used in \code{mgcv::jagam()}. Arguments include c(bs, k, family, sp.prior, m, ...)
#' @param dir.models Location for where to store the GAM portion of the JAGS model
#' @param overwrite if FALSE will not overwrite file if it exists at location `fn.out`
#' @importFrom mgcv jagam
#' @importFrom dplyr group_by filter summarise distinct ungroup arrange mutate rename select
#' @importFrom stringr str_detect
#' @importFrom units drop_units
#' @importFrom sf st_drop_geometry
#' @importFrom reshape2 acast
#' @export make_jags_list
make_jags_list <-
  function(dat,
           dir.models,
           dir.out,
           jagam.args,
           max.C.ebird=100,
           scale.vars = TRUE,
           overwrite=FALSE,
           fn.out = "jdat") {

    ## create output filename
    fn = tolower(paste0(paste0(dir.out, "/", fn.out, ".RDS")))
    ### make/check fn
    if(file.exists(fn) & !overwrite){
      cat("File ", fn," exists and `overwrite`== FALSE. Importing from file. \n")
      jdat <- readRDS(fn)
      return(jdat)}

    # force object(s) in dat to a list, despite length
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
    stopifnot(all(c("grid", "bbs","ebird") %in% names(dat)))

    #Rearrange the list elements
    dat <- list(grid  = dat$grid,
                bbs   = dat$bbs,
                ebird = dat$ebird)

    # specify all the possible objects to go into a single list of listout
    objs <-
      c(
        "C", # observed counts
        "Xp", # site-level detection covariates
        "indexing" # indexes for where and how much data exists within each dataset
       )

# YEARS -------------------------------------------------------------------
    ## create index for all years across data
    year.index <-
      data.frame(year.id = sort(unique(c(
        unique(dat$bbs$year), unique(dat$ebird$year)
      )))) %>%
      dplyr::mutate(year.ind = 1:n())

# GRID -----------------------------------------------------
    ### create list of shit for GRID
    grid.list <- NULL
    grid.index <- dat$grid   %>%
      sf::st_drop_geometry() %>%
      units::drop_units() %>%
      dplyr::arrange(gridcellid) %>%
      dplyr::filter(!is.na(gridcellid)) %>%  # just to be safe.
      dplyr::rename(grid.id = gridcellid,
             X=cell.lon.centroid,
             Y=cell.lat.centroid) %>%
      dplyr::mutate(grid.ind  = 1:nrow(dat$grid))
    grid.list$index <- grid.index %>% dplyr::select(grid.id, grid.ind)
    grid.list$XY    <- grid.index %>% dplyr::select(X, Y)
    grid.list$area  <- grid.index %>% dplyr::select(area)

# BBS AND EBIRD --------------------------------------
# intialize mpty maxN
maxN <- NULL
for (i in seq_along(dat)) {
      ind <-
        names(dat)[i] # make lazy indicator for which data we are munging
      if(!ind %in% c("ebird", "bbs"))next()
      cat("building ", ind, " objects..\n")
      ## grab data frame
      df <- dat[[i]]
      names(df) <- tolower(names(df))

      ## Do some light munging
      if ("sf" %in% class(df)) {
        df <- df %>% sf::st_drop_geometry()
      }

      rownames(df) <- NULL
      df <- df %>%
        ### add year index
        rename(year.id = year) %>%
        left_join(year.index)
      ### add grid index
      df <- df %>%
        rename(grid.id = gridcellid) %>%
        left_join(grid.index %>% dplyr::select(grid.id, grid.ind))
      ### calculate mean det covs for bbs
      if (ind == "bbs") {
        df <- df %>% dplyr::rename(site.id = rteno) %>%
          mutate(windmean = abs(startwind - endwind) / 2) %>%
          mutate(skymean = abs(startsky - endsky) / 2)
      }

      ### filter out max C if ebird
      if (ind == "ebird") {
        df <- df %>%
          dplyr::rename(site.id = checklist_id)
        # replace the max values with NA rather tahn remove the rows entirely beucase we want to keep all the sites/grids combinations here
        df$c[df$c > max.C.ebird] <- NA
      }

      ## Site index -----------------------------------------------
      ## create some indexes and IDS to ensure all matrices are properly sorted.
      site.index <- df %>%
        dplyr::distinct(site.id) %>%
        dplyr::filter(!is.na(site.id)) %>%
        dplyr::arrange(site.id) %>%
        dplyr::mutate(site.ind = 1:n())

      ## add the grid and site site.ind to the data frame
      df <- df %>% dplyr::left_join(site.index)

      ## ensure all grid cells are represented in the dataset for creating even matrices
      df <- df %>% dplyr::full_join(grid.index %>% dplyr::select(grid.id, grid.ind))


## C: Count Matrix ------------------------------------------------------------
  ## make a matrix of observed counts
      C   <-
        make_mat(
          df %>%
            dplyr::distinct(year.ind, site.ind, c) %>%
            dplyr::filter(!is.na(site.ind)), # be sure to remove na sites
          row = "site.ind",
          col = "year.ind",
          val = "c"
        )
      stopifnot(as.integer(rownames(C))==sort(as.integer(rownames(C))))

##XP: det. covs --------------------------------------------------
  ## Specify all the possble varibles (and desired names) to be used as detection covariates in model
  Xp.val       = c("number_observers",
                       "duration_minutes",
                       "effort_area_ha",
                       "time_observations_started",
                       "observer_id",
                       "c",
                       "carmean",
                       "windmean",
                       "noisemean",
                       "obsfirstyearbbs",
                       "obsfirstyearroute",
                       "assistant"
      )
  Xp.names     = c("nobs",
                       "nmins",
                       "effort_ha",
                       "start_time",
                       "obs_id",
                       "C",
                       "car",
                       "wind",
                       "mean",
                       "fyrbbs",
                       "fyrroute",
                       "assistant"
      )

  ## make the detection covariates matrices (with dimensiosn site by)
  Xp <- list()
  for(j in seq_along(Xp.val)){
    cols <- c("site.ind", "year.ind", Xp.val[j])
    if(!all(cols %in% names(df))){next()}
    xpdf <- df %>%
      dplyr::select(!!!cols) %>%
      dplyr::filter(!is.na(site.ind)) %>% ## must remove NA site.ind
      dplyr::distinct() %>%
      dplyr::rename(val=cols[3]) %>%
      dplyr::arrange(site.ind, year.ind)
    list.number  = length(Xp)+1
    Xp[[list.number]] <-
      make_mat(df.in=xpdf,
               row="site.ind",
               col="year.ind",
               val="val")
    ##test rownames
    names(Xp)[list.number] <- Xp.names[j]
  }#end Xp j-loop

  stopifnot(all(as.integer(rownames(Xp[[list.number]]))==sort(site.index$site.ind)))
  stopifnot(all(as.integer(colnames(Xp[[list.number]]))==sort(year.index$year.ind)))
  stopifnot(dim(Xp[[1]])[1]==dim(C)[1])

## Max N for JAGAM ---------------------------------------------------------

maxN <- rbind(maxN,
              rbind(df %>%
                      dplyr::group_by(grid.ind) %>%
                      dplyr::filter(!is.na(c)) %>%
                      dplyr::summarise(N.max = max(c, na.rm=TRUE)), maxN)
         )
## Indexing: Make list of indexes ------------------------------------------
# Loop indexes for JAGS
indexing <- make_indexing_df(X=df)

## BBS specific matrices -------------------------------------------------------
if(ind == "bbs"){
  temp.bbs <-
    df %>% filter(!is.na(c)) # to ensure we remove the NA rteno

  nGridsBySiteByYear <- temp.bbs %>%
    dplyr::group_by(site.ind, year.ind) %>%
    dplyr::mutate(n=n_distinct(grid.ind)) %>%
    dplyr::distinct(n, year.ind, site.ind) %>%
          reshape2::acast(
            site.ind~year.ind, value.var = "n")
  nGridsBySiteByYear[is.na(nGridsBySiteByYear)] <- 0

  #create an array with dimensions [nRoutes by nYears by nMaxNumberGridsASingleRteFallsInto (nMaxGrid)]
  nMaxGrid <- max((temp.bbs %>%
                      distinct(grid.ind, site.ind, year.ind) %>%
                      group_by(site.ind, year.ind) %>%
                      summarise(n=n_distinct(grid.ind)))["n"], na.rm=TRUE) # index used in JAGS
  idsGridsbySiteYear <- temp.bbs %>%
      distinct(grid.ind, site.ind, year.ind) %>%
      group_by(site.ind, year.ind) %>%
      mutate(ng = 1:n()) %>%
      arrange(grid.ind) %>%
      reshape2::acast(site.ind~year.ind~nMaxGrid, value.var = "grid.ind")

  ## Grab max values for BBS in each grid cell for use in JAGAM
    maxN <- rbind(df %>%
                       dplyr::group_by(grid.ind) %>%
                        dplyr::filter(!is.na(c)) %>%
                        dplyr::summarise(N.max = max(c, na.rm=TRUE)), maxN)


    ## add these indexes to bbs$indexing
    # indexing$maxn <- maxN
    indexing$gridsiteyear.id <- idsGridsbySiteYear
    indexing$ngridsiteyear <- nGridsBySiteByYear
    indexing$nmaxgrids <- nMaxGrid


} # end if BBS


# LOOP OUTPUT ------------------------------------------------------------------
## create the list of elements
objs.in <- objs[objs %in% ls()] %>% as.vector()
list.out <- vector(mode='list', length=length(objs.in))
names(list.out) <- objs.in
for (z in seq_along(objs.in)) {
  new = eval(parse(text = objs.in[z]))# this is necessary for some reason idk why
  list.out[[objs.in[z]]] <- new
}
if(ind=="ebird") ebird.list <- list.out
if(ind=="bbs")   bbs.list <- list.out
if(ind=="grid")  grid.list <- list.out
#remove all objects to be sure they arent put into other lists
suppressWarnings(rm(list=c(objs)))
rm(list.out)
} # END EBIRD AND BBS LOOPS


# Create JAGAM Data ---------------------------------------------------
# grab max values of N across ebird and bbs for each grid cell
    maxN.all <- maxN %>%
      dplyr::group_by(grid.ind) %>%
      dplyr::filter(N.max == max(N.max, na.rm = TRUE)) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()
    maxN.all <- rbind(maxN.all, cbind(
      N.max = 0,
      grid.ind = setdiff(grid.index$grid.ind, maxN.all$grid.ind)
    )) %>%
      dplyr::arrange(grid.ind)

## just to ensure its sorted
temp=grid.index %>%
  dplyr::arrange(grid.ind)

## bundle data for use in jagam
jagam.data <- data.frame(X = temp$X,
                         Y = temp$Y,
                         N = maxN.all$N.max)

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

gam.list$jags.fn <-  gam.fn
cat("GAM jags model specification saved:\n\t", gam.fn,"\n\n")

# Create Return Object ----------------------------------------------------
objs.out <- c("ebird.list", "bbs.list", "grid.list", "gam.list")
names    <- c("ebird",      "bbs",      "grid",      "gam")
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
list.out$metadata <- jdat.contents

# Export to file ----------------------------------------------------------
cat("Saving output to file. This may take a minute or two depending on size of eBird data:\n\t", fn)
saveRDS(list.out, file = fn)
cat("\t....done saving\n")
# export obj from function
return(list.out)


# END FUN -----------------------------------------------------------------

} # END FUN
