#' @title Bundle Data for Use in JAGS (LONG format)
#'
#' @description An updated version of bundle_data, where site by year elements are provided in long format
#' @param bbs BBS data
#' @param ebird eBird data
#' @param grid spatial sampling grid/study area
#' @param scale.covs logical if TRUE will automatically scale the numeric/integer covariates.
#' @param K the maximum number of basis functions that JAGAM will produce. Used for code development purposes, mostly. Do not change unless you know what you're doing.
#' @param X variable name associated with the x-coordinate (e.g., long, longitude, Easting, X) across 'grid', 'bbs', and 'ebird'
#' @param Y variable name associated with the x-coordinate (e.g., latitude, Northing, Y) across 'grid', 'bbs', and 'ebird'
#' @param cell.id column name(s) of the grid cell identifier
#' @param site.id column name(s) of the site  identifier (e.g., BBS route, eBird checklists)
#' @param year.id column name of the temporal identifier
#' @param obs.id  column name(s) of the observer identifier
#' @param use.ebird.in.gam logical if TRUE will use data across both eBird and BBS observations to create basis functions.
#' @param cell.covs column name(s) of the grid-level covariates
#' @param ENgrid.arg if "max" will use the maximum value of observed birds at each grid cell. Alternatives include "min", "mean".
#' @param site.covs column name(s) of the site-level covariates
#' @importFrom dplyr group_by mutate select distinct arrange filter
#'
#' @export bundle_data

bundle_data <-
  function(bbs,
           ebird,
           grid,
           scale.covs = TRUE,
           use.ebird.in.gam = TRUE,
           ENgrid.arg    = "max",
           X           = "cell.lon.centroid",
           Y           = "cell.lat.centroid",
           cell.id     = "gridcellid",
           year.id     = "year",
           site.id     = c("checklist_id", "rteno"),
           obs.id      = c("obsn", "observer_id"),
           cell.covs   = c("area"),
           site.covs   = c("wind", "noise", "cars", "observ", "minute", "assistant",
                           "obsfirstyearbbs", "obsfirstyearroute",
                           "duration_minutes", "effort_distance_km", "effort_area_ha", "number_observers"),
           K = NULL
           ){
# bbs=bbs_spatial; ebird=ebird_spatial; grid=study_area ## FOR DEV
names(bbs)   <- tolower(names(bbs))
names(ebird) <- tolower(names(ebird))
names(grid)  <- tolower(names(grid))

# Drop Spatial Geometry ---------------------------------------------------
bbs   <- as.data.frame(bbs)
ebird <- as.data.frame(ebird)
grid  <- as.data.frame(grid)


# ARG CHECK ---------------------------------------------------------------
ENgrid.arg <- tolower(ENgrid.arg)
stopifnot(ENgrid.arg %in% c("mean", "max", "min"))


# RENAME VARS FOR CONSISTENT OUTPUT ----------------------------------------------------
L <- list(bbs=bbs, ebird=ebird, grid=grid)
for(i in seq_along(L)){
  n <- ns <- nt <- nc <- no <- nx <- ny <- NULL
### find overlap in things i want to rename
  n  <- names(L[[i]])
  if(any(year.id %in% n))     nt <- data.frame(old = n[n %in% c(year.id)], new = "year.id")
  if(any(obs.id %in% n)) no <- data.frame(old = n[n %in% c(obs.id)], new = "obs.id")
  if(any(site.id %in% n))     ns <- data.frame(old = n[n %in% c(site.id)], new = "site.id")
  if(any(cell.id %in% n))     nc <- data.frame(old = n[n %in% c(cell.id)], new = "cell.id")
  if(any(X %in% n))           nx <- data.frame(old = n[n %in% c(X)], new = "X")
  if(any(Y %in% n))           ny <- data.frame(old = n[n %in% c(Y)], new = "Y")

  nn <- rbind(nt, ns, nc, nx, ny, no)
  if(is.null(nn))next()
  # if(!any(n %in% nn$old))next()
  for(j in 1:nrow(nn)){
    colind <- which(names(L[[i]]) == nn$old[j])
    names(L[[i]])[colind] <- nn$new[j]
  }
  }#end L loop

bbs   <- L$bbs
ebird <- L$ebird
grid  <- L$grid
rm(L)


# Ensure no duplicates exist ----------------------------------------------
bbs   <- bbs %>% distinct(year.id, site.id, cell.id, c, .keep_all = TRUE)
ebird <- ebird %>% distinct(year.id, site.id, cell.id, c, .keep_all = TRUE)

# GLOBAL INDEXES -----------------------------------------------------------------
## STUDY AREA GRID CELL INDEX
cell.index <- grid %>%
  units::drop_units() %>%
  arrange(cell.id) %>%
  distinct(cell.id, X, Y, .keep_all=TRUE) %>%
  # create grid cell id as index and not identity (though they may be the)
  mutate(cell.ind = 1:n())
# keep only useful information....
cell.index <- cell.index[names(cell.index) %in% c("cell.id", "cell.ind", "X", "Y", cell.covs)]
## scale if specified
if (scale.covs) {
  cell.index <- cell.index %>%
    mutate(area = standardize(area)) %>%
    mutate(X  = standardize(X)) %>%
    mutate(Y  = standardize(Y))
}

## YEAR INDEX
year.index <-
  data.frame(year.id = min(c(ebird$year.id, bbs$year.id), na.rm = TRUE):max(c(ebird$year.id, bbs$year.id), na.rm =
                                                                     TRUE)) %>% mutate(year.ind = 1:n())
# ### YEAR-GRID INDEX
# # make a version of all years and grid index combinations, regardless data availability.
# # this is used here, internally, but can also be called in certain jags model components
yg.index <-
  expand.grid(year.ind = year.index$year.ind,
              cell.ind = cell.index$cell.ind) %>%
  merge(year.index) %>%
  merge(cell.index) %>%
  dplyr::select(cell.ind, cell.id, year.ind, year.id)

# BBS-EBIRD INDEXES ---------------------------------------------------------------
## for bbs and ebird data, create site (route, checklist_id) and observer indexes
LL <- list(bbs=bbs, ebird=ebird)
for(i in seq_along(LL)){
  ## first, drop grid-level covariates and metadata since it iwll be stored there
  LL[[i]] <- LL[[i]][!names(LL[[i]]) %in% c("X", "Y", cell.covs)]
  ## second, drop NA count values, since we don't want to include non-sampled site-year combinations
  LL[[i]] <- LL[[i]] %>%
    units::drop_units() %>%
    filter(!is.na(site.id), !is.na(c))
  ## create index each for SITE and OBSERVER, then append to the data frame
  LL[[i]] <-
    LL[[i]] %>%
    mutate(obs.ind  = obs.id  %>% as.factor() %>% as.numeric()) %>%
    mutate(site.ind = site.id %>% as.factor() %>% as.numeric())
  suppressMessages( LL[[i]] <- left_join(LL[[i]], yg.index))
  # suppressMessages( LL[[i]] <- full_join(LL[[i]], yg.index))
  ## finally, drop the NA observations on bbs and ebird..
  LL[[i]] <-     LL[[i]][!is.na(LL[[i]]$site.ind),]
} # end LL loop

##extract from list
bbs   <- LL$bbs
ebird <- LL$ebird
rm(LL)


# BBS-SPECIFIC DATA CREATE MATRIX PROP (% site.ind in cell.ind) ----------------------------------------------------
stopifnot(nrow(bbs %>% distinct(site.ind, cell.ind, proprouteincell))==nrow(bbs %>% distinct(site.ind, cell.ind)))
## grab all cell ids and site inds
prop <- bbs %>%
  distinct(site.ind, cell.ind, proprouteincell) %>%
  full_join(cell.index %>% dplyr::select(cell.ind))
prop$prop[is.na(prop$prop)] <- 0  ##supply prop with zero if route
prop <-
  reshape2::acast(prop,
                  site.ind ~ cell.ind,
                  value.var = "prop",
                  fill = 0)
# remove the rownames where NA
prop <-
  prop[-which(rownames(prop) %in% c(NA, "NA")), ]

# COVARIATES --------------------------------------------------------------
## create arrays for covariates
### dimensions are n.sites by n.years
LL    <- list(bbs=bbs,  ebird=ebird)
Xsite <- list(bbs=NULL, ebird=NULL)
cat("creating site-level covariate matrices....\n")
for(i in seq_along(LL)){
  temp <- site.covs[site.covs %in% names(LL[[i]])]
  if(length(temp)==0) next()
  # remove the extra data (for routes w/>1 grid cell)
  LL[[i]] <- LL[[i]] %>% distinct(site.ind, year.ind, .keep_all = TRUE)
  for(j in seq_along(temp)){
    cov.name  <- temp[j]

    cov.dat   <- data.frame(as.vector(LL[[i]][cov.name]),
                            LL[[i]]["site.ind"],
                            LL[[i]]["year.ind"])
    ## scale the covariate if scale.covs==TRUE
    names(cov.dat)[1] <- "cov"

    is.binary <- ifelse(max(cov.dat$cov, na.rm=TRUE) > 1, FALSE, TRUE)
    if(is.binary)cat("site-level covariate '",cov.name,"' is binary and was not standardized.\n\n", sep = "")
    if (scale.covs & !is.binary) {cov.dat$cov <- standardize(cov.dat$cov)}
    cov.mat  <-  reshape2::acast(cov.dat,
                                 site.ind ~ year.ind,
                                 value.var = "cov",
                                 fill = NA)
    ## to ensure rownames and colnames match the indexes..
    stopifnot(all(as.integer(rownames(cov.mat))==as.integer(sort(unique(cov.dat$site.ind)))))
    stopifnot(all(as.integer(colnames(cov.mat))==as.integer(sort(unique(cov.dat$year.ind)))))
    Xsite[[i]][[j]] <- cov.mat
    names(Xsite[[i]])[j] <- cov.name
  }
}# end Xsite loops
rm(LL)

# GRID-LEVEL COVARIATES ---------------------------------------------------
## create arrays for grid cell-level covariates
### dimensions are ngrid by nyear
Xgrid <- list(NULL)

cell.index <- cell.index %>% arrange(cell.ind)
cell.covs <- cell.covs[cell.covs %in% colnames(cell.index)]
if(!is.null(cell.covs)){
  for(i in seq_along(cell.covs)){
  Xgrid[[i]] <- cell.index[,cell.covs[i]]
  names(Xgrid)[i] <- cell.covs[i]
  }# end Xgrid loop
}# end if cell.covs


# JAGAM -------------------------------------------------------------
# create basis functions and data for GAM model components
## first, we need to grab the maximum number of birds per grid cell/year
## argument use.ebird.in.gam lets user ignore the eBird data when producing the GAM data
## this is important when modeling only the BBS data, as eBird observations
## are typically >>> BBS observations for some (many?) species.
if(use.ebird.in.gam){
  ENgrid <- rbind(
    bbs %>% dplyr::distinct(cell.ind, year.ind, c),
    ebird %>% dplyr::distinct(cell.ind, year.ind, c))
}else{
  ENgrid <-  bbs %>% dplyr::distinct(cell.ind, year.ind, c)

}
# evaluate according to specified argument.
### id prefer to just evaluate the argument inside the
### the filter call, but not sure rn how to do that.
### using filter(c==eval(parse(text=paste0(ENgrid.arg,"(c,na.rm=TRUE)")))))
### did not work. Nor does bang bang !!ENgrid.arg(c,na.rm=TRUE)
### have yet to test out !!rlang::sym(Engrid.arg)(c,na.rm=TRUE)..

if(ENgrid.arg == "max"){ENgrid <- ENgrid %>%
  dplyr::group_by(cell.ind, year.ind) %>%
  dplyr::filter(c == max(c, na.rm=TRUE)) %>%
  dplyr::ungroup()
}
if(ENgrid.arg == "mean"){ENgrid <- ENgrid %>%
  dplyr::group_by(cell.ind, year.ind) %>%
  dplyr::filter(c == mean(c, na.rm=TRUE)) %>%
  dplyr::ungroup()
}
if(ENgrid.arg == "min"){ENgrid <- ENgrid %>%
  dplyr::group_by(cell.ind, year.ind) %>%
  dplyr::filter(c == min(c, na.rm=TRUE)) %>%
  dplyr::ungroup()
}

## next, add the missing grid cells and fill in with grand MEAN
gy.all <- expand.grid(cell.ind=cell.index$cell.ind,
                      year.ind=year.index$year.ind)
ENgrid <- full_join(gy.all, ENgrid)
ENgrid$c[is.na(ENgrid$c)] <- round(mean(ENgrid$c, na.rm=TRUE), 0)

# if not specified, K is defined as:
if (is.null(K))
  K <- min(max(20, length(unique(
    ENgrid$cell.ind
  ))), 150)

# create data for use in jagam
jagam.in <- data.frame(merge(ENgrid, cell.index))
jagam.in <- jagam.in %>% group_by(cell.ind) %>% filter(c==max(c, na.rm=TRUE)) %>% ungroup() %>%
  distinct(cell.ind, .keep_all = TRUE)

jagam.fn <- paste0(dirs$dir.models, "/gam-UNEDITED.txt")
jagam.mod <- mgcv::jagam(
      c ~ s(
        X,
        Y,
        bs = "ds",
        k = K,
        m = c(1, 0.5)
      ),
      file = jagam.fn,
      sp.prior = "log.uniform",
      data = jagam.in,
      diagonalize = TRUE,
      # parallell = TRUE,
      # modules = "glm"
      family = "poisson"
    )

jagam.mod$fn <- jagam.fn

## make a matrix of ENgrid for use in JAGS model as Exp. num in grid
ENgrid.mat =  reshape2::acast(jagam.in,
                            cell.ind ~ year.ind,
                            value.var = "c",
                            fill = 0)

# BUNDLE DATA -------------------------------------------------------------
jdat <- list(
  # "all" data
  bbs.df     = bbs %>% distinct(year.ind, site.ind, .keep_all=TRUE),
  ebird.df   = ebird %>% distinct(year.ind, site.ind, .keep_all=TRUE),
  grid.df    = grid,
  # max C per grid per year  (zero-filled)
  ENgrid       = ENgrid,
  ENgrid.mat   = ENgrid.mat,
  # covariates
  Xsite      = Xsite,
  Xgrid      = Xgrid,
  # indexes created
  prop       = prop,                                # % BBS route per grid cell (dims <ngrid nroutes>)
  cell.index = cell.index,                          # lookup table for grid cells
  year.index = year.index,                          # lookup table for year
  # create indexes here
  T          = length(unique(year.index$year.ind)), # number of years across all data
  G          = length(unique(cell.index$cell.id)),  # number of grid cells in study area
  Mb         = length(unique(bbs$site.id)),         # number of routes bbs
  Me         = length(unique(ebird$site.id)),       # number of checklists bbs
  gy         = gy.all,
  ngy        = nrow(gy.all),
  ## more indexes
  GTb        = bbs   %>% dplyr::distinct(cell.ind, year.ind) %>% dplyr::arrange(cell.ind, year.ind), # grid-years sampled bbs
  GTe        = ebird %>% dplyr::distinct(cell.ind, year.ind) %>% dplyr::arrange(cell.ind, year.ind), # grid-years sampled ebird
  # all JAGAM output
  jags.mod   = jagam.mod,
  Z          = jagam.mod$jags.data$X,               # dims <ncells  by nknots>
  nbfs       = dim(jagam.mod$jags.data$X)[2]        # number of basis functions/knots
)




# RETURNED OBJECT ---------------------------------------------------------
stopifnot(!any(is.na(jdat$bbs.df$c)))
stopifnot(!any(is.na(jdat$ebird.df$c)))

return(jdat)

} # end function
