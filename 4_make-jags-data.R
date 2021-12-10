if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("0_setup.r")
devtools::load_all()

# Create or Load in Post-spatial Munging BBS and eBird Data ----------------------------
fns <- c("bbs.rds", "ebird.rds", "grid.rds")

if(all(fns %in% list.files(dir.jags))){
  cat("importing the munged bbs, ebird and grid sf objects.")
  fns=list.files(dir.jags, full.names = TRUE)
  bbs <- readRDS(fns[str_detect(fns, "bbs")])
  grid <- readRDS(fns[str_detect(fns, "grid")])
  ebird <- readRDS(fns[str_detect(fns, "ebird")])
}else{
  cat("Building the BBS and eBird data and spatial objects. Sit back and relax......\nor be worried for errors halfway through")
  source("3_make-bird-data.R")
}


# Munge BBS for jags ---------------------------------------------------------
## first, create and scale as necessary.




# Munge eBird for jags ---------------------------------------------------------


# Create JAGS data ------------------------------------------------
## for my sanity just going to list things out to populate them later
jags <- list(
  BBSr = matrix(NA),  # matrix
  Brsite = NULL,
  Brgrid = NULL,
  covar1 = NULL,
  day = NULL,
  E = NULL,
  distance = NULL,
  groupsize = NULL,
  hours = NULL,
  nChecklist = NULL,
  nGridwithRoutes = NULL,
  nov = NULL,
  nRoutes = col_integer(),
  nYearsB = col_integer(),
  nSites = col_integer(), # number of grid cells in study area
  nSitesSurveyed = NULL,
  offroad = NULL,
  propStops = NULL,
  time = NULL,
  traveling = NULL, # binary/logical (0=stationary protocol, 1=traveling/moving protocol)
  Xp = matrix(NA),
  XY = data.frame(x=NULL, y=NULL, layer=NULL, covar1=NULL)
)
# PAIGE'S JDAT
#   nSites = nrow(XY), # Number of grid cells in study area
#
#   ## Doublecheck that phab is in the correct order... ##
#   # Proportion of grid cell covered by freshwater. Scaled to mean 0, unit
#   # variance.
#   #pwet = XY.filt[,"pwet"],    # Matrix (ngridCells x nYears).
#
#   # Vector (nSites).  Proportion of grid cell covered by freshwater.
#   # Scaled to mean 0, unit variance.
#   phab = XY[,"phab"],
#
#   ## BBS route level data ##
#   BBSr = bbs.jags.NY$BBSr, # Matrix (nyears x nroutes) with DCCO count data for
#   # each route, each year
#   Xp = bbs.jags.NY$wind.z, # Matrix (nyears x nroutes) with wind data for each
#   # route x year
#   nov = bbs.jags.NY$nov,   # Matrix (nyears x nroutes) with indicator for
#   # whether an observer was brand new in a given year (1) or not (0) data for
#   # each route x year
#   propStops = bbs.jags.NY$propStops, # Array (nyears x nroutes x ngridcells)
#   # with effort data for each route x year combination. This is the proportion
#   # of a route that falls within each of our grid cells
#
#   nYearsB = nrow(bbs.jags.NY$BBSr), # Scalar indicating number of years with BBS data
#   Brsite = bbs.jags.NY$Brsite,      # Matrix (nyears x nroutes) indicating
#   # routes run in a given year
#   nRoutes = bbs.jags.NY$nRoutes,    # Vector (nyears) indicating total number of
#   # routes run each year
#   Brgrid = bbs.jags.NY$Brgrid,      # Array (nroutes x ngridcells x nyears)
#   # Indicates the identity of grid cells that a route crosses each year
#   nGridswithRoutes = bbs.jags.NY$nGridswithRoutes, # Matrix (nroutes x nyears)
#   # Indicates maximum number of grid cells that a route crosses each year
#
#   ## eBird Data ##
#   E = ebird.jags.NY$ebird_counts, # Array of DCCO ebird data (nGridcells x nyears
#   # x nchecklists)
#   nSitesSurveyed = ebird.jags.NY$nSitesSurveyed, # Vector (nyears). Number of
#   # gridcells with eBird checklists each year
#   nChecklist = ebird.jags.NY$nChecklist,         # Matrix (nyears x max number
#   # of grid cells surveyed each year). Number of checklists within each grid
#   # cell, each year
#   Esite = ebird.jags.NY$Esite,  # Matrix (nyears x max number of grid cells
#   # surveyed each year). Identity of checklists within each grid cell, each year
#   nYears = ncol(ebird.jags.NY$ebird_counts), # Scalar indicating number of years
#   # with BBS route level data
#
#   # Ebird checklist covariates. All numeric variables are scaled to mean 0, unit
#   # variance.
#   # All arrays with dim = ngridcells x nyears x nchecklists
#   traveling = ebird.jags.NY$traveling, # Protocol type. stationary = 0,
#   # traveling = 1.
#   hours = ebird.jags.NY$hours,           # Duration of survey (hrs)
#   distance = ebird.jags.NY$distance,     # Distance traveled (km)
#   groupsize = ebird.jags.NY$group,       # Group size
#   day = ebird.jags.NY$day,               # Day of year
#   time = ebird.jags.NY$time,             # Time of day
#   score = ebird.jags.NY$score,           # Observer CCI
#   offroad = ebird.jags.NY$offroad        # Roadside = 0, off-road survey = 1
# )
#




## munge bbs for jags ------------------------------------------------------
names(bbs)

BBSr <- acast(bbs, Year~ID, value.var="C",
            # Because a route could be split into multiple grid cells. BUT the
            # total count is applicable at the route level, not the route
            # segment level.
            fun.aggregate=Mode,
            drop=FALSE) # Keep year / route combinations that didn't occur




## munge ebird for jags -----------------------------------------------------
names(ebird)


