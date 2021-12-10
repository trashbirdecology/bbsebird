if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("0_setup.r")
devtools::load_all()

# Create or Load in Post-spatial Munging BBS and eBird Data ----------------------------
fns <- c("bbs.rds", "ebird.rds", "grid.rds")

if(all(fns %in% list.files(dir.jags))){
  cat("importing the munged bbs, ebird and grid sf objects.")
  fns=list.files(dir.jags, full.names = TRUE)
  bbs <- readRDS(fns[str_detect(fns, "bbs_spat")])
  grid <- readRDS(fns[str_detect(fns, "grid")])
  ebird <- readRDS(fns[str_detect(fns, "ebird_spat")])
}else{
  cat("Building the BBS and eBird data and spatial objects. Sit back and relax......\nor be worried for errors halfway through")
  source("3_make-bird-data.R")
}


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


