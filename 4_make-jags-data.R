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


# Grid/study area ---------------------------------------------------------
# XY: centroid coords for grid cells in study area
XY <- cbind(grid$cell.lon.centroid, grid$cell.lat.centroid)
# nSites: num. grid cells in study area
nSites <- nrow(XY)


# BBS data ---------------------------------------------------------
##coerce the bbs data to a data frame.
bbs.df <- bbs %>% st_drop_geometry() %>%
  as.data.frame() %>%
  na.omit(rteno, year) %>%
  distinct(rteno, year, id,.keep_all=TRUE)

## create and scale covariates
  ### z-scale wind
bbs.df$avgwind.z <- (bbs.df$avgwind-mean(bbs$avgwind, na.rm=TRUE))/sd(bbs.df$avgwind, na.rm=TRUE)
#### paige uses Xp and wind.z so just keeping this for now will clean up later.
Xp <- wind.z <- pivot_wider(bbs.df %>% distinct(year, rteno, avgwind.z),
                      id_cols = year, names_from = rteno,
                      values_from = "avgwind.z")

#### cols in paige's bbs.jags.NY
# [1] "BBSr"              "bbs_counts_annual" "propStops"         "wind.z"            "bbs_wind_annual"
# [6] "nov"               "Brsite"            "nRoutes"           "Brgrid"            "nGridswithRoutes"
# BBSr: count data matrix (year by rteno)
BBSr <- bbs.df %>%
  distinct(year, rteno, C) %>%
  pivot_wider(id_cols = year,
              names_from = rteno,
              values_from = "C")


# eBird for jags ---------------------------------------------------------
##coerce the ebird data to a data frame.
ebird.df <- ebird %>% st_drop_geometry() %>%
  as.data.frame() %>%
  distinct(checklist_id, year, id,.keep_all=TRUE)

# E: array (grid cell id by year, slice=checklist_id); counts (C) from eBird
E <- ebird.df %>%
  acast(id~year~checklist_id,
        value.var="C")
gc()

# nSitesSurveyed: vector (length: num years) number of grid cells containing eBird checklists per year
ebird.df %>% distinct()
# nSitesSurveyed = ebird.jags.NY$nSitesSurveyed, # Vector (nyears). Number of
# # gridcells with eBird checklists each year
# nChecklist = ebird.jags.NY$nChecklist,         # Matrix (nyears x max number
# # of grid cells surveyed each year). Number of checklists within each grid
# # cell, each year
# Esite = ebird.jags.NY$Esite,  # Matrix (nyears x max number of grid cells
# # surveyed each year). Identity of checklists within each grid cell, each year
# nYears = ncol(ebird.jags.NY$ebird_counts), # Scalar indicating number of years
# # with BBS route level data
#
# # Ebird checklist covariates. All numeric variables are scaled to mean 0, unit
# # variance.
# # All arrays with dim = ngridcells x nyears x nchecklists
# traveling = ebird.jags.NY$traveling, # Protocol type. stationary = 0,
# # traveling = 1.
# hours = ebird.jags.NY$hours,           # Duration of survey (hrs)
# distance = ebird.jags.NY$distance,     # Distance traveled (km)
# groupsize = ebird.jags.NY$group,       # Group size
# day = ebird.jags.NY$day,               # Day of year
# time = ebird.jags.NY$time,             # Time of day
# score = ebird.jags.NY$score,           # Observer CCI
# offroad = ebird.jags.NY$offroad        # Roadside = 0, off-road survey = 1
# )


# TO FIGURE OUT -----------------------------------------------------------

# Brsite: matrix (year by rteno) value= ?????
# Brsite <- reshape2::acast(bbs.df,
# year ~ rteno,
# value.var = "WHAT?!"
# )

# Brsite = bbs.jags.NY$Brsite,      # Matrix (nyears x nroutes) indicating
# # routes run in a given year
# Brgrid = bbs.jags.NY$Brgrid,      # Array (nroutes x ngridcells x nyears)
# # Indicates the identity of grid cells that a route crosses each year



# Final JAGS Data ------------------------------------------------
## for my sanity just going to list things out to populate them later
# jags <- list(
#   BBSr = matrix(NA),  # matrix
#   Brsite = NULL,
#   Brgrid = NULL,
#   covar1 = NULL,
#   day = NULL,
#   E = NULL,
#   distance = NULL,
#   groupsize = NULL,
#   hours = NULL,
#   nChecklist = NULL,
#   nGridwithRoutes = NULL,
#   nov = NULL,
#   nRoutes = col_integer(),
#   nYearsB = col_integer(),
#   nSites = col_integer(), # number of grid cells in study area
#   nSitesSurveyed = NULL,
#   offroad = NULL,
#   propStops = NULL,
#   time = NULL,
#   traveling = NULL, # binary/logical (0=stationary protocol, 1=traveling/moving protocol)
#   Xp = matrix(NA),
#   XY = data.frame(x=NULL, y=NULL, layer=NULL, covar1=NULL)
# )
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
#
## this is wrong need to check obsfirstyearroute vals
# nov = reshape2::acast(bbs.df,
#                       year ~ rteno,
#                       fun.aggregate = get_mode,
#                       value.var="obsfirstyearroute") # matrix (year by rteno) indicating if observer's first year on route OR bbs (tbd)

# nov: matrix (year by rteno) indicating if observer's first year on route OR bbs (tbd)
nov <- bbs.df %>%
  distinct(year, rteno, obsfirstyearbbs) %>%
  pivot_wider(id_cols = year,
              names_from = rteno,
              values_from = obsfirstyearbbs) %>%
  select(-year)

# propStops: array (year by rteno, slice = grid cell)
# propStops: proportion of each route that falls inside a grid cell
### paige's propstops is also filled with NAs, but I am confident this is not what is meant to be here.
### need to check up later
propStops <- bbs.df %>%
  reshape2::acast(year~rteno~id,
                  value.var="proprouteincell"
                  )

# nRoutes = vector (number of unique rteno run per year)
nRoutes <- (bbs.df %>%
              group_by(year) %>%
              summarise(nRoutes=n_distinct(rteno)))$nRoutes
assertthat::are_equal(nRoutes %>% length(), nrow(BBSr))##test

# nYearsB: scalar for n years of BBS data
nYearsB <- nrow(BBSr)


# nGridswithRoutes: matrix (rteno by year) number of grid cells each rteno passes through per year
nGridswithRoutes <- bbs.df %>%
  distinct(year, rteno, id) %>%
  group_by(year, rteno) %>%
  summarise(ncellscrossedperyear = n_distinct(id)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = rteno,
    names_from = year,
    values_from=ncellscrossedperyear,
    values_fill = 0
    ) %>%
  select(-rteno)


