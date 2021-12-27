if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("0_setup.r")
devtools::load_all()

# Create or Load in Post-spatial Munging BBS and eBird Data ----------------------------
fns <- c("bbs.rds", "ebird.rds", "grid.rds")

if(all(fns %in% list.files(dir.jags))){
  cat("importing the munged bbs, ebird and grid sf objects.")
  fns=list.files(dir.jags, full.names = TRUE)
  bbs <- readRDS(fns[str_detect(fns, "bbs.rds")])
  grid <- readRDS(fns[str_detect(fns, "grid.rds")])
  ebird <- readRDS(fns[str_detect(fns, "ebird.rds")])
}else{
  cat("Building the BBS and eBird data and spatial objects. Sit back and relax......\nor be worried for errors halfway through")
  source("3_make-bird-data.R")
}


# BBS data ---------------------------------------------------------
##coerce the bbs data to a data frame and munge a little
bbs.df <- bbs %>% st_drop_geometry() %>%
  as.data.frame() %>%
  ### remove empty grid cells..
  distinct(rteno, gridcellid, year, C, .keep_all=TRUE) %>%
  filter(!is.na(rteno)) %>%
  ### scale covariates
  mutate(
    wind.z = (avgwind - mean(avgwind, na.rm=TRUE))/sd(avgwind, na.rm=TRUE),
    noise.z = (noisemean - mean(noisemean, na.rm=TRUE))/sd(noisemean, na.rm=TRUE),
    car.z = (carmean - mean(carmean, na.rm=TRUE))/sd(carmean, na.rm=TRUE)
  ) %>%
  ### weighted counts by prop route in cell and cell area
  mutate(C.weighted.area = as.numeric(C*proprouteincell/cellarea)) # weighted C per grid area (# per m^2)


## Count data---arrays for BBS count data (dims: rteno by year by grid cell)
### C: array <rteno by year by cell id>
C            <-   acast(bbs.df, rteno~year~gridcellid, value.var="C")
### C.route: C at the route level
C.route      <-   acast(bbs.df %>% distinct(rteno, year, C), rteno~year, value.var="C")


## Adjusted count data---arrays for "effort" adjusted BBS count data
### C.adj: weighted count (birds per m^2);  == C*proprouteincell/cellarea
# C.adj        <- acast(bbs.df, rteno~year~gridcellid, value.var="C.weighted.area")
### C.adj.route: C.adj at the route-level (i.e. just dropping the grid)
# C.adj.route  <- acast(bbs.df %>% distinct(rteno, year, C.weighted.area), rteno~year, value.var="C.weighted.area")


## Weight-prop---array containing weights for count data (dims: rteno by grid cell)
# w.prop <- acast(bbs.df, rteno~year~gridcellid, value.var="proprouteincell") # % of rteno in a cell
w.prop <- acast(bbs.df %>% distinct(rteno, gridcellid, proprouteincell), rteno~gridcellid, value.var="proprouteincell") # % of rteno in a cell

## Area offset--vector containing area (m^2) for each grid id (dims: grid cell by 1)
w.area <- acast(bbs.df %>% distinct(gridcellid, cellarea), gridcellid~., value.var="cellarea") # area (units in original df) of gri cell.
w.area <- scale(as.vector(w.area))


### Observation covariates (p.XXXX)---observation process covariates (dims: rteno by year)
p.cars <-  acast(bbs.df %>% distinct(rteno, year, car.z), rteno~year)
p.noise <-  acast(bbs.df %>% distinct(rteno, year, noise.z), rteno~year)
p.obsfyrbbs <-  acast(bbs.df %>% distinct(rteno, year, obsfirstyearbbs), rteno~year)
p.obsfyrrteno <-  acast(bbs.df %>% distinct(rteno, year, obsfirstyearroute), rteno~year)
p.wind <-  acast(bbs.df %>% distinct(rteno, year, wind.z), rteno~year)

## Trend effects
doy.bbs <-  acast(bbs.df %>% distinct(rteno, year, yday), rteno~year)

## Indexing
### unique rtenos sampled
id.routessampled <- unique(bbs.df$rteno) %>% sort()
n.routessampled  <- length(id.routessampled)
### number of RTENO sampled each year
n.routesperyear <- bbs.df %>% group_by(year) %>%
  summarise(n = n_distinct(rteno)) %>% arrange(year) %>% ungroup()
n.routesperyear <- n.routesperyear$n


### for loops
R = dim(C)[1] # <route> number of unique BBS routes with data
T.bbs = dim(C)[2] # <time> number of years of BBS data
G.bbs = dim(C)[3] # <site/grid cell> number of unique grid cells sampled by BBS data


## jdat.bbs -------------------------------------------------------------------------
T = length(min(c(ebird.df$year, bbs.df$year)):max(c(bbs.df$year, ebird.df$year))) # <years> total number of years
names.bbs <-
  c(
    "R",
    "G",
    "T",
    "T.bbs", # will likely always equal T, but just in case
    "G.bbs",
    "C",
    "C.route",
    "id.routessampled",
    "n.routessampled",
    "n.routesperyear",
    "w.area",
    "w.prop",
    "p.obsfyrbbs",
    "p.obsfyrrteno",
    "p.cars",
    "p.noise",
    "p.wind",
    "doy.bbs"
  )
jdat.bbs <- list()
## Pack up the BBS data for use in JAGS
for(i in seq_along(names.bbs)){
  jdat.bbs[[i]] <- eval(parse(text=paste(names.bbs[i])))
  names(jdat.bbs)[[i]] <- names.bbs[i] # doing this inside loop to prevent issues where data DNE
}

# # check dimensions on objects
# lapply(jdat.bbs, ncol) %>% unlist()
# lapply(jdat.bbs, nrow) %>% unlist()
# test <- list()
# for(i in seq_along(jdat.bbs)){
#   temp=dim(jdat.bbs[[i]])
#   if(length(temp)>1){test[[i]] = temp
#   }else{test[[i]]= NA}
# }
# test


# eBird data --------------------------------------------------------------
ebird.df <- ebird %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  distinct() %>% ## not sur why there are d to check this upstream later!!!
  filter(number_observers < 10) %>% ### some strange number of observers exist... going to limit to 10 observers arbitrarily
  ### scale covariates
  mutate(
    duration_minutes.z = (duration_minutes - mean(duration_minutes, na.rm=TRUE))/sd(duration_minutes, na.rm=TRUE),
    effort_area.z = (effort_area_ha - mean(effort_area_ha, na.rm=TRUE))/sd(effort_area_ha, na.rm=TRUE),
    effort_distance_km.z = (effort_distance_km - mean(effort_distance_km, na.rm=TRUE))/sd(effort_distance_km, na.rm=TRUE),
    number_observers.z = (number_observers - mean(number_observers, na.rm=TRUE))/sd(number_observers, na.rm=TRUE)
  )


## jdat.ebird -------------------------------------------------------------------------
jdat.ebird <- list() ## keep empty if nothing yet



# Grid/study area ---------------------------------------------------------
## create a data frame for indexing off the grid cell
grid.temp <- grid %>%
  arrange(gridcellid) %>%
  st_drop_geometry()

### create an index for the grid cells with count data
gridcellswbbs <- unique(bbs.df$gridcellid) %>% sort()
gridcellswebird <- unique(ebird.df$gridcellid) %>% sort()
gridcellswdata <- unique(c(gridcellswbbs, gridcellswebird))


## grid cell coordinates
XY <- grid.temp %>% # centroid coordinates of the grid cells
  select(cell.lon.centroid, cell.lat.centroid) %>% as.matrix()

## jdat.grid --------------------
### create a list of elements relevant to the study area/grid cells
jdat.grid <- list(
  coords.gridcells = XY,
  G = nrow(XY), # integer, number of cells in study area (with or without data)
  id.gridcells = sort(grid.temp$gridcellid), # identifier for grid cells
  n.gridcellswdata = length(gridcellswdata), # number of grid cells with any count data (ebird or bbs)
  id.gridcellswdata = sort(gridcellswdata),  # vector of grid cell ids with any count data
  n.gridcellswbbs = length(gridcellswbbs), # number of grid cells with any count data (ebird or bbs)
  id.gridcellswbbs = sort(gridcellswbbs),  # vector of grid cell ids with any count data
  n.gridcellswebird = length(gridcellswebird), # number of grid cells with any count data (ebird or bbs)
  id.gridcellswebird = sort(gridcellswebird)  # vector of grid cell ids with any count data
  )


# Other Indexing -----------------------------------------------------------------

# The Integrated JAGS Data ---------------------------------------------------------------------
jdat <- c(jdat.bbs, jdat.ebird, jdat.grid)
## save the data for andy to use/check
saveRDS(jdat, file=paste0(here::here("jdat-jar.rds")))

# Some Random Plots -----------------------------------------------------------------
plot(grid[2])
bbs %>% group_by(gridcellid) %>% summarise(max_C_bbs= max(C)) -> temp
plot(temp["max_C_bbs"])
ebird %>% filter(year==2019) %>% group_by(gridcellid) %>% summarise(max_C_ebird_yr2019= max(C)) -> temp
plot(temp["max_C_ebird_yr2019"])


# Notes -------------------------------------------------------------------
## Important 1: access arrays C, coverage, car, noise, wind, ydays, by
## Important 1: calling array[nrow, ncol, nslice]
## Important 1: e.g., to access the first array element, call `array[,,1]`
## Important 2: remember that JAGS uses PRECISION and NOT VARIANCE paramters in distributions
## Important 2: e.g., dnorm(0,.001) means precision==0.001 and variance ==1,000!!!



# Clear mem ---------------------------------------------------------------------
# args.save <- c(args.save, "jdat")
# rm(list=setdiff(ls(), args.save))
#
