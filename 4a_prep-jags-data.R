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
  )

n.years.bbs <- length(unique(bbs.df$year))

## create array of BBS count data sliced by RTENO
# C.bbs.array: array <cell id by year sliced by BBS route num>
# C.bbs.array cont.: C for BBS comprises count data at the route-level (sum of 50-stops)
# C.bbs.array cont.: has rows for all grid cells that have at least some BBS data, but not necesssarily any counts for that RTENO.
C.bbs.array <- acast(bbs.df, gridcellid~as.integer(year)~rteno, value.var="C")
ind <- assertthat::are_equal(dim(C.bbs.array)[3],
                      length(unique(bbs.df$rteno)))
if(ind) bbs.rtenos <- sort(unique(bbs.df$rteno)); rm(ind)
## create single data frame of BBS count data per RTENO and year
# C.bbs.df: array <cell id by year sliced by BBS route num>
# C.bbs.df cont.: C for BBS comprises count data at the route-level (sum of 50-stops)
# C.bbs.df cont.: has rows for all grid cells that have at least some BBS data, but not necesssarily any counts for that RTENO.
C.bbs.df <- pivot_wider(data = bbs.df,
                        id_cols=rteno, names_from=year,
                        values_from = "C",
                          values_fill=NA,
                        values_fn=sum # C should be the sum across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
                        ) %>%
          tibble::column_to_rownames("rteno") %>%
  as.matrix()

### observation process covariates
pcov.bbs.wind <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "wind.z", ## mean and scaled wind across all of the route
                              values_fill=NA,
                              values_fn=mean)%>%
  tibble::column_to_rownames("rteno") %>%
  as.matrix() # sum across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
pcov.bbs.cars <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "car.z", ## mean and scaled number of cars across all of the route
                              values_fill=NA,
                              values_fn=mean)%>%
  tibble::column_to_rownames("rteno") %>%
  as.matrix() # average across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
pcov.bbs.noise <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "noise.z", ## mean and scaled noise across all of the route
                              values_fill=NA,
                              values_fn=mean) %>%
  tibble::column_to_rownames("rteno") %>%
  as.matrix()# average across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
pcov.bbs.obsfyer <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "obsfirstyearbbs", ## observer's first year on bbs
                              values_fill=NA,
                              values_fn=get_mode) %>%
  tibble::column_to_rownames("rteno") %>%
  as.matrix()# average across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)

## "effort" (proportion of a route within each grid cell)
# propRoutePerGridCell: <array> rteno by grid cell  sliced by time; % of rteno that falls within the grid cell
# propRoutePerGridCell cont.: note: proportion doesn't change over time because we only have one year of stop-level route location data...just keeping for posterity.
propRoutePerGridCell <- acast(bbs.df %>% distinct(rteno, gridcellid, proprouteincell, year),
                              fill = 0,
                              rteno~gridcellid~year, value.var =
                              "proprouteincell",
)

# w[,,1]


### trend effects
ydays <- acast(bbs.df, gridcellid~year~rteno, value.var="yday") #ydays: <array> gridcellid by year by rteno sliced    indicator for day of year BBS conducted


## initial values ----------------------------------------------------------
## Create initial values for C
# Cmax.bbs: maximum count of species within a grid cell per year. NA values (cells with no BBS data) assigned zero as max count.
# Cmax.bbs: <matrix> gridcellid by year, values = max C (with NA==0)
Cmax.bbs <- bbs.df %>%
  group_by(gridcellid, year) %>%
  summarise(Cmax = max(C, na.rm=TRUE)) %>%
  pivot_wider(names_from=year,
              id_cols=gridcellid,
              values_fill = 0,
              values_from = "Cmax") %>%
  tibble::column_to_rownames("gridcellid") %>%
  as.matrix()

n.RoutesPerYear <- bbs.df %>% group_by(year) %>%
  summarise(n=n_distinct(rteno)) %>% arrange(year) %>% ungroup()

## jdat.bbs -------------------------------------------------------------------------
jdat.bbs <- list(
  ## count data
  C.bbs.df = C.bbs.df, # bbs count data aggreagtedf to the grid-level;matrix/df
  C.bbs.array = C.bbs.array, # bbs count data as an array; sliced by RTENO
  N_i_bbs = Cmax.bbs, # matrix
  ## indices/scalars
  id.routesSampled = bbs.rtenos, # vector; identities of bbs routes with data in study area
  n.routesSampled  = length(bbs.rtenos), # integer; # routes with data in study area
  n.routesPerYear = n.RoutesPerYear$n, #
  n.years.bbs = n.years.bbs, # number of of years w/bbs data
  ## covars
  w = propRoutePerGridCell, #  array % of each length of rteno in a grid cell
  bbs.observer.experience = pcov.bbs.obsfyer, # logical-whether it's observer's first year on BBS
  cars = pcov.bbs.cars,
  noise = pcov.bbs.noise,
  wind = pcov.bbs.wind

)
#
str(jdat.bbs$w)

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
# par(mfrow=c(2,1))
# hist(ebird.df$effort_area.z)
# hist(ebird.df$effort_area_ha)
# hist(ebird.df$effort_distance_km)
# hist(ebird.df$effort_distance_km.z)
# hist(ebird.df$number_observers)
# hist(ebird.df$number_observers.z)
# range(ebird.df$number_observers, na.rm=TRUE)
# range(ebird.df$number_observers.z, na.rm=TRUE)


## jdat.ebird -------------------------------------------------------------------------
jdat.ebird <- list() ## keep empty if nothing yet





# Grid/study area ---------------------------------------------------------
## create a data frame for indexing off the grid cell
grid.temp <- grid %>%
  arrange(gridcellid) %>%
  st_drop_geometry()

### create an index for the grid cells with count data
grid.cells.w.ebird <- unique(ebird.df$gridcellid) %>% sort()
grid.cells.w.bbs <- unique(bbs.df$gridcellid) %>% sort()
grid.cells.w.data <- unique(c(grid.cells.w.bbs, grid.cells.w.ebird))


## grid cell coordinates
XY <- grid.temp %>% # centroid coordinates of the grid cells
  select(cell.lon.centroid, cell.lat.centroid) %>% as.matrix()

## jdat.grid --------------------
### create a list of elements relevant to the study area/grid cells
jdat.grid <- list(
  coords.GridCells = XY,
  n.GridCells = nrow(XY), # integer, number of cells in study area (with or without data)
  id.GridCells = grid.temp$gridcellid, # identifier for grid cells
  n.GridCellswData = length(grid.cells.w.data), # number of grid cells with any count data (ebird or bbs)
  id.GridCellswData = grid.cells.w.data,  # vector of grid cell ids with any count data
  n.GridCellswBBS = length(grid.cells.w.bbs), # number of grid cells with any count data (ebird or bbs)
  id.GridCellswBBS = grid.cells.w.bbs,  # vector of grid cell ids with any count data
  n.GridCellswebird = length(grid.cells.w.ebird), # number of grid cells with any count data (ebird or bbs)
  id.GridCellswebird = grid.cells.w.ebird  # vector of grid cell ids with any count data
  )



# Other Indexes -----------------------------------------------------------------


# The Integrated JAGS Data ---------------------------------------------------------------------
jdat <- c(jdat.bbs, jdat.ebird, jdat.grid)



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
args.save <- c(args.save, "jdat")
rm(list=setdiff(ls(), args.save))

