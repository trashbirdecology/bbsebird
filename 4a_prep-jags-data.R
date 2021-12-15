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


# Grid/study area ---------------------------------------------------------
## create a data frame for indexing off the grid cell
grid.temp <- grid %>%
  arrange(gridcellid) %>%
  st_drop_geometry()

### create an index for the grid cells with count data
grid.cells.w.counts <- unique(c(bbs.df$gridcellid, ebird.df$gridcellid)) %>% sort()

## grid cell coordiantes
XY <- grid.temp %>% # centroid coordinates of the grid cells
  select(cell.lon.centroid, cell.lat.centroid) %>% as.matrix()

## jdat.grid --------------------
### create a list of elements relevant to the study area/grid cells
jdat.grid <- list(
  coords.GridCells = XY,
  n.GridCells = nrow(XY), # integer, number of cells in study area (with or without data)
  id.GridCells = grid.temp$gridcellid, # identifier for grid cells
  n.GridCellswData = length(grid.cells.w.counts), # number of grid cells with any count data (ebird or bbs)
  id.GridCellswData = grid.cells.w.counts # vector of grid cell ids with any count data
)

str(jdat.grid)


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

## create array of BBS count data sliced by RTENO
# C.bbs.array: array <cell id by year sliced by BBS route num>
# C.bbs.array cont.: C for BBS comprises count data at the route-level (sum of 50-stops)
# C.bbs.array cont.: has rows for all grid cells that have at least some BBS data, but not necesssarily any counts for that RTENO.
C.bbs.array <- acast(bbs.df, gridcellid~as.integer(year)~rteno, value.var="C")
ind <- assertthat::are_equal(dim(C.bbs.array)[3],
                      length(unique(bbs.df$rteno)))
if(ind) bbs.rtenos <- sort(unique(bbs.df$rteno))
## create single data frame of BBS count data per RTENO and year
# C.bbs.df: array <cell id by year sliced by BBS route num>
# C.bbs.df cont.: C for BBS comprises count data at the route-level (sum of 50-stops)
# C.bbs.df cont.: has rows for all grid cells that have at least some BBS data, but not necesssarily any counts for that RTENO.
C.bbs <- pivot_wider(data = bbs.df,
                        id_cols=rteno, names_from=year,
                        values_from = "C",
                          values_fill=NA,
                        values_fn=sum # C should be the sum across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
                        )

### observation process covariates
pcov.bbs.wind <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "wind.z", ## mean and scaled wind across all of the route
                              values_fill=NA,
                              values_fn=mean) # sum across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
pcov.bbs.cars <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "car.z", ## mean and scaled number of cars across all of the route
                              values_fill=NA,
                              values_fn=mean) # average across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
pcov.bbs.noise <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "noise.z", ## mean and scaled noise across all of the route
                              values_fill=NA,
                              values_fn=mean) # average across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)
pcov.bbs.obsfyer <-  pivot_wider(data = bbs.df,
                              id_cols=rteno, names_from=year,
                              values_from = "obsfirstyearbbs", ## observer's first year on bbs
                              values_fill=NA,
                              values_fn=get_mode) # average across rteno (b/c some rtenos are split b/w many gridcellid, and here we are ignoring grid cell id)

## "effort" (proportion of a route within each grid cell)
# propRoutePerGridCell: <array> grid cell id by year sliced by RTENO; proportion of
propRoutePerGridCell <- acast(bbs.df,
                              # fill = 0, # not sure if we need to zero-fill where no data eixts. dont thikn it matters..
                              gridcellid ~ as.integer(year) ~ rteno, value.var =
                                "proprouteincell",
)



## scalars/indexes -----------------------------------------------------------------





## make initial Values ----------------------------------------------------------
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



## jdat.bbs -------------------------------------------------------------------------
jdat.bbs <- list(C = C.bbs, # array
                 N_i = Cmax.bbs, # matrix
                 routes.sampled = bbs.rtenos, # vector
)




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


# Indexes -----------------------------------------------------------------
## remove empty grid cells from ebird/bbs data
bbs.df   <- bbs.df %>% filter(!is.na(C))
ebird.df <- ebird.df %>% filter(!is.na(C))

## initial values
### N_bbs: maximum number of observed
N_bbs <-

N_ebird <- NULL



# Integrated data ---------------------------------------------------------------------




# Some Random Plots -----------------------------------------------------------------
plot(grid[2])
bbs %>% group_by(gridcellid) %>% summarise(max_C_bbs= max(C)) -> temp
plot(temp["max_C_bbs"])
ebird %>% filter(year==2019) %>% group_by(gridcellid) %>% summarise(max_C_ebird_yr2019= max(C)) -> temp
plot(temp["max_C_ebird_yr2019"])


