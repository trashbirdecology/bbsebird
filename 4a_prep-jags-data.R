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


## Grid/study area ---------------------------------------------------------
XY <- cbind(grid$cell.lon.centroid, grid$cell.lat.centroid)
XY.scaled <- scale(XY)
nGridCells <- nrow(XY)


## BBS data ---------------------------------------------------------
##coerce the bbs data to a data frame.
bbs.df <- bbs %>% st_drop_geometry() %>%
  as.data.frame() %>%
  ### scale covariates
  mutate(
    wind.z = (avgwind - mean(avgwind, na.rm=TRUE))/sd(avgwind, na.rm=TRUE),
    noise.z = (noisemean - mean(noisemean, na.rm=TRUE))/sd(noisemean, na.rm=TRUE),
    car.z = (carmean - mean(carmean, na.rm=TRUE))/sd(carmean, na.rm=TRUE)
  )


## eBird data --------------------------------------------------------------
ebird.df <- ebird %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  distinct() %>% ## not sur why there are dupilcates...need to check this upstream later!!!
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
#





# END RUN -----------------------------------------------------------------


