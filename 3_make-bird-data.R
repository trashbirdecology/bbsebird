#### This script is to create and ensure consistency among ebird and bbs spatial data

if(exists("args.save")) {
  rm(list = setdiff(ls(), args.save))
} else
  (rm(list = ls()))
source("0_setup.R")
devtools::load_all()

# Check to see if the eBird and BBS spatial data are already created.  -------------------
fns.spatial <- c("bbs_spatial.rds", "ebird_spatial.rds", "grid.rds")

if(all(fns.spatial %in% list.files(dir.spatial.out))){
  cat("importing bbs, ebird and grid sf objects.")
  fns.spatial=list.files(dir.spatial.out, full.names = TRUE)
  bbs_spatial <- readRDS(fns.spatial[str_detect(fns.spatial, "bbs_spat")])
  grid <- readRDS(fns.spatial[str_detect(fns.spatial, "grid")])
  ebird_spatial <- readRDS(fns.spatial[str_detect(fns.spatial, "ebird_spat")])
}else{
  cat("Building the BBS and eBird data and spatial objects. Sit back and relax......\nor be worried for errors halfway through")
  source("2a_bbs-data.R")
  source("2b_ebird-data.R")
}

# Some Tests -------------------------------------------------------------------------
##For good measure, ensure all the spatial files are in same proj
if(!(st_crs(grid)==st_crs(bbs_spatial) & st_crs(ebird_spatial)==st_crs(grid)))stop("Warning. The ebird, bbs, and grid spatial layers are not in same projection and/or CRS.")

if(!all(grid$id %in% unique(c(ebird_spatial$id, bbs_spatial$id))))
  warning("Empty grid cells (no ebird or bbs data) are not represented in the bird data.\nShould make sure empty cells are added to bird data.")

# Map relevant column names for eBird and BBS -----------------------------
## Force BBS colnames to lowercase.
names(bbs_spatial) <- tolower(names(bbs_spatial))
bbs_spatial <- match_col_names(bbs_spatial)
ebird_spatial <- match_col_names(ebird_spatial)

# Handle Dates and Times --------------------------------------------------
cat("managing dates and times of spatial objects")
# dates
## base date for julian date
base.date <- min(bbs_spatial$date)
## make julian dates
bbs_spatial$date <- lubridate::as_date(bbs_spatial$date)
ebird_spatial$date <- lubridate::as_date(ebird_spatial$date)
## make julian dates
bbs_spatial$julian <- julian(bbs_spatial$date, origin = base.date)
ebird_spatial$julian <- julian(as.Date(ebird_spatial$date), origin = base.date)
## make day of year
ebird_spatial$yday <- lubridate::yday(ebird_spatial$date)
bbs_spatial$yday <- lubridate::yday(bbs_spatial$date)

# times
### this is an ugly workaround and can be improved, including putting it into
### the BBS and eBird munging functions but this is it for now.
bbs_spatial$starttime=hms::as_hms(as.POSIXct(bbs_spatial$starttime, format="%H%M"))
bbs_spatial$endtime=hms::as_hms(as.POSIXct(bbs_spatial$endtime, format="%H%M"))
ebird_spatial$time_observations_started=hms::as_hms(as.POSIXct(ebird_spatial$time_observations_started, format="%H:%M:%S"))

## here, data must hvave columns lat and lon. I took care of this in
## utils.R function `match_col_names()`
cat("calculating astronomical stats...yes, the astronomy definition")
sunlight.keep <- c("dawn", "solarNoon", "sunrise","sunriseEnd")
bbs.sunlight <- suncalc::getSunlightTimes(data=bbs_spatial, keep = sunlight.keep)
ebird.sunlight <- suncalc::getSunlightTimes(data=ebird_spatial, keep = sunlight.keep)

### turn vars in sunlight.keep into time only (otherwise they are in YYYY-MM-DD HH-MM-SS; we need only HH-MM)
bbs.sunlight <- bbs.sunlight %>%
  mutate(across(sunlight.keep, hms::as_hms))
ebird.sunlight <- ebird.sunlight %>%
  mutate(across(sunlight.keep, hms::as_hms))

### add sunlight information to spatial data
bbs <- left_join(bbs_spatial, bbs.sunlight)
ebird <- left_join(ebird_spatial, ebird.sunlight)

# Some exploratory plots to ensure data is sensical.  ---------------------
ggplot(ebird)+geom_histogram(aes(log(C)))+ggtitle("ebird")
ggplot(bbs)+geom_histogram(aes(log(C)))+ggtitle("bbs")

ggplot(bbs)+geom_histogram(aes(sunrise))+ggtitle("bbs")
ggplot(ebird)+geom_histogram(aes(sunrise))+ggtitle("ebird")


# Export Data -------------------------------------------------------------
saveRDS(bbs, file=paste0(dir.jags, "bbs.rds"))
saveRDS(ebird, file=paste0(dir.jags, "ebird.rds"))
saveRDS(grid, file=paste0(dir.jags, "grid.rds"))

# Clear mem ---------------------------------------------------------------------
if(exists("args.save")){
  args.save <- c(args.save, "ebird", "bbs",'grid')
  rm(list=setdiff(ls(), args.save))
}
