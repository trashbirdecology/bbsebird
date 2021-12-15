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

if(!all(grid$gridcellid %in% unique(c(ebird_spatial$gridcellid, bbs_spatial$gridcellid))))
  warning("Empty grid cells (no ebird or bbs data) are not represented in the bird data.\nShould make sure empty cells are added to bird data.")

# Map relevant column names for eBird and BBS -----------------------------
## Force BBS colnames to lowercase.
names(bbs_spatial) <- tolower(names(bbs_spatial))
bbs_spatial <- match_col_names(bbs_spatial)
ebird_spatial <- match_col_names(ebird_spatial)

# Handle Dates and Times --------------------------------------------------
cat("managing dates and times of spatial objects")
# dates
## make julian dates
bbs_spatial$date <- lubridate::as_date(bbs_spatial$date)
ebird_spatial$date <- lubridate::as_date(ebird_spatial$date)
## base date for julian date
#### eventually will need to save this or export it somewhere, maybe add it to jags list idk
base.date <- min(c(bbs_spatial$date, ebird_spatial$date), na.rm=TRUE)
## make julian dates
bbs_spatial$julian <- julian(bbs_spatial$date, origin = base.date)
ebird_spatial$julian <- julian(as.Date(ebird_spatial$date), origin = base.date)
## make day of year
ebird_spatial$yday <- lubridate::yday(ebird_spatial$date)
bbs_spatial$yday <- lubridate::yday(bbs_spatial$date)

## Filter days of the year if specified
if(exists("yday")) ebird_spatial <- ebird_spatial %>%
  filter(yday >= min.yday & yday <= max.yday)
if(exists("yday")) ebird_spatial <- ebird_spatial %>%
  filter(yday >= min.yday & yday <= max.yday)


# times
### this is an ugly workaround and can be improved, including putting it into
### the BBS and eBird munging functions but this is it for now.
bbs_spatial$starttime=hms::as_hms(as.POSIXct(bbs_spatial$starttime, format="%H%M"))
bbs_spatial$endtime=hms::as_hms(as.POSIXct(bbs_spatial$endtime, format="%H%M"))
ebird_spatial$time_observations_started=hms::as_hms(as.POSIXct(ebird_spatial$time_observations_started, format="%H:%M:%S"))


# Sunlight/daylight/moonlight ---------------------------------------------
## here, data must have columns lat and lon. I took care of this in
## utils.R function `match_col_names()`
cat("calculating astronomical stats...yes, the astronomy definition.\n")
sunlight.keep <- c("dawn", "solarNoon", "sunrise","sunriseEnd")

bbs.sunlight <- suncalc::getSunlightTimes(data=bbs_spatial %>% distinct(date, lon, lat),
                                            keep = sunlight.keep)


## ebird is so large that I need to split up b/c takes forever.
## i'd like to use kit::funique, but cannot figure out how to do that with >1 columns.
### so, am resorting to this method.
x <- ebird_spatial %>% dplyr::select(date, lat, lon)
chunks <- parallel::splitIndices(nrow(x), 100)
for(i in seq_along(chunks)){
  if(i==1) ebird.sunlight <- NULL
  rows = as.data.frame(chunks[i])
  chunk.start = min(rows[1])
  chunk.end   = max(rows[1])
  dat = x[chunk.start:chunk.end, ]
  dat = suncalc::getSunlightTimes(data=dat,
                            keep = sunlight.keep)

  ebird.sunlight <- dplyr::bind_rows(ebird.sunlight,dat)
  rm(dat, chunk.end, chunk.start, rows)
}

### turn vars in sunlight.keep into time only (otherwise they are in YYYY-MM-DD HH-MM-SS; we need only HH-MM)
ebird.sunlight <- ebird.sunlight  %>%
  mutate(across(sunlight.keep, hms::as_hms))
bbs.sunlight <- bbs.sunlight %>%
  mutate(across(sunlight.keep, hms::as_hms))

### add sunlight information to spatial data
bbs <- left_join(bbs_spatial, bbs.sunlight)
ebird <- left_join(ebird_spatial, ebird.sunlight)

rm(ebird_spatial, bbs_spatial, ebird.sunlight, bbs.sunlight)
gc()

# Munge covariates --------------------------------------------------------
## BBS detection covariates
bbs <- bbs %>%
  group_by(rteno, year) %>%
  mutate(avgwind = abs(startwind-endwind)/2) %>%
  ungroup()



# Export Data -------------------------------------------------------------
saveRDS(bbs, file=paste0(dir.jags, "bbs.rds"))
saveRDS(ebird, file=paste0(dir.jags, "ebird.rds"))
saveRDS(grid, file=paste0(dir.jags, "grid.rds"))


# Plots-exploratory---------------------
ggplot(ebird)+geom_histogram(aes(log(C)))+ggtitle("ebird")
ggplot(bbs)+geom_histogram(aes(log(C)))+ggtitle("bbs")

ggplot(bbs)+geom_histogram(aes(sunrise))+ggtitle("bbs")
ggplot(ebird)+geom_histogram(aes(sunrise))+ggtitle("ebird")


# Clear mem ---------------------------------------------------------------------
if(exists("args.save")){
  args.save <- c(args.save, "ebird", "bbs",'grid')
  rm(list=setdiff(ls(), args.save))
}
