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
  source("2a_bbs_data.R")
  source("2b_ebird-data.R")
}

# Some Tests -------------------------------------------------------------------------
##For good measure, ensure all the spatial files are in same proj
if(!(st_crs(grid)==st_crs(bbs_spatial) & st_crs(ebird_spatial)==st_crs(grid)))stop("Warning. The ebird, bbs, and grid spatial layers are not in same projection and/or CRS.")

##For good measure, ensure all the spatial files are in same proj
if(!all(grid$id %in% c(ebird_spatial$id, bbs_spatial$id))) stop("Some grid cells are missing. Good luck finding that problem.")

# Map relevant column names for eBird and BBS -----------------------------
## Force BBS colnames to lowercase.
names(bbs_spatial) <- tolower(names(bbs_spatial))
bbs_spatial<-match_col_names(bbs_spatial)
ebird_spatial<-match_col_names(ebird_spatial)


# Handle Dates and Times --------------------------------------------------
# base date for julian date
base.date <- min(bbs_spatial$date)
## make julian dates
bbs_spatial$julian <- julian(bbs_spatial$date, origin = base.date)
ebird_spatial$julian <- julian(as.Date(ebird_spatial$date), origin = base.date)
## make day of year
ebird_spatial$yday <- lubridate::yday(ebird_spatial$date)
bbs_spatial$yday <- lubridate::yday(bbs_spatial$date)



# ebirdC only ZEROES AT THIS POINT... Need to check

# Some exploratory plots to ensure data is sensical.  ---------------------
plot(density(log(bbs_spatial$C)))
plot(density(log(na.omit(ebird_spatial$C))))
