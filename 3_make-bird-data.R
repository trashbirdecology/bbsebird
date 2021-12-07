if(exists("args.save")) {
  rm(list = setdiff(ls(), args.save))
} else
  (rm(list = ls()))
source("0_setup.R")
devtools::load_all()

# Check to see if zero-filled eBird data is already created.  -------------------
fns.spatial <- c("bbs_spatial.rds", "ebird_spatial.rds", "grid.rds")
if(all(fns.spatial %in% list.files(dir.spatial.out))){
  fns.spatial=list.files(dir.spatial.out, full.names = TRUE)
  bbs_spatial <- readRDS(fns.spatial[str_detect(fns.spatial, "bbs")])
  ebird_spatial <- readRDS(fns.spatial[str_detect(fns.spatial, "ebird")])
  grid <- readRDS(fns.spatial[str_detect(fns.spatial, "grid")])
}else(source("2_make-ebird"))


# Some Tests -------------------------------------------------------------------------
##For good measure, ensure all the spatial files are in same proj
if(!(st_crs(grid)==st_crs(bbs_spatial) & st_crs(ebird_spatial)==st_crs(grid)))stop("Warning. The ebird, bbs, and grid spatial layers are not in same projection and/or CRS.")

##For good measure, ensure all the spatial files are in same proj
if(!all(grid$id %in% c(ebird_spatial$id, bbs_spatial$id))) stop("Some grid cells are missing. Good luck finding that problem.")


# New Data Specs --------------------------------------------------------
## Force BBS colnames to lowercase.
names(bbs_spatial) <- tolower(names(bbs_spatial))

# base date for juliaday
base.date <- min(bbs_spatial$date)


# Map relevant column names for eBird and BBS -----------------------------


## munge data frames for ebird and bbs
## to force consistency in naming conventions
### EBIRD
names(ebird_spatial) %>% sort()
ebird_spatial <- ebird_spatial %>%
  rename(
    C=`observation count`)

### BBS
names(bbs_spatial) %>% sort()
bbs_spatial <- bbs_spatial %>%
  mutate( # for some reason rename isn't working on this df...wtf
    C=totalspp) %>%
  dplyr::select(-totalspp)


# Some exploratory plots to ensure data is sensical.  ---------------------
hist(bbs_spatial$TotalSpp)
plot(bbs_spatial["TotalSpp"])

