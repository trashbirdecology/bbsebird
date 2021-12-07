rm(list=ls())
source("0_setup.R")
devtools::load_all()

# Create a spatial grid -----------------------------------------------------------
# grid.size = c(diam.km*1000, diam.km*1000) ## convert km to m //
study.area <-
  ne_states(country = countries, returnclass = "sf") %>%
  # remove region(s)
  filter(tolower(name) %in% tolower(states)) %>%
  filter(!tolower(name) %in% tolower(region.remove)) %>%
  st_transform(study.area, crs = crs.target)
# unique(study.area$adm0_a3) #should add a test here to make sure number of countries expected is grabbed.

# throw a grid over the study area layer
grid <- study.area %>%
  st_make_grid(cellsize = grid.size,
               square = FALSE,
               flat_topped = TRUE) %>%
  st_intersection(study.area) %>%
  # st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number()) %>%
  st_transform(crs = crs.target)

# # Visualize to check
# tmap::qtm(grid)
# mapview::mapview(grid) # interactive, openstreetmap


# Munge BBS data ----------------------------------------------------------
## Import and/or Download BBS Observations and Metadata -----------------------------
if(!exists("bbs.orig")){
  if("bbs-orig.rds" %in% dir.bbs.out) bbs.orig <- readRDS(paste0(dir.bbs.out, "/bbs-orig.rds"))
  if(!"bbs-orig.rds" %in% dir.bbs.out) bbs.orig <- grab_bbs_data(sb_dir=dir.bbs.out)
  }

bbs_obs <-
  munge_bbs(
    list = bbs.orig,
    spp = interest.species,
    zero.fill = TRUE,
    active.only = TRUE,
    keep.stop.level.data = FALSE
  )


## Make BBS Spatial Layers ----------------------------------------------
### Create BBS routes spatial layer ----------------------------------------------------------------------
# munges all routes at first, so could take 30sec or more
bbs_routes <-
  munge_bbs_shapefiles(
    cws.routes.dir = cws.routes.dir, #location of the CWS BBS routes shapefiles
    usgs.routes.dir = usgs.routes.dir, #location of the USGS BBS routes shapefiles
    crs.target = crs.target,
    routes.keep=unique(bbs$RTENO),
    grid=grid,
    overwrite=TRUE # wanna overwrite existing bbs_routes in workspace?
  )

## to this point from 0_setup.r takes about 82 seconds for FL, GA, and SC
# if grid was provided in munge_bbs_shapefiles, this is unnecessary so don't do it
if(!"id" %in% tolower(names(bbs_routes))){bbs_routes <- st_intersection(grid, bbs_routes)}


### Add BBS obsv data to spatial-------------------------------------------------------------------------
if("routename" %in% tolower(names(bbs_obs))){bbs_obs <-  bbs_obs %>%
  dplyr::select(-RouteName)}

bbs_spatial <-
  merge(bbs_routes, bbs_obs, by="RTENO") %>%
  # create var with percent route in grid cell
  mutate(PercSegmentInCell = SegmentLength / RouteLength)


### Some exploratory plots (optional) -----------------------------
mapview::mapview(st_filter(bbs_spatial, grid)) # interactive, openstreetmap
# exploratory plots (should move elsewhere.....)
# plot(bbs_spatial[c("",)])# select a specific variable(S) to plot
# plot(bbs_spatial %>% group_by(RTENO) %>% summarise(n_years=n_distinct(Year)) %>% dplyr::select(n_years))
# plot(bbs_spatial %>% group_by(id) %>% summarise(n_observers_per_cell=n_distinct(ObsN)) %>% dplyr::select(n_obs_per_cell))
# plot(bbs_spatial  %>% group_by(id) %>% summarise(n_routes_cell=n_distinct(RTENO, na.rm=TRUE))ot((bbs_spatial  %>% group_by(id) %>% summarise(n_routes_cell=n_distinct(RTENO, na.rm=TRUE)))[,"n_routes_cell"],)
# plot(bbs_spatial %>% group_by(id) %>% summarise(maxC_bbs=max(TotalSpp)) %>% dplyr::select(maxC_bbs))
# t=  bbs_spatial %>%
#     dplyr::select(ObsN, RTENO, id) %>%
#   group_by(RTENO, id) %>%
#   summarise(nhumans=n_distinct(ObsN)) %>%
#     distinct(RTENO, nhumans, .keep_all=T) %>%
#   ungroup() %>%
#   group_by(id) %>%
#   summarize(med_nhuman_cell=median(nhumans))
# plot(t[,"med_nhuman_cell"]) # median number of observers per route within the cell

as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))


gc(full = TRUE)
if(detectCores() <=4 | memory.limit() < 25000) warning("You don't have enough RAM and/or CPU to munge the eBird data. Don't blame me if your machine crashes.")
message("Tossing out the garbage (`gc`) and about to deal with this eBird data. Buckle up, buttercup.")


# Munge eBird data --------------------------------------------------------
### CURRENTLY, THIS REQUIRES A SIGNIFICANT AMT OF RAM FOR EVEN A SINGLE SPECIES. >>40GB
# Get the list of potential files for import. This will be used in get_ebird()
## Import Original Data and Create Zero-filled eBird data -----------------------------
fn_zf <- list.files(dir.ebird.out, "rds")
fns <- id_ebird_files(dir.ebird.in = dir.ebird.in)
message("This process of initial eBird munging takes ~ 3min for 3 U.S. states worth of data. FYI...")
if (!exists("ebd_zf")) {
  tic()
  ebd_zf <- get_zerofilled_ebird(fns, overwrite = FALSE)
  ## keep only modern data...(ebird dataset has dates back to year 1880). should help
  ebd_zf <-
    ### these need to go into the get_zerofilled_ebird but thats for a later date.
    ebd_zf %>% mutate(
      julian = lubridate::yday(observation_date),
      year = lubridate::year(observation_date)
    ) %>%
    filter(year >= 1966)

  ## remove Alaska and Hawaii just to wittle down the data a little
  if (exists("states") &
      !is.null(states))
    ebd_zf <- ebd_zf %>% filter(tolower(state) %in% tolower(states))
  if (exists("region.remove") &
      !is.null(region.remove))
    ebd_zf <-
    ebd_zf %>% filter(!tolower(state) %in% tolower(region.remove))
  ## remove all presumed bbs observations from ebird
  ebd_zf <- ebd_zf %>%
    filter(!(tolower(protocol_type) == "stationary" &
               duration_minutes == 3))

  # Create some variables for use later...
  ebd_zf <-
    ebd_zf %>% mutate(is.stationary = if_else(tolower(protocol_type) == "stationary", 1, 0))
  toc()
}

## eBird to Spatial Layers -----------------------------------------------------
###NEED TO PUSH THIS TO MUNGE_EBIRD_SPATIAL or something like that
# convert ebd to spatial object
coordinates(ebd_zf) <-
  ~ longitude + latitude # 1-2 minutes for all of N.Amer.
# define projection for lat long (ebird documentation states CRS is 4326)
proj4string(ebd_zf) <-
  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# transform spatial object to sf
ebd_zf <- sf::st_as_sf(ebd_zf)
# match proj to target proj
ebd_zf <-
  st_transform(ebd_zf, crs = CRS(paste0("+init=epsg:", crs.target)))

## Combine grid and eBird -----------------------------
# this will take a minute
tic()
ebird_spatial <- grid %>% #  37sec for Ohio//2min for FL,GA,SC
  st_join(ebd_zf)
toc()


# Export Data -------------------------------------------------------------
saveRDS(ebird_spatial, file = paste0(dir.munged, "/", "ebird_spatial.rds"))
saveRDS(bbs_spatial, file = paste0(dir.munged, "/", "bbs_spatial.rds"))
saveRDS(grid, file = paste0(dir.munged, "/", "grid.rds"))
