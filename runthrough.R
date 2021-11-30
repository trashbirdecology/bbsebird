# Working runthrough of the package
## using this script as a template for organizing the package.
## not sure exactly yet what functions are necessary/useful, so throwing the workflow in here and we will add as we go.
rm(list=ls())
gc()
# Libraries ---------------------------------------------------------------
devtools::install_github("trashbirdecology/bbsassistant",ref="convert-to-50-stop", force=FALSE)
devtools::install_github("ropensci/rnaturalearthhires") ## must install from GH -- source has unresolved issues for 2+years (see issue https://github.com/ropensci/rnaturalearthhires/issues/1)
library(bbsAssistant)
library(purrr)
library(auk)
library(hms) ## for some reason zerofiltering function in auk cannot find this fun?
library(dplyr)
library(stringr)
library(lubridate)
library(rgdal)
library(spData) # world map
library(rnaturalearth) # state and prov data?
library(rnaturalearthhires) # ne_states needs this idfk why
library(sp) ## goign to stick with sp and rgdal for now unless it gives me reason not to...
library(sf)
library(maps)
library(ggplot2)
library(mapproj)
library(doParallel)
library(multidplyr)
## not sure i need
library(gridExtra)
library(raster)
library(rasterVis)
library(rgeos)
library(rgbif)
library(lubridate)
options(stringsAsFactors = FALSE)

devtools::load_all()

# Memory limits -----------------------------------------------------------
# adjust memory limit to handle large eBird data files depending on OS
# if(.Platform$OS.type=="unix") library(unix)
# if(.Platform$OS.type=="unix") unix::rlimit_as(1e12) # prob unnecessary but doesn't hurt (probably)
# if(.Platform$OS.type=="windows") memory.limit(35000)


# Directory specifications and creation ------------------------------------------------
# ebird data
  ## because the ebird data is so massive, it's inconvenient to store inside the package.
  ## specify dir.ebird.in as the location where you have saved the EBD (eBird reference database)
  dir.ebird.in <- "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird"
  dir.ebird.out <- "data-local/ebird/"
  auk_set_ebd_path(dir.ebird.in, overwrite = TRUE)
  fn_ebd <- paste0(dir.ebird.in, "/ebd_US-FL_doccor_relSep-2021.txt")
  fn_samp <- paste0(dir.ebird.in, "/ebd_sampling_relSep-2021.txt")


# BBS data
  ## bbs data is much smaller than eBird, so storing in the project directory is feasible
  ## specify dir.ebird.out as the location where you will save local, post-munging ebird data (as .rda/.rds)
  dir.bbs.out <- "data-local/bbs/"

  ## BBS Route location shapefiles
  cws.routes.dir="C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/bbs/route_shapefiles/cws"
  usgs.routes.dir="C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/bbs/route_shapefiles/usgs"

# GeoSpatial data
  dir.spatial.out <- "data-local/spatial"

  if(!"data-local" %in% list.files())dir.create("data-local", showWarnings = FALSE)
  sapply(c(dir.bbs.out, dir.spatial.out), dir.create)

# Data specifications -----------------------------------------------------
  # Specify region(s), specie(S) and temporal period(s) to use for data subsetting, etc.
  # interest.spatial <- paste0("US-", c("OR", "CA","WA", "ID", "AZ", "NV")) # states / province / territory
  interest.species <- c("DOCCOR", "DOCCO", "DCCO", "DCCOR", "Double-crested Cormorant", "Double Crested Cormorant") ## need to provide a lookup-table relating the ebird to BBS taxa, including codes
  # interest.spatial <- c("United States", "Canada", "CAN", "CA","US", "USA")
  ebird.protocol <- c("Traveling", "Stationary")
  complete.checklists.only <- TRUE
  crs.target <- 4326 # 5070 =usgs//usa/alberts equal area; 4326=unprojected;
  include.unid <- FALSE ## Whether or not to include UNIDENTIFIED // hybrid species
  countries <- c("Canada","USA", "United States", "United States of America") # used to create base maps and spatial grid system.
  diam.deg=69/111.111 # 111.111km in 1 deg lat lon is a good estimate for much of world
  # diam.deg=6/111.111 # 111.111km in 1 deg lat lon is a good estimate for much of world
  # diam.km=1.61*(24.5+10) #km conversion of (~24,5 mile routes + 10miles buffer)*X to ensure route always falls within a single cell
  region.remove = c("Alaska", "Hawaii", "Northwest Territories", "Yukon", "Nunavut", "Yukon Territory") #
  # states <-
  #   c(
  #     'Iowa',
  #     'Illinois',
  #     'Indiana',
  #     'Michigan',
  #     'Minnesota',
  #     # 'New York',
  #     'Ohio',
  #     # 'Pennsylvania',
  #     'Wisconsin',
  #     'Ontario'
    # )
  states<- c("Ohio")

# Create a spatial grid ------------------------------------------------------------
  grid.size = diam.deg
  # grid.size = c(diam.km*1000, diam.km*1000) ## convert km to m //
  study.area <- ne_states(country = countries, returnclass = "sf") %>%
    # remove region(s)
    filter(tolower(name) %in% tolower(states)) %>%
    filter(!tolower(name) %in% tolower(region.remove)) %>%
    st_transform(study.area, crs=crs.target)
  # unique(study.area$adm0_a3) #should add a test here to make sure number of countries expected is grabbed.
  # throw a grid over the study area layer
  grid <- study.area %>%
    st_make_grid(cellsize = grid.size, square = FALSE, flat_topped = TRUE) %>%
    st_intersection(study.area) %>%
    # st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(id = row_number()) %>%
    st_transform(crs=crs.target)

  # # Visualize to check
  # plot(st_geometry(grid), axes=TRUE)
  # plot(st_filter(bbs_routes, grid), add=T)
  # mapview::mapview(st_filter(bbs_routes, grid)) # interactive, openstreetmap


# Munge BBS data ----------------------------------------------------------
  # if(!exists("bbs.orig")) bbs.orig <- grab_bbs_data(sb_dir=dir.bbs.out) # defaults to most recent release of the BBS dataset available on USGS ScienceBase
  # if(exists("sb_items"))rm(sb_items) # i need to add an arg to bbsassistant:grab_bbs_data that prevents output of sb_items...
  # filter by species of interest, zero-fill
  # saveRDS(bbs.orig,"data-local//bbs/bbs-orig.rds")
  bbs.orig <- readRDS("data-local/bbs/bbs-orig.rds")
  bbs <- filter_bbs_by_species(list = bbs.orig, search = interest.species,
                               zero.fill=TRUE, active.only=TRUE, keep.stop.level.data=FALSE)
  bbs$weather <- make.rteno(bbs$weather) # create var called RTENO (need to fix this in bbsassistant)
  bbs$vehicle_data <- make.rteno(bbs$vehicle_data) # create var called RTENO (need to fix this in bbsassistant)
  bbs$weather <- bbs$weather %>%
    filter(QualityCurrentID==1)
  bbs$observers <- bbs$weather %>%
    mutate(Date= as.Date(paste(Day, Month, Year, sep="/"), format="%d/%m/%Y")) %>%
    dplyr::select(ObsN, RTENO, Date, TotalSpp, Month, Day, Year) %>%
  ##create binary for if observer's first year on the BBS and on the route
  group_by(ObsN) %>% #observation identifier (number)
    mutate(ObsFirstYearOnBBS = ifelse(Date==min(Date), 1, 0)) %>%
    group_by(ObsN, RTENO) %>%
    mutate(ObsFirstYearOnRTENO = ifelse(Date==min(Date), 1, 0)) %>%
    ungroup() # to be safe
  bbs$observations <- bbs$observations %>%
    # keep only Canada and US (excluding HI and AK)
    filter(CountryNum %in% c(124, 840))  %>%
    # Keep only RPID=101
    filter(RPID==101) %>%
    filter(RTENO %in% bbs$weather$RTENO)
  # remove HI and AK from the bbs dataset
  # data(region_codes) # region codes from bbsAssistant package.
  region_codes.subset <- region_codes %>%
    filter(CountryNum %in% c(124, 840)) # keep US and CAN only
  # have to split up the state num and country num process b/c of Mexico's character issues.
  statenums.keep <- region_codes.subset$StateNum[tolower(region_codes.subset$State) %in% tolower(states)]
  statenums.remove <- region_codes.subset$StateNum[tolower(region_codes.subset$State) %in% tolower(region.remove)]
  if(!is.null(statenums.keep)) region_codes.subset <- region_codes.subset %>%
    filter(StateNum %in% statenums.keep) # remove states specified above US and CAN only
  if(!is.null(statenums.remove)) region_codes.subset <- region_codes.subset %>%
    filter(!StateNum %in% statenums.remove) # remove states specified above US and CAN only
  bbs$observations <- bbs$observations %>%
    filter(!StateNum %in% statenums.remove)
  ## Finally, remove all the unnecessary routes in bbs$routes and weather and vehicle
  bbs$routes <- bbs$routes %>% filter(RTENO %in% bbs$observations$RTENO)
  bbs$weather <- bbs$weather %>% filter(RTENO %in% bbs$observations$RTENO)
  bbs$vehicle_data <- bbs$vehicle_data %>% filter(RTENO %in% bbs$observations$RTENO)

  ## A TEST!
  if(!all(bbs$routes$RTENO %in% bbs$observations$RTENO)) stop("something is wrong use traceback()")
  if(!all(bbs$weather$RTENO %in% bbs$observations$RTENO)) stop("something is wrong use traceback()")
  if(!all(bbs$vehicle_data$RTENO %in% bbs$observations$RTENO)) stop("something is wrong use traceback()")
  if(!all(bbs$observations$RTENO %in% bbs$routes$RTENO)) stop("something is wrong use traceback()")
  if(!all(bbs$observations$RTENO %in% bbs$weather$RTENO)) stop("something is wrong use traceback()")
  if(!all(bbs$observations$RTENO %in% bbs$vehicle_data$RTENO)) warning("Some vehicle data missing. Don't worry about it unless you're using vehicle data in the model.")

# Munge BBS route shapefiles ----------------------------------------------
  # munges all routes at first, takes 30sec or so
  bbs_routes <-
    munge_bbs_shapefiles(
      cws.routes.dir = cws.routes.dir,
      usgs.routes.dir = usgs.routes.dir,
      crs.target =
        crs.target
    )%>%
    ## remove the routes not in the observations dataset
    filter(RTENO %in% bbs$observations$RTENO)
  # which routes are missing from the spatial shapefile layer

  # #### PROBLEMS TO SOLVE: MISSING ROUTES ### RUN AFTER IMPORTING BBS OBS DATA
  # bbs$routes[which(!bbs$routes$RTENO %in% bbs_routes_sldf$RTENO),] %>%
  #   distinct(RTENO, .keep_all = TRUE) %>%
  #   group_by(CountryNum, StateNum) %>%
  #   summarise(n()) %>% left_join(region_codes) %>%
  #   write.csv('data-local/missingroutesbyregion.csv')
  # setdiff(unique(bbs$observations$RTENO),
  #         unique(bbs_routes_sldf@data$RTENO)) %>%  ## which of A are not in B
  #   write.csv('data-local/missingroutes.csv')

## Join BBS routes to grid. Left_join preserves the empty grid cells.
bbs_intersection <- st_intersection(grid, bbs_routes) %>%
    mutate(SegmentLength = st_length(.)) %>%
    st_drop_geometry() # complicates things in joins later on
bbs_grid <- grid %>%
  left_join(bbs_intersection, by = "id")
# add bbs data to grid and lines obj
bbs_spatial <- bbs_grid %>%
  left_join(bbs$observations) %>%
  left_join(bbs$observers) %>%
# create var with percent route in grid cell
  mutate(PercSegmentInCell=SegmentLength/RouteLength)


# mapview::mapview(st_filter(bbs_spatial, grid)) # interactive, openstreetmap
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
# plot(t3[,"med_nhuman_cell"]) # median number of observers per route within the cell


# Munge eBird data --------------------------------------------------------
### CURRENTLY, THIS REQUIRES A SIGNIFICANT AMT OF RAM FOR EVEN A SINGLE SPECIES. >>40GB
# Get the list of potential files for import. This will be used in get_ebird()
fn_zf <- list.files(dir.ebird.out, "rds")
fns <- id_ebird_files(dir.ebird.in = dir.ebird.in)
if(!exists("ebd_zf")) {
  ebd_zf <- get_zerofilled_ebird(fns, overwrite = FALSE)
  ## keep only modern data...(ebird dataset has dates back to year 1880). should help
  ebd_zf <- ### these need to go into the get_zerofilled_ebird but thats for a later date.
    ebd_zf %>% mutate(
      julian = lubridate::yday(observation_date),
      year = lubridate::year(observation_date)
    ) %>%
    filter(year >= 1966)
  ## remove Alaska and Hawaii just to wittle down the data a little
  if(exists("states") & !is.null(states))  ebd_zf <- ebd_zf %>% filter(tolower(state) %in% tolower(states))
  if(exists("region.remove") & !is.null(region.remove))  ebd_zf <- ebd_zf %>% filter(!tolower(state) %in% tolower(region.remove))
  # Create some variables for analysis
  ebd_zf <- ebd_zf %>% mutate(is.stationary=if_else(tolower(protocol_type)=="stationary", 1, 0))
}

# Create eBird spatial -----------------------------------------------------
###NEED TO PUSH THIS TO MUNGE_EBIRD_SPATIAL or something like that
# convert ebd to spatial object
coordinates(ebd_zf) <- ~longitude + latitude # 1-2 minutes for all of N.Amer.
# define projection for lat long (ebird documentation states CRS is 4326)
proj4string(ebd_zf) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# transform spatial object to sf
ebird_spatial <- sf::st_as_sf(ebd_zf)
# match proj to target proj
ebird_spatial <- st_transform(ebird_spatial, crs = CRS(paste0("+init=epsg:", crs.target)))
# merge ebird with the spatial grid
# this will take a minute
tic()
ebird_spatial <- grid %>%
  st_join(ebird_spatial)
toc()
tic()
test=st_intersection(grid, ebird_spatial)
toc()


# Ensure eBird and BBS Overlay as Expected ------------------------------------------------
plot(st_geometry(grid))
plot(st_geometry(ebird_spatial),add=TRUE)
plot(st_geometry(bbs_spatial),add=TRUE)

