# Working runthrough of the package
## using this script as a template for organizing the package.
## not sure exactly yet what functions are necessary/useful, so throwing the workflow in here and we will add as we go.
rm(list=ls())
gc()
# Libraries ---------------------------------------------------------------
devtools::install_github("trashbirdecology/bbsassistant",ref="convert-to-50-stop", force=FALSE)
library(auk)
library(hms) ## for some reason zerofiltering function in auk cannot find this fun?
library(bbsAssistant)
library(dplyr)
library(stringr)
library(rgdal)
library(sp)
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
  include.unid <- FALSE ## Whether or not to include UNIDENTIFIED // hybrid species
  map.region <- c("Canada","USA", "Mexico") # used to create base maps and spatial grid system.
  # grid.size <- c(1, "deg")
  grid.size <- c(6, "km")

# Munge eBird data --------------------------------------------------------
  # The ebird data should be in directory ebird.data.in.
    ## Functions in this section will import, munge, and save those files as .rds.
    ## Currently, this script / package does not use Auk to manipulate the ebird data using awk.
    ## Auk seems significantly slower than just importing as .txt and munging, at least when working with a single species
devtools::load_all()

# Get the list of potential files for import. This will be used in get_ebird()
(fn_zf <- list.files(dir.ebird.out, "rds"))
fns <- id_ebird_files(dir.ebird.in = dir.ebird.in)
# get_ebird()
# ebd_zf <- readRDS(fn_zf)



# Munge BBS route shapefiles ----------------------------------------------
bbs_routes_sldf <- munge_bbs_shapefiles(cws.routes.dir = cws.routes.dir,
                                        usgs.routes.dir = usgs.routes.dir,
                                        proj.target = "USGS")



# Munge BBS data ----------------------------------------------------------
bbs <- grab_bbs_data(sb_dir=dir.bbs.out, overwrite = TRUE) # defaults to most recent release of the BBS dataset available on USGS ScienceBase
if(exists("sb_items"))rm(sb_items) # id like to add an arg to grab_bbs_data that prevents output of sb_items...

# filter by species of interest
bbs.subset<-filter_bbs_by_species(bbs, search = interest.species)


# Spatial Grid System ------------------------------------------------------------
## I think i want to make this a function...

## create map data for N. America
for(i in seq_along(region)){
 if(i==1){map.df<-list()}
 map.df[[i]] <- map_data("world", region=region[i])
 if(i==length(region)){
   names(map.df)<-region
   map.df=bind_rows(map.df)
   }
}
## convert data frame to sf object
n.amer <- SpatialPointsDataFrame(
  coords = map.df[, c("long", "lat")],
  data = map.df,
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
  spTransform(proj4string(bbs_routes_sldf))

## create grid
# x=spsample(x=n.amer, n=1000, "regular")
xy <- expand.grid(x = n.amer$long, y = n.amer$lat)
plot(xy)


# extent.bbs <- sp::bbox(bbs_routes_sldf)

