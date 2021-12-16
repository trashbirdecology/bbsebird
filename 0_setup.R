# Working runthrough of the package
## using this script as a template for organizing the package.
## not sure exactly yet what functions are necessary/useful, so throwing the workflow in here and we will add as we go.
rm(list=ls())

# Memory limits -----------------------------------------------------------
# adjust memory limit to handle large eBird data files depending on OS
# if(.Platform$OS.type=="unix") library(unix)
# if(.Platform$OS.type=="unix") unix::rlimit_as(1e12) # prob unnecessary but doesn't hurt (probably)
# if(.Platform$OS.type=="windows") memory.limit(35000)

# Libraries ---------------------------------------------------------------
## Installs ----------------------------------------------------------------
# install.packages ("stringi", type = "win.binary")
# install.packages ("readr", type = "win.binary")
devtools::install_github("trashbirdecology/bbsassistant",
                         ref="main",force=FALSE)
devtools::install_github("ropensci/rnaturalearthhires", force=FALSE) ## must install from GH -- source has unresolved issues for 2+years (see issue https://github.com/ropensci/rnaturalearthhires/issues/1)

## Package loads -----------------------------------------------------------------

### Common use-------------------
library(purrr) # for a function in munge shapefiles methinks need to check
# library(data.table)
# library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(lubridate)
library(tidyr)
library(hms) ## for some reason zerofiltering function in auk cannot find this fun?
library(lubridate)
library(vroom)
library(reshape2) # arrays
### Plotting and visualization
library(ggplot2)
library(leaflet)
library(mapview)
### Bird data-relevant-------------------
library(auk)
library(bbsAssistant)
### Spatial manipulation and analysis-------------------
library(rgdal)
library(spData) # world map
library(rnaturalearth) # state and prov data?
library(rnaturalearthhires) # ne_states needs this idfk why
library(sp) # not a core package, but used in munge_bbs_shapefiles() due to issues with CWS BBS Routes Geodatabase being too old
library(sf)
library(maps)
### Frivolous but useful-------------------
library(tictoc)
### TBD if necessary-------------------
library(doParallel)
library(mapproj)
library(multidplyr)
library(rgeos)
### Potential uses--------
# library(tmap)

# Data Munging -----------------------------------------------------
## Subsetting by species and region --------------------------------
interest.species <- c("DOCCOR", "DOCCO", "DCCO", "DCCOR", "Double-crested Cormorant", "Double Crested Cormorant") # to protect against changes in case, hyphenation
countries <- c("Canada","USA", "United States", "United States of America") # used to create base maps
region.remove = c("Alaska", "Hawaii", "Northwest Territories", "Yukon", "Nunavut", "Yukon Territory")
if(!exists("states")) states <- c("New York")
if(!exists("proj.shorthand")) proj.shorthand="ny"
# states <-
  # c( # full names for BBS data, ctry-state abbrev for ebird files.annoying?yes.
    # 'Iowa','US-IA',
    # 'Illinois', 'US-IL',
    # 'Indiana','US-IN',
    # 'Michigan','US-MI',
    # 'Minnesota','US-MN',
#     # 'New York','US-NY',
#     'Ohio','US-OH',
#     # 'Pennsylvania','US-PA',
    # 'Wisconsin','US-WI'
#     # 'Ontario', 'CA-ON',
#     NULL
# )


## BBS and eBird specifications--------------------------------
if(!exists("year.range")) year.range <- c(2008:year(Sys.Date()))
min.yday <- 91 # breeding season day start (day of year)
max.yday <- 245 # breeding season day end (day of year)


### ebird specs
ebird.protocol <- c("Traveling", "Stationary")
complete.checklists.only <- TRUE
max.effort.mins <-  3*60 ## arbitrary
max.effort.km   <-  5 #This is coarse also, typically 5km or less
max.num.observers <- 10
# max.effort.ha   <-  XX
mmyyyy <- "Oct-2021" # month and year of most recent ebird EBD/samp download in file.

### bbs specs
include.unid <- FALSE ## Whether or not to include UNIDENTIFIED // hybrid species



## Geospatial specifications --------------------------------
crs.target <- 4326 # 5070 =usgs//usa/alberts equal area; 4326=unprojected;

## Provide the value of the grid size IN DECIMAL DEGREES
### A good estimate for large-scale (>=state) studies in North Am.
### is that there are 111.111km in 1 degree latitude or longitude
#### miles to km: km=1.61*miles
# grid.size=111.111/111.111 # == 111.111 km grid size (or 1 degree lat/lon)
grid.size=.25 #1/4 deg

# Specify Directories & File Names -----------------------------------------------------
# dir.proj.out <- paste0("examples/greatlakes-subset-example-", grid.size*111.111, "km/")
dir.proj.out <- paste0("examples/", proj.shorthand,"-example-", round(grid.size*111.111), "km/")

## eBird Data Directory (MUST BE SPECIFIED) ----------------------------------
## where the eBird data is stored (locally)
if(!exists("dir.ebird.in")) dir.ebird.in <- "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird"

## BBS Route Shapefiles/GDBs -----------
if(!exists("cws.routes.dir")) cws.routes.dir <- "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/bbs/route_shapefiles/cws"
if(!exists("usgs.routes.dir")) usgs.routes.dir <- "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/bbs/route_shapefiles/usgs"

## JAGS data in and out -----------------------------------------------------
dir.jags <- paste0(dir.proj.out, "jags/")

## Output Directories (project-specific) -----------------------------------
## Intermediary data files--------------------------------------------------
dir.bbs.out <- paste0(dir.proj.out,"bbs/")
dir.ebird.out <- paste0(dir.proj.out,"ebird/")
dir.spatial.out <- paste0(dir.proj.out,"spatial/")
dir.plots <- paste0(dir.proj.out, "plots/")
sapply(c(dir.proj.out, dir.bbs.out, dir.ebird.out, dir.spatial.out, dir.jags, dir.plots), FUN=
         function(x) dir.create(x, showWarnings = FALSE))
# dir.exploratory.plots <- paste0(dir.plots, "exploratory/")


# Arguments to never remove from env --------------------------------------
## To help with memory issues, list of arguments that we want to keep.
args.save <- ls()
