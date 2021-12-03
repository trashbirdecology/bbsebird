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
devtools::install_github("trashbirdecology/bbsassistant",
                         ref="major-update-50stop",
                         force=FALSE)
devtools::install_github("ropensci/rnaturalearthhires", force=FALSE) ## must install from GH -- source has unresolved issues for 2+years (see issue https://github.com/ropensci/rnaturalearthhires/issues/1)

## Package loads -----------------------------------------------------------------
### Common use-------------------
library(purrr) # for a function in munge shapefiles methinks need to check
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(hms) ## for some reason zerofiltering function in auk cannot find this fun?
library(lubridate)
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
# states <- c("Florida")
states <-
  c( # full names for BBS data, ctry-state abbrev for ebird files.annoying?yes.
    'Iowa','US-IA',
    'Illinois', 'US-IL',
    'Indiana','US-IN',
    'Michigan','US-MI',
    'Minnesota','US-MN',
    # 'New York','US-NY',
    'Ohio','US-OH',
    # 'Pennsylvania','US-PA',
    'Wisconsin','US-WI',
    'Ontario', 'CA-ON'
)


## BBS and eBird specifications--------------------------------
ebird.protocol <- c("Traveling", "Stationary")
complete.checklists.only <- TRUE

include.unid <- FALSE ## Whether or not to include UNIDENTIFIED // hybrid species

## Geospatial specifications --------------------------------
crs.target <- 4326 # 5070 =usgs//usa/alberts equal area; 4326=unprojected;

## Provide the value of the grid size in decimal degrees
### A good estimate for large-scale (>=state) studies in North Am.
### is that there are 111.111km in 1 degree latitude or longitude
#### miles to km: km=1.61*miles
grid.size=25/111.111 #


# Specify Directories & File Names -----------------------------------------------------
dir.proj.out <- "examples/greatlakes-example-25km/"

## Original Observations Data (for import and munging BBS, eBird) ----------------------------------
## specify dir.ebird.in as the location where you have saved the EBD (eBird reference database)
dir.ebird.in <- "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird"
auk_set_ebd_path(dir.ebird.in, overwrite = TRUE)
# Filenames for eBird data
fn_samp <- paste0(dir.ebird.in, "/ebd_sampling_relSep-2021.txt")
fns_ebd <- list.files(dir.ebird.in) #no need for fullnames because the auk package doesnt handle it well. needs a filename and a directory.
# Example using great lakes region. Example uses multple ebd files (because munging the entire EBD is A LOT...)
fns_ebd <- fns_ebd[grepl("(?=.*doccor)(?=.*Sep)(?=.*txt)", fns_ebd, perl = TRUE)]
fns_ebd <- fns_ebd[!str_detect(fns_ebd, c("US_|CA_|sampling"))] # remove the national-level data (too cumbersome)
fns_ebd <- fns_ebd[str_detect(fns_ebd, paste(states, collapse="|"))] # keep only the states we need.


## BBS Route Shapefiles/GDBs -----------
cws.routes.dir="C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/bbs/route_shapefiles/cws"
usgs.routes.dir="C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/bbs/route_shapefiles/usgs"

## Output Directories (project-specific) -----------------------------------
## Intermediary data files--------------------------------------------------
dir.bbs.out <- paste0(dir.proj.out,"/bbs/")
dir.ebird.out <- paste0(dir.proj.out,"/ebird/")
dir.spatial.out <- paste0(dir.proj.out,"/spatial/")
sapply(c(dir.proj.out, dir.bbs.out, dir.ebird.out, dir.spatial.out), FUN=
         function(x) dir.create(x, showWarnings = FALSE))



# Arguments to never remove from env --------------------------------------
## To help with memory issues, list of arguments that we want to keep.
args.save <- ls()
