# Working runthrough of the package
## using this script as a template for organizing the package.
## not sure exactly yet what functions are necessary/useful, so throwing the workflow in here and we will add as we go.
rm(list=ls())
gc()# Working runthrough of the package
## using this script as a template for organizing the package.
## not sure exactly yet what functions are necessary/useful, so throwing the workflow in here and we will add as we go.
rm(list=ls())
gc()
# Libraries ---------------------------------------------------------------
devtools::install_github("trashbirdecology/bbsassistant",ref="convert-to-50-stop", force=FALSE)
devtools::install_github("ropensci/rnaturalearthhires") ## must install from GH -- source has unresolved issues for 2+years (see issue https://github.com/ropensci/rnaturalearthhires/issues/1)
library(bbsAssistant)
library(purrr) # for a function in munge shapefiles methinks need to check
library(tictoc)
library(auk)
library(hms) ## for some reason zerofiltering function in auk cannot find this fun?
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
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

# Directory to save munged data
dir.munged <- "data-local/"


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
states<- c("Florida")

# overwrite existing munged data files?
overwrite.existing.munged.data=FALSE ## used in 2_make-jags.r

# Libraries ---------------------------------------------------------------
devtools::install_github("trashbirdecology/bbsassistant",ref="convert-to-50-stop", force=FALSE)
devtools::install_github("ropensci/rnaturalearthhires") ## must install from GH -- source has unresolved issues for 2+years (see issue https://github.com/ropensci/rnaturalearthhires/issues/1)
library(bbsAssistant)
library(purrr) # for a function in munge shapefiles methinks need to check
library(tictoc)
library(auk)
library(hms) ## for some reason zerofiltering function in auk cannot find this fun?
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
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

# Directory to save munged data
dir.munged <- "data-local/"


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
states<- c("Florida", "Georgia", "South Carolina")

# overwrite existing munged data files?
overwrite.existing.munged.data=FALSE ## used in 2_make-jags.r
