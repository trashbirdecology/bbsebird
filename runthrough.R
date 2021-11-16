# Working runthrough of the package
## using this script as a template for organizing the package.
## not sure exactly yet what functions are necessary/useful, so throwing the workflow in here and we will add as we go.

# Libraries ---------------------------------------------------------------
devtools::install_github("trashbirdecology/bbsassistant",ref="convert-to-50-stop", force=FALSE)
library(auk)
library(bbsAssistant)
library(dplyr)
library(stringr)


# Memory limits -----------------------------------------------------------
# adjust memory limit to handle large eBird data files depending on OS
if(.Platform$OS.type=="unix") library(unix)
if(.Platform$OS.type=="unix") unix::rlimit_as(1e12) # prob unnecessary but doesn't hurt (probably)
if(.Platform$OS.type=="windows") round(memory.limit()/2^20, 2)


# Directory specifications and creation ------------------------------------------------
# ebird data
  ## because the ebird data is so massive, it's inconvenient to store inside the package.
  ## specify dir.ebird.in as the location where you have saved the EBD (eBird reference database)
  dir.ebird.in <- "C:\\Users\\jburnett\\OneDrive - DOI\\research\\cormorants\\dubcorm-data-backup"
  ## AUK package maintainers suggest setting  this directory using auk::auk_set_ebd_path
  auk_set_ebd_path(dir.ebird.in, overwrite = TRUE)
  ## specify dir.ebird.out as the location where you will save local, post-munging ebird data (as .rda/.rds)
  dir.ebird.out <- "data-local/ebird"

  # BBS data
  ## bbs data is much smaller than eBird, so storing in the project directory is feasible
  ## specify dir.ebird.out as the location where you will save local, post-munging ebird data (as .rda/.rds)
  dir.bbs.out <- "data-local/bbs"

# GeoSpatial data
  dir.spatial.out <- "data-local/spatial"

  if(!"data-local" %in% list.files())dir.create("data-local")
  sapply(c(dir.ebird.out, dir.bbs.out, dir.spatial.out), dir.create)


# Data specifications -----------------------------------------------------
# Specify region(s), specie(S) and temporal period(s) to use for data subsetting, etc.
interest.spatial<-c("North America")
interest.species<-c("DCCO", "DCCOR", "Double-crested Cormorant", "Double Crested Cormorant") ## need to provide a lookup-table relating the ebird to BBS taxa, including codes
interest.temporal<-1970:2019
include.unid <- FALSE ## Whether or not to include UNIDENTIFIED // hybrid species

# Munge BBS data ----------------------------------------------------------
  bbs <- grab_bbs_data(sb_dir=dir.bbs.out, overwrite = TRUE) # defaults to most recent release of the BBS dataset available on USGS ScienceBase
  if(exists("sb_items"))rm(sb_items)

## Filter by species of interest. ### GO TO FUNCTION PROBABLY OR ADD TO BBSASSISTANT....
(bbs.species <- bbs$species_list %>% filter(str_detect(tolower(English_Common_Name), paste(tolower(interest.species), collapse="|"))))
if(!include.unid) bbs.species <- bbs.species %>% filter(!str_detect(tolower(English_Common_Name), "unid"))
message(cat("The following species are included in the BBS dataset: ",bbs.species$English_Common_Name))

bbs$observations[bbs$observations$AOU %in% bbs.species$AOU]
temp=unlist(bbs)


# Munge eBird data --------------------------------------------------------
  ebird <- NULL




