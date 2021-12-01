rm(list=ls())
source("0_setup.r")
# Original Data --------------------------------------------------------------------
# check dir.munged for files, if DNE, source script
fns <- list.files(dir.munged, full.names=TRUE)

ebird.fn <- fns[str_detect(fns, "ebird_spatial")]
bbs.fn <- fns[str_detect(fns, "bbs_spatial")]
grid.fn <- fns[str_detect(fns, "grid")]
munging.fn <- fns[str_detect(list.files(), "1_")]

## create or import the data--------------
if(all(is.null(ebird.fn))) source(munging.fn) else{
  # can't figure out how to source an entire script while allowing the menu to work...
  # choice=menu(title = paste0("Munged data files already exist in ", dir.munged,". Do you want to overwrite?"), choice=c("Overwrite existing data (this will take a while!)", "Do not overwrite and import existing files"))
  # if(choice==1) source(list.files(dir.munged, "data-mung"))
  if(overwrite.existing.munged.data) source(list.files(dir.munged, "data-mung"))
  # if(choice==2){
  if(!overwrite.existing.munged.data)
    ebird_spatial <- readRDS(ebird.fn)
    bbs_spatial <- readRDS(bbs.fn)
    grid <- readRDS(grid.fn)
  }

# Create JAGS data ------------------------------------------------
## JAGS data will be split up into BBS and eBird, mostly because the eBird data can be cumbersome
## Make BBS for Jags -----------------------------
bbs_jags <- NULL

## Make eBird for Jags -----------------------------

