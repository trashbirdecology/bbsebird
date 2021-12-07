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



# For good measure, ensure all the spatial files are in same proj ---------
