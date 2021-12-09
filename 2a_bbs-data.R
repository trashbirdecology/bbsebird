if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("1_spatial-grid.r")
devtools::load_all()

fns <- list.files(dir.bbs.out)
fns.spatial <- list.files(dir.spatial.out)
# Check for existing files ------------------------------------------------
if("bbs_spatial.rds" %in% tolower(fns.spatial)) bbs_spatial <- readRDS(paste0(dir.spatial.out, "/", "bbs_spatial.rds"))else{

# Munge BBS data ----------------------------------------------------------
## Import and/or Download BBS Observations and Metadata -----------------------------
#### Original observations data
if("bbs_orig.rds" %in% tolower(fns)){
  bbs_orig <-
    readRDS(paste0(dir.bbs.out, "/bbs_orig.rds"))} else{
      print("grabbing bbs data, this might take 45sec")
      bbs_orig <- grab_bbs_data(sb_dir = dir.bbs.out)
      saveRDS(bbs_orig, paste0(dir.bbs.out, "/bbs_orig.rds"))
}

#### Munged observations data
if (!"bbs_obs.rds" %in% tolower(fns)) {
  bbs_obs <-
    munge_bbs(
      list = bbs_orig,
      spp = interest.species,
      states=states,
      keep.stop.level.data = FALSE
    )
  saveRDS(bbs_obs, paste0(dir.bbs.out, "/bbs_obs.rds"))
} else{
  bbs_obs <- readRDS(paste0(dir.bbs.out, "/bbs_obs.rds"))
}


## Make BBS Spatial Layers ----------------------------------------------
### Create BBS routes spatial layer ----------------------------------------------------------------------
# munges all routes at first then moves to subsetting to the grid, so have patience
# takes about a minute for 1-3 states
cat("Munging the BBS route shapefiles/spatial layer.\nIf `grid` specified, will take a hot minute.\nWILL SOON PASTE ALGEBRA TO MESSAGE OUT ESTIMATED TIME BASED ON STATES AND GRID CELL SIZE")
bbs_spatial <-
  make_bbs_spatial(
    bbs.obs = bbs_obs,
    cws.routes.dir = cws.routes.dir, #location of the CWS BBS routes shapefiles
    usgs.routes.dir = usgs.routes.dir, #location of the USGS BBS routes shapefiles
    crs.target = crs.target,
    routes.keep=unique(bbs_obs$RTENO),
    grid=grid,
    keep.empty.cells =TRUE,
    plot.dir=dir.exploratory.plots,
    overwrite=TRUE # wanna overwrite existing bbs_routes in workspace? if
  )

saveRDS(bbs_spatial, paste0(dir.spatial.out, "/bbs_spatial.rds"))
}


# Clear mem ---------------------------------------------------------------------
if(exists("args.save")){
  args.save <- c(args.save, "bbs_spatial")
rm(list=setdiff(ls(), args.save))
}
