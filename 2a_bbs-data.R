if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("1_spatial-grid.r")
devtools::load_all()

fns <- list.files(dir.bbs.out)

# Check for existing files ------------------------------------------------
if("bbs_spatial.rds" %in% tolower(fns)) bbs_spatial <- readRDS(paste0(dir.spatial.out, "/", "bbs_spatial.rds"))else{

# Munge BBS data ----------------------------------------------------------
## Import and/or Download BBS Observations and Metadata -----------------------------
#### Original observations data
if("bbs_orig.rds" %in% tolower(fns)) bbs_orig <-
    readRDS(paste0(dir.bbs.out, "/bbs_orig.rds"))
else{
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
      zero.fill = TRUE,
      active.only = TRUE,
      keep.stop.level.data = FALSE
    )
  saveRDS(bbs_obs, paste0(dir.bbs.out, "/bbs_obs.rds"))
} else{
  bbs_obs <- readRDS(paste0(dir.bbs.out, "/bbs_obs.rds"))
}
rm(bbs_orig)

## Make BBS Spatial Layers ----------------------------------------------
### Create BBS routes spatial layer ----------------------------------------------------------------------
# munges all routes at first then moves to subsetting to the grid, so have patience
# takes about a minute for 1-3 states

if(!"bbs_routes.rds" %in% tolower(fns)){
message("Munging the BBS route shapefiles/spatial layer.
        If `grid` specified, will take a hot minute.
        WILL SOON PASTE ALGEBRA TO MESSAGE OUT ESTIMATED TIME BASED ON STATES AND GRID CELL SIZE")
bbs_routes <-
  munge_bbs_shapefiles(
    cws.routes.dir = cws.routes.dir, #location of the CWS BBS routes shapefiles
    usgs.routes.dir = usgs.routes.dir, #location of the USGS BBS routes shapefiles
    crs.target = crs.target,
    routes.keep=unique(bbs$RTENO),
    grid=grid,
    overwrite=TRUE # wanna overwrite existing bbs_routes in workspace?
  )
saveRDS(bbs_routes, paste0(dir.bbs.out, "/bbs_routes.rds"))
}else{readRDS(paste0(dir.bbs.out, "/bbs_routes.rds"))}


# if grid was provided in munge_bbs_shapefiles, skip step
if(!"id" %in% tolower(names(bbs_routes))) {
  # overlays bbs routes to grid, if not already done.
  bbs_routes <- st_intersection(grid, bbs_routes)
}


### Add BBS observations data to the routes+grid layer-------------------------------------------------------------------------
### Minor issue, but Route Names in route shaepfiles do not match the BBS observations data route names.
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

# as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))


# Export Data -------------------------------------------------------------
saveRDS(bbs_spatial, file = paste0(dir.spatial.out, "/", "bbs_spatial.rds"))


}
# Clear mem ---------------------------------------------------------------------
args.save <- c(args.save, "bbs_spatial")
rm(list=setdiff(ls(), args.save))

