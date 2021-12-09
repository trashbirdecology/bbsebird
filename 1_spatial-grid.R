if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("0_setup.R")
devtools::load_all()


# Check for existing grid -------------------------------------------------
## If a grid.rds is saved in project directory, will not run script but will
## load in that rds.

if("grid.rds" %in% list.files(dir.spatial.out)) grid <- readRDS(paste0(dir.spatial.out, "/", "grid.rds"))else{
# Create a spatial grid -----------------------------------------------------------
# grid.size = c(diam.km*1000, diam.km*1000) ## convert km to m //
study.area <-
  ne_states(country = countries, returnclass = "sf") %>%
  # remove region(s)
  filter(tolower(name) %in% tolower(states)) %>%
  filter(!tolower(name) %in% tolower(region.remove)) %>%
  st_transform(study.area, crs = crs.target)
# unique(study.area$adm0_a3) #should add a test here to make sure number of countries expected is grabbed.

# throw a grid over the study area layer
grid <- study.area %>%
  st_make_grid(cellsize = grid.size,
               square = FALSE,
               flat_topped = TRUE) %>%
  st_intersection(study.area) %>%
  # st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number()) %>%
  st_transform(crs = crs.target)

# # Visualize to check
# tmap::qtm(grid)
# mapview::mapview(grid) # interactive, openstreetmap


# Add centroid lat lon to grid --------------------------------------------
centroid.coords <- st_coordinates(st_geometry(st_centroid(grid)))
grid$cell.lon.centroid <- centroid.coords[,1]
grid$cell.lat.centroid <- centroid.coords[,2]


# Clear junk ----------------------------------------------------------
rm(study.area)
gc()


# Export Data -------------------------------------------------------------
saveRDS(grid, file = paste0(dir.spatial.out, "/", "grid.rds"))

}

# Clear mem ---------------------------------------------------------------------
args.save <- c(args.save, "grid")
rm(list=setdiff(ls(), args.save))
