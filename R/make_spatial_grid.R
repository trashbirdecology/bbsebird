#' Create a Spatial Sampling Grid
#'
#' @param dir.out Where to save the resulting grid (as .RDS).
#' @param overwrite TRUE will overwrite existing `grid.rds` in `dir.out`
#' @param countries Vector of countries. Defaults to a base map of USA and CAN, unless arg `states` is provided.
#' @param states Vector of states to which the spatial grid will be clipped.
#' @export make_spatial_grid
make_spatial_grid <- function(dir.out,
                              overwrite=TRUE,
                              states = NULL,
                              countries = c("USA", "CAN", "CA", "United States", "Canada", "United States of America")
                              ){
 # If grid.rds already exists in the spatial files directory AND overwrite is FALSE, will just import the file.
  if("grid.rds" %in% list.files(dir.spatial.out) & !overwrite){
  grid <- readRDS(paste0(dir.spatial.out, "/", "grid.rds"))
  return(grid) # exit function
}


# Begin by grabbing national boundaries
study.area <-
    rnaturalearth::ne_states(country = countries, returnclass = "sf")

if(!is.null(states)) study.area <- study.area %>%
  filter(tolower(name) %in% tolower(states)) ## couldnt get this conditional filter to work inside a full pipe.

study.area <- study.area %>%
    sf::st_transform(study.area, crs = crs.target)

# throw a grid over the study area layer
grid <- study.area %>%
  sf::st_make_grid(cellsize = grid.size,
                   square = FALSE,
                   flat_topped = TRUE) %>%
  sf::st_intersection(study.area) %>%
  # st_cast("MULTIPOLYGON") %>%
  sf::st_sf() %>%
  mutate(gridcellid = row_number()) %>%
  sf::st_transform(crs = crs.target)

  # add the grid cell area as a variable
  grid$area <- sf::st_area(grid)

#   # # Visualize to check
#   # tmap::qtm(grid)
# if(interactive.map)  mapview::mapview(grid) # interactive, openstreetmap
#
# Add centroid lat lon to grid
centroid.coords <- sf::st_coordinates(sf::st_geometry(sf::st_centroid(grid)))
grid$cell.lon.centroid <- centroid.coords[,1]
grid$cell.lat.centroid <- centroid.coords[,2]
grid$area <- st_area(grid)

# Export Data
fn <- paste0(dir.spatial.out, "/", "grid.rds")
fn <- stringr::str_replace(fn, "//", "/")
cat("Saving spatial grid as .RDS to file: ", fn)
saveRDS(grid, file = fn)

return(grid)
}
