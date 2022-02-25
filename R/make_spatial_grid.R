#' Create a Spatial Sampling Grid
#'
#' @param dir.out Where to save the resulting grid (as .RDS).
#' @param overwrite TRUE will overwrite existing `grid.rds` in `dir.out`
#' @param countries Vector of countries. Defaults to a base map of USA and CAN, unless arg `states` is provided. If arg `states` is provided, this argument will be ignored. Must be specified using ISO-A2
#' @param states Vector of states to which the spatial grid will be clipped. Must be specified using ISO 3166-2 (see \url{https://en.wikipedia.org/wiki/ISO_3166-2})
#' @param crs.target Target CRS number for spatial grid.
#' @param hexagonal logical if TRUE will produce a spatial grid with hexagonal, as opposed to rectangular, cells
#' @param grid.size numeric size (relative to units defining crs.target) of resulting cell. E.g., if crs.target==4326 a value of gridsize=1.0 equals ~111.11km
#' @importFrom rnaturalearth ne_states
#' @importFrom sf st_transform st_make_grid st_area st_make_grid
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @export make_spatial_grid
make_spatial_grid <- function(dir.out,
                              overwrite = TRUE,
                              countries = c("US", "CA"),
                              states    = NULL,
                              crs.target = 4326,
                              hexagonal=TRUE,
                              grid.size
                              ){
# check arguments
if(is.null(countries)){   countries <- c("US", "CA")
cat("argument `countries` is NULL--creating a grid across Canada and United States of America")
} ## this is messy -- should be improved to etiher throw a menu to select some countries or approve the north american approach...

# If grid.rds already exists in the spatial files directory AND overwrite is FALSE, will just import the file.
if("grid.rds" %in% list.files(dir.out) & !overwrite){
  grid <- readRDS(paste0(dir.out, "/", "grid.rds"))
  return(grid) # exit function
}else{cat("Making spatial sampling grid.")}

# Begin by grabbing  all data to check arguments
regions.avail <-
    rnaturalearth::ne_states() %>% as.data.frame()

regions.avail$states    <- toupper(gsub(x=regions.avail$iso_3166_2, pattern="-", replacement=""))
regions.avail$countries <- toupper(gsub(x=regions.avail$iso_a2, pattern="-", replacement=""))

#test
if(!is.null(states)) if(!all(states    %in% regions.avail$states)){message("the following regions weren't found. please check specification or remove from arg `states`: ", states[which(!states %in% regions.avail$states)], "\n")}
if(is.null(states))  stopifnot(all(countries %in% regions.avail$countries))

# Match states and countries to rnaturalearth::ne_states codes
countries.ind <- unique(regions.avail$iso_a2[which(regions.avail$countries %in% countries)]) # grab countries to filter out in study.area
states.ind    <- unique(regions.avail$iso_3166_2[which(regions.avail$states %in% states)]) # grab countries to filter out in study.area

study.area <- rnaturalearth::ne_states(iso_a2 = countries.ind, returnclass="sf")

if(!is.null(states)) study.area <- study.area %>% filter(iso_3166_2 %in% states.ind)

# crs transform
study.area <- study.area %>%
    sf::st_transform(study.area, crs = crs.target)

# throw a grid over the study area layer
square = ifelse(hexagonal, FALSE, TRUE)
grid <- study.area %>%
  sf::st_make_grid(cellsize = grid.size,
                   square = FALSE,
                   flat_topped = TRUE) %>%
  sf::st_intersection(study.area) %>%
  # st_cast("MULTIPOLYGON") %>%
  sf::st_sf() %>%
  dplyr::mutate(gridcellid = row_number()) %>%
  sf::st_transform(crs = crs.target)

# Calculate and add the grid cell centroid to the sf
suppressWarnings(centroid.coords <- sf::st_coordinates(sf::st_geometry(sf::st_centroid(grid))))
### This warning is supposed to be regarding calculating centroids on a LAT LON CRS, but I've tried with both PCRS and UNProj-CRS and sitll get the warning..
grid$cell.lon.centroid <- centroid.coords[,1]
grid$cell.lat.centroid <- centroid.coords[,2]
grid$area <- sf::st_area(grid)

# Export Data
fn <- paste0(dir.out, "/", "grid.rds")
fn <- stringr::str_replace(fn, "//", "/")
cat("Saving spatial grid as .RDS to file: ", fn)
saveRDS(grid, file = fn)




return(grid)
}
