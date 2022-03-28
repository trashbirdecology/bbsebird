#' # BORROWED FROM SO ANSWER https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
#' # Coordinate examples with expected UTM values
#' coord_sample <- data.frame(
#'   "Northing" = c(1105578.589, 5540547.370),
#'   "Easting" = c(609600.773, 643329.124),
#'   "Latitude" = c(10, 50),
#'   "Longitude" = c(118, 119))
#' 
#' #' Find UTM EPSG code from Latitude and Longitude coordinates (EPSG 4326 WGS84)
#' #' (vectorised)
#' #' @param lon longitudinal coordinate(s) in decimal degrees 
#' #' @param lon latitudinal coordinate(s) in decimal degrees 
#' #' @source: https://geocompr.robinlovelace.net/reproj-geo-data.html
#' #' @export latlong_to_utm
#' #' @source: https://gis.stackexchange.com/questions/13291/computing-utm-zone-from-lat-long-point
#' latlon_to_utm <- function(lon, lat) {
#'   xy <- data.frame(lon, lat)
#'   xy <- sf::st_as_sf(xy, coords = c("lon", "lat"),
#'                      crs = 4326)
#'   
#'   xy <- xy %>%
#'     do(cbind(., sf::st_coordinates(.)))
#'   
#'   lon = xy$X
#'   lat = xy$Y
#'   
#'   zone_number <- (floor((lon + 180) / 6) %% 60) + 1
#'   
#'   # Special zones for Norway
#'   cond_32 <- lat >= 56.0 & lat < 64.0 & lon >= 3.0 & lon < 12.0
#'   zone_number[cond_32] <- 32
#'   
#'   # Special zones for Svalbard
#'   cond_lat <- lat >= 72.0 & lat < 84.0
#'   
#'   cond_31 <- cond_lat & lon >= 0.0 & lon <  9.0
#'   zone_number[cond_31] <- 31
#'   
#'   cond_33 <- cond_lat & lon >= 9.0 & lon < 21.0
#'   zone_number[cond_33] <- 33
#'   
#'   cond_35 <- cond_lat & lon >= 21.0 & lon < 33.0
#'   zone_number[cond_35] <- 35
#'   
#'   cond_37 <- cond_lat & lon >= 33.0 & lon < 42.0
#'   zone_number[cond_37] <- 37
#'   
#'   # EPSG code
#'   utm <- zone_number
#'   utm[lat > 0] <- utm[lat > 0] + 32600
#'   utm[lat <= 0] <- utm[lat <= 0] + 32700
#'   return(utm)
#' }
#' 
#' 
#' utms <- latlon_to_utm(lon = grid$cell.lon.centroid, lat = grid$cell.lat.centroid)
#' 
#' test=grid %>%
#'   mutate(EPSG = utms) %>%
#'   group_by(EPSG) %>%
#'   do(cbind(as.data.frame(.) %>% select(cell.lon.centroid, cell.lat.centroid),
#'            sf::st_coordinates(sf::st_transform(., crs = head(.$EPSG, 1))))) %>%
#'   ungroup()
#' 
#' head(test)
