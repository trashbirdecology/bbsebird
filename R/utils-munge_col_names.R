#' Munge Column Names for Bird Observations Data
#' @param data flat data object with column names
#' @export munge_col_names
munge_col_names <- function(data){

index = ifelse(("sf" %in% class(data)), "spatial", "not")
if(index == "spatial"){
  # treat spatial data frames a little differently to avoid losing the saptia features
 return(data)

}
col_names <- list(
  date = c("observation_date", "date"),
  c =    c("observation_count", "count", "routetotal"),
  lat  = c("lati", "latitude", "cell_lat_centroid", "cell_lati_centroid","cell.lat.centroid", "cell.lati.centroid"),
  lon = c("longitude", "long", "cell_lon_centroid", "cell_long_centroid","cell.lon.centroid", "cell.long.centroid")
)


for(i in seq_along(col_names)){
  oldnames <- col_names[[i]]
  newnames <- rep(names(col_names)[i], length=length(oldnames))
  data.table::setnames(data, oldnames, newnames,skip_absent = TRUE)} # do not reassign, saves in place

## if its spatial, treat a little differently
# if(index == "spatial"){
#   geom.col <-  attr(data, "sf_column")
#   names(data)[names(data)==geom.col] <-  "geometry"
#   data <-  sf::st_set_geometry(data,"geometry")
# }

names(data) <- tolower(gsub(x = names(data),pattern = " ", replacement = "_")) # replace spaces
names(data) <- tolower(gsub(x = names(data),pattern = "\\.", replacement = "_")) # replace periods

return(data)
}
