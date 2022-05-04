#' Munge Column Names for Bird Observations Data
#' @param data flat data object with column names
#' @export munge_col_names
munge_col_names <- function(data){

names(data) <- tolower(gsub(x = names(data),pattern = " ", replacement = "_"))

col_names <- list(
  date = c("observation_date", "date"),
  c =    c("observation_count", "count", "routetotal"),
  lat  = c("lati", "latitude", "cell.lat.centroid", "cell.lati.centroid"),
  lon = c("longitude", "long", "cell.lon.centroid", "cell.long.centroid")
)
for(i in seq_along(col_names)){
  oldnames <- col_names[[i]]
  newnames <- rep(names(col_names)[i], length=length(oldnames))
  data.table::setnames(data, oldnames, newnames,skip_absent = TRUE) # do not reassign, saves in place
}
return(data)
}
