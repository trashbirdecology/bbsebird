#' Create an SF object for eBird Observations
#'
#' Converts eBird (preferably zero-filled data) into a simple features spatial object.
#' @param df A data frame with columns c(lat, lon) and optional attributes (columns).
#' @param crs.target the integer representing the target CRS.
#' @param grid  a spatial grid over which the eBird data will be overlaid.
#' @param dir.out path to where the resulting spatial data frame will be saved. If NULL will not save to file.
#' @param overwrite logical if TRUE will overwrite any existing file named "ebird_spatial.rds" in path dir.out
#' @importFrom sp CRS coordinates proj4string
#' @importFrom sf st_as_sf st_transform
#' @importFrom dplyr mutate
#' @export make_ebird_spatial
make_ebird_spatial <- function(df, crs.target=4326, dir.out=NULL,
                               grid = NULL, overwrite=FALSE) {

  # first, if overwrite is false and this file exists. import and return asap.
  f <-paste0(dir.out, "ebird_spatial.rds")
  if(file.exists(f) & !overwrite){
    cat("File ", f," exists and overwrite.ebird = FALSE. Importing existing spatial ebird data from .RDS")
    ebird_spatial <-readRDS(f)
    return(ebird_spatial)
  }

  ## munge col names to ensure consitency
  df <-  munge_col_names(df)
  grid  <-  munge_col_names(grid)

  # convert ebird data frame to spatial object
  cat("assigning coordinates to `df`. takes a  minute for a few states' worth of ebird data.\n")

  # add attribute comprising the ebird coordinates. when i assign lat and lon as coordinates to the sf object, i lose them as attributes and
  ### theyre hard to recover when it becomes an SFC object... annnoying? yes.
  stopifnot(c("lon","lat")  %in% names(df))

  sp::coordinates(df) <-
    ~ lon + lat # 1-2 minutes for all of N.Amer.

  # define projection for lat long (ebird documentation states CRS is 4326)
  sp::proj4string(df) <-
    sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # transform spatial object to sf
  cat("coercing eBird data to sf object. \n may take between 1 and 20 minutes......lol sry...")
  df <- sf::st_as_sf(df)
   cat("\t...done.\n")
  # match proj to target proj
  cat("projecting or re-projecting the eBird data to match crs.target. \ntakes ~1 minute for smaller spatial extents...\n")
  ## set CRS
  crs.string <- sp::CRS(SRS_string = paste0("EPSG:", crs.target))
  df <-
    sf::st_transform(df, crs = crs.string)
  cat("\t...done\n")
  ## Exit function if no grid is provided
  if (is.null(grid)){
    cat("No `grid` provided. Returning ebird spatial data without grid.\n")
    return(df)}

  cat(
    "overlaying eBird and the spatial sampling grid. \ntakes ~1-2 min for a few states/provinces.\n"
  )

  sf::st_geometry(grid)<- names(grid)[1]

  # cat("Joining ebird to spatial grid. Takes at least a couple of minutes for smaller eBird datasets.\n")
  ebird_spatial <- sf::st_join(grid, df)
  ### maybe delete::must be done in this order to retain the 'grid cell id' numbers. Slightly slower than using

  #remove rownames
  rownames(ebird_spatial) <- NULL

  # SAve to file
  if(!is.null(dir.out)){
    cat("Writing to file: ", f, "\n")
    while(substr(f,1,1)=="/") f <-  substr(f,2, nchar(f))  ## in linux must remove leading /, idfk
    saveRDS(ebird_spatial, file=f)
}

  return(ebird_spatial)

}
