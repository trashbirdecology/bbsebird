#' Create an SF object for eBird Observations
#'
#' Converts eBird (preferably zero-filled data) into a simple features spatial object.
#' @param df A data frame with columns c(latitude, longitude) and optional attributes (columns).
#' @param crs.target the integer representing the target CRS.
#' @param grid  a spatial grid over which the eBird data will be overlaid.
#' @param dir.out path to where the resulting spatial data frame will be saved. If NULL will not save to file.
#' @param overwrite logical if TRUE will overwrite any exsiting file named "ebird_spatial.rds" in path dir.out
#' @export
make_ebird_spatial <- function(df, crs.target, dir.out=NULL, grid = NULL, overwrite=FALSE) {

  # first, if overwrite is false and this file exists. import and return asap.
  f <-paste0(dir.out, "ebird_spatial.rds")
  if(file.exists(f) & !overwrite){
    cat("File ", f," exists and overwrite.ebird = FALSE. Importing spatial ebird data.")
    ebird_spatial <-readRDS(f)
    return(ebird_spatial)
  }

  ## need to add messages for when following assers fail
  assertthat::assert_that(is.numeric(crs.target)) ||
    assertthat::assert_that(is.integer(crs.target))
  assertthat::assert_that(any(c("tbl_df", "tbl", "data.frame") %in% class(df)))

  # convert ebird data frame to spatial object
  cat("assigning coordinates to `df`. takes a  minute for a few states' worth of ebird data.\n")

  # add attribute comprising the ebird coordinates. when i assign lat and lon as coordinates to the sf object, i lose them as attributes and
  ### theyre hard to recover when it becomes an SFC object... annnoying? yes.
  df <- df %>%
    dplyr::mutate(lon=longitude,
           lat=latitude) ## we want to duplicate because its easier to keep lat and lon in the df also

  sp::coordinates(df) <-
    ~ longitude + latitude # 1-2 minutes for all of N.Amer.


  # define projection for lat long (ebird documentation states CRS is 4326)
  sp::proj4string(df) <-
    sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # transform spatial object to sf
  cat("coercing eBird data to sf object. \n may take between 1 and 20 minutes......lol sry...")
  df <- sf::st_as_sf(df)
   cat("\t...done.\n")
  # match proj to target proj
  cat("projecting or re-projecting the eBird data to match crs.target. \ntakes ~1 minute for smaller spatial extents...\n")
  df <-
    sf::st_transform(df, crs = sp::CRS(paste0("+init=epsg:", crs.target)))
  cat("\t...done\n")
  ## Exit function if no grid is provided
  if (is.null(grid)){
    cat("No `grid` provided. Returning ebird spatial data without grid.\n")
    return(df)}

  cat(
    "overlaying eBird and the spatial sampling grid. \ntakes ~1-2 min for a few states/provinces.\n"
  )

  # # expand the grid to include all years and  grid cell ids
  ### not really sure why i wanted to do this..eh
  # grid <- grid %>%
  #   as.data.frame() %>%
  #   ## add years to the grid layer
  #   tidyr::expand(year = unique(df$year), gridcellid) %>%
  #   # add these to grid attributes attributes
  #   full_join(grid)%>%
  #   sf::st_as_sf()

  # cat("Joining ebird to spatial grid. Takes at least a couple of minutes for smaller eBird datasets.\n")
  ebird_spatial <- sf::st_join(grid, df)
  # ## must be done in this order to retain the 'grid cell id' numbers. Slightly slower than using
  # ebird_spatial <-
  #   grid.expanded %>%
  #   sf::st_join(df)
  #
  # # append the missing grid cell ids (this is a lot faster than st_joining the ebird_spatial and grid.expanded)
  # ebird_spatial <- full_join(ebird_spatial, grid.expanded)
  #
  # ebird_spatial <- ebird_spatial %>% filter(!is.na(year))

  #remove rownames
  rownames(ebird_spatial) <- NULL

  # SAve to file
  if(!is.null(dir.out)){
  cat("Writing to file: ", f, "\n")
  saveRDS(ebird_spatial, file=f)
}

  return(ebird_spatial)

}
