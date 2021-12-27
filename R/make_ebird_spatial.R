#' Create an SF object for eBird Observations
#'
#' Converts eBird (preferably zero-filled data) into a simple features spatial object.
#' @param df A data frame with columns c(latitude, longitude) and optional attributes (columns).
#' @param crs.target the integer representing the target CRS.
#' @param grid  a spatial grid over which the eBird data will be overlaid.
#' @export
make_ebird_spatial <- function(df, crs.target, grid = NULL) {
  ## need to add messages for when following assers fail
  assertthat::assert_that(is.numeric(crs.target)) ||
    assertthat::assert_that(is.integer(crs.target))
  assertthat::assert_that(any(c("tbl_df", "tbl", "data.frame") %in% class(df)))

  # convert ebird data frame to spatial object
  cat("assigning coordinates to `df`. takes a  minute for a few states' worth of ebird data.\n")

  # add attribute comprising the ebird coordinates. when i assign lat and lon as coordinates to the sf object, i lose them as attributes and
  ### theyre hard to recover when it becomes an SFC object... annnoying? yes.
  df <- df %>%
    mutate(lon=longitude,
           lat=latitude) ## we want to duplicate because its easier to keep lat and lon in the df also

  coordinates(df) <-
    ~ longitude + latitude # 1-2 minutes for all of N.Amer.


  # define projection for lat long (ebird documentation states CRS is 4326)
  proj4string(df) <-
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # transform spatial object to sf
  cat("coercing eBird data to sf object. \n may take between 2 and 20 minutes (rofl u like that estimate?!).\n")
  df <- sf::st_as_sf(df)

  # match proj to target proj
  cat("projecting or re-projecting the eBird data to match crs.target. \ntakes ~1 minute.\n")
  df <-
    st_transform(df, crs = CRS(paste0("+init=epsg:", crs.target)))

  ## Exit function if no grid is provided
  if (is.null(grid)){return(df)}

  cat(
    "overlaying eBird and the spatial sampling grid. \ntakes ~1-2 min for a few states/provinces.\n"
  )
  # tic()
  # ebird_spatial <- st_join(df, grid)
  #   grid %>%
  #   st_join(df)
  # # toc()
  ebird_spatial <- st_join(grid, df) # should produce empty cells where no counts exist for that cell


  return(ebird_spatial)

}
