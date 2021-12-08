#' Create an SF object for eBird Observations
#'
#' Converts eBird (preferably zero-filled data) into a simple features spatial object.
#' @param x A data frame with columns c(latitude, longitude) and other attributes
#' @param crs.target the integer representing the target CRS.
#' @param grid  a spatial grid over which the eBird data will be overlaid.
#' @export make_ebird_spatial


make_ebird_spatial <- function(x, crs.target, grid = NULL) {
  ## need to add messages for when following assers fail
  assertthat::assert_that(is.numeric(crs.target)) ||
    assertthat::assert_that(is.integer(crs.target))
  assertthat::assert_that(any(c("tbl_df", "tbl", "data.frame") %in% class(x)))

  # convert ebird data frame to spatial object
  coordinates(x) <-
    ~ longitude + latitude # 1-2 minutes for all of N.Amer.

  # define projection for lat long (ebird documentation states CRS is 4326)
  proj4string(x) <-
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # transform spatial object to sf
  cat("coercing eBird data to sf object. \nshould take 1-2 minutes\n")
  x <- sf::st_as_sf(x)

  # match proj to target proj
  cat("projecting or re-projecting the eBird data to match crs.target. \ntakes ~1 minute.\n")
  x <-
    st_transform(x, crs = CRS(paste0("+init=epsg:", crs.target)))

  ## Exit function if no grid is provided
  if (is.null(grid))
    return(x)

  cat(
    "overlaying eBird and the spatial sampling grid. \ntakes ~1-2 min for a few states/provinces. "
  )
  ebird_spatial <-
    grid %>% #  37sec for Ohio//2min for FL,GA,SC//2 min for Great Lakes
    st_join(x)

  return(ebird_spatial)

}
