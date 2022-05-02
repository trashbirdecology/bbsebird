#' Create a Base Sampling Grid for Use in Multiple Projects
#'
#' @param dir.out Where to save the resulting grid (as .RDS).
#' @param overwrite TRUE will overwrite existing `grid.rds` in `dir.out`
#' @param countries Vector of countries. Defaults to a base map of USA and CAN, unless arg `states` is provided. If arg `states` is provided, this argument will be ignored. Must be specified using ISO-A2
#' @param states Vector of states to which the spatial grid will be clipped. Must be specified using ISO 3166-2 (see \url{https://en.wikipedia.org/wiki/ISO_3166-2})
#' @param crs.target Target CRS number for spatial grid.
#' @param hexagonal logical if TRUE will produce a spatial grid with hexagonal, as opposed to rectangular, cells
#' @param grid.size numeric size (relative to units defining crs.target) of resulting cell. E.g., if crs.target==4326 a value of gridsize=1.0 equals ~111.11km
#' @importFrom rnaturalearth ne_states
#' @param ... Additional arguments
#' @param conterminousUSA logical if TRUE will remove Hawaii and Alaska. Default TRUE to reduce computation times and extent of sptaial grid.
#' @importFrom sf st_transform st_make_grid st_area st_make_grid
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @export make_base_grid
make_base_grid <- function(dir.out = NULL,
                              overwrite = FALSE,
                              countries = c("US", "CA"),
                              states    = NULL,
                              crs.target = 4326,
                              hexagonal = TRUE,
                              grid.size = 0.50,
                              conterminousUSA = TRUE,
                              ...) {
  # check arguments
  countries <- toupper(countries)
  if( "CAN" %in% countries) countries <- stringr::str_replace(countries, pattern="CAN", "CA")
  if( "USA" %in% countries) countries <- stringr::str_replace(countries, pattern="USA|U.S.A", "US")
  if (is.null(countries)) {
    countries <- c("US", "CA")
    cat(
      "argument `countries` is NULL--creating a grid across Canada and United States of America"
    )
  } ## this is messy -- should be improved to etiher throw a menu to select some countries or approve the north american approach...

  if(!is.null(states)) states <- gsub(x=toupper(states), pattern="-", replacement="")

  # if(is.null(dir.out)) dir.out <- "grids"
  # dir.create(dir.out, showWarnings = FALSE, recursive = TRUE)
  #
  grid.temp <- gsub(x=sub("^0+", "", grid.size),pattern="\\.", replacement="dot")
  if(is.null(dir.out)) dir.out <- "grids/"
  dir.create(dir.out, showWarnings = FALSE, recursive = TRUE)
  fn.out <- paste0(
    dir.out,
    "/",
    tolower(paste0(countries, collapse = "-")),
    "_",
    ifelse(hexagonal, "hex", "square"),
    "_crs",
    crs.target,
    "_gridsize",
    grid.temp, ".rds"
  )

  # If grid.rds already exists in the spatial files directory AND overwrite is FALSE, will just import the file.
  if (file.exists(fn.out)  & !overwrite) {
    cat("grid.rds already exists and overwrite=FALSE. Importing existing file....\n")
    grid <- readRDS(fn.out)
    return(grid) # exit function
  }

  # Begin by grabbing  all data to check arguments
  regions.avail <-
    rnaturalearth::ne_states() |> as.data.frame()

  regions.avail$states    <-
    toupper(gsub(
      x = regions.avail$iso_3166_2,
      pattern = "-",
      replacement = ""
    ))
  regions.avail$countries <-
    toupper(gsub(
      x = regions.avail$iso_a2,
      pattern = "-",
      replacement = ""
    ))

  #test
  if (!is.null(states))
    if (!all(states    %in% regions.avail$states)) {
      message(
        "the following regions weren't found. please check specification or remove from arg `states`: ",
        states[which(!states %in% regions.avail$states)],
        "\n"
      )
    }
  if (is.null(states))
    stopifnot(all(countries %in% regions.avail$countries))

  # Match states and countries to rnaturalearth::ne_states codes
  countries.ind <-
    unique(regions.avail$iso_a2[which(regions.avail$countries %in% countries)]) # grab countries to filter out in study.area
  states.ind    <-
    unique(regions.avail$iso_3166_2[which(regions.avail$states %in% states)]) # grab countries to filter out in study.area
  study.area <-
    rnaturalearth::ne_states(iso_a2 = countries.ind, returnclass = "sf")

  ## remove alaska and hawaii if conterminous ==TRUE
  if(conterminousUSA) study.area <- study.area[!tolower(study.area$name) %in% c("hawaii","alaska"
  ),]

  if (!is.null(states))
    study.area <- study.area |> filter(iso_3166_2 %in% states.ind)

  # crs transform
  study.area <- study.area |>
    sf::st_transform(study.area, crs = crs.target)

  # throw a grid over the study area layer
  cat("Creating spatial grid. \n")
  square <- ifelse(hexagonal, FALSE, TRUE)
  grid <- study.area |>
    sf::st_make_grid(cellsize = grid.size,
                     square = square,
                     flat_topped = TRUE,
                     what = "polygons")


  ## GRID.OVERLAY IS USEFUL FOR PLOTTING but NOT FOR ALIGNING BBS AND EBIRD DATA
  grid.overlay <- grid |>
    sf::st_intersection(study.area) |>
    # st_cast("MULTIPOLYGON") |>
    sf::st_sf() |>
    dplyr::mutate(gridcellid = row_number()) |>
    sf::st_transform(crs = crs.target)

  grid <- grid[study.area] |>
    sf::st_sf() |>
    dplyr::mutate(gridcellid = row_number()) |>
    sf::st_transform(crs = crs.target)

  # Calculate and add the grid cell centroid to the sf
  suppressWarnings(xy <-
                     sf::st_coordinates(sf::st_geometry(sf::st_centroid(grid))))
  ### This warning is supposed to be regarding calculating centroids on a LAT LON CRS, but I've tried with both PCRS and UNProj-CRS and sitll get the warning..
  grid$cell.lon.centroid <- xy[, 1]
  grid$cell.lat.centroid <- xy[, 2]
  grid$area <- sf::st_area(grid)

  grid <- list(grid=grid, overlay = grid.overlay)
  if(exists("xy.nb")) grid$neighborhood <-spdep::nb2WB(xy.nb)
  # Export Data
  cat("Saving spatial grid as .RDS to file: ", fn.out)
  saveRDS(grid, file = fn.out)

  return(grid)
}
