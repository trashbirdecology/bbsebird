#' Functions for converting longitude and latitude to UTM zones and coordinates.
#' @param longitude vector of X coordinate(s)
#' @param latitude vector of Y coordinate(s)
#' @source https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
#' @export find_UTM_zone
find_UTM_zone <- function(longitude, latitude) {

  stopifnot(length(longitude)==length(latitude))

  # Special zones for Svalbard and Norway
  if (all(latitude >= 72.0) && all(latitude < 84.0 ))
    if (all(longitude >= 0.0)  && all(longitude <  9.0))
      return(31);
  if (all(longitude >= 9.0)  && all(longitude < 21.0))
    return(33)
  if (all(longitude >= 21.0) && all(longitude < 33.0))
    return(35)
  if (all(longitude >= 33.0) && all(longitude < 42.0))
    return(37)

  return((floor((longitude + 180) / 6) %% 60) + 1)
}


# LL to UTM ---------------------------------------------------------------
#' Convert long, lat Locations to UTM coordinates and zones
#' Returns a data frame containing the UTM values, the zone and the hemisphere associated with long lat coordinates.
#' @param long vector of X coordinates
#' @param units one of c('m', 'km') returns UTM coordinates in meters or kilometers, respectively.
#' @param lat vector of Y coordinates
#' @export longlat_to_UTM

longlat_to_UTM <- function(long, lat, units = 'm') {

  df <- data.frame(
    id = seq_along(long),
    x = long,
    y = lat
  )
  sp::coordinates(df) <- c("x", "y")

  zone <- find_UTM_zone(long, lat)

  sp::proj4string(df) <- sp::CRS("+init=epsg:4326")

  hemisphere <- ifelse(lat > 0, "north", "south")

  CRSstring <- paste0(
    "+proj=utm +zone=", zone,
    " +ellps=WGS84",
    " +", hemisphere,
    " +units=", units)

  # if (dplyr::n_distinct(CRSstring) > 1L)
  #   stop("multiple zone/hemisphere detected")

  nzones <- unique(zone)
  # sort by zone
  out <- list()
  for(i in seq_along(nzones)){
    z = nzones[i]
    zrows = which(zone==z)
    dat = df[zrows,]
    h = hemisphere[i]


    ids = dat$id
suppressWarnings(  out[[i]] <- sp::spTransform(dat, sp::CRS(CRSstring[1L]))  |>
    tibble::as_tibble() |>
    dplyr::mutate(
      zone = z,
      hemisphere = h,
      id = ids
    )
)

  } # end loop

  # browser()
  coords.out <- dplyr::bind_rows(out) |>
    dplyr::arrange(id) |>
    dplyr::mutate(
      long = long,
      lat  = lat
    ) |>
    dplyr::select(-id)

  return(coords.out)
}

