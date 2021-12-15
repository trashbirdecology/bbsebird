# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- lubridate::hms(ebd_zf$time_observations_started)
  # x <- lubridate::hms(x)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
clean_zf <- function(ebd_zf){
  clean_zf <- ebd_zf %>%
    mutate(
      # convert X to NA
      observation_count = if_else(observation_count == "X",
                                  NA_character_, observation_count),
      observation_count = as.integer(observation_count),
      # effort_distance_km to 0 for non-travelling counts
      effort_distance_km = if_else(protocol_type != "Traveling",
                                   0, effort_distance_km))

  clean_zf %>% mutate(
    # convert time to decimal hours since midnight
    time_observations_started_hsm = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

  return(clean_zf)


}



# CONVERT COLUMNS  --------------------------------------------------------

convert_cols <- function(x){

  ## numeric
  num <- c("duration_minutes",
           "effort_area_ha",
           "effort_distance_km",
           "latitude",
           "longitude")
  ## integer
  ints <- c("all_species_reported",
            "number_observers",
            "observation_count")

  ## date/time
  dates <- c("observation_date",
             "date")
  # times <- c("time_observations_started")
  ## characters
  chrs <- c("country",
            "common_name",
            "country_code",
            "county",
            "county_code",
            "group_identifier",
            "observer_id",
            "protocol_code",
            "protocol_type",
            "sampling_event_identifier",
            "scientific_name",
            "state",
            "state_code",
            "time_observations_started"
  )


  x <- x %>%
    mutate(across(any_of(c(chrs, num, ints)), as.character)) %>%
    mutate(across(any_of(num), as.numeric)) %>%
    mutate(across(any_of(ints), as.integer)) %>%
    mutate(across(any_of(dates), as.Date))

return(x)


}


# split a dataframe or tibble into a list ---------------------------------

split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])

