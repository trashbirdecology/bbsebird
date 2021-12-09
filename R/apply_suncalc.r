apply_suncalc <- function(x) {

  # munge bbs to apply getSunLightTimes
  if ("rteno" %in% names(x)) {
    x.new <- x %>% distinct(date, latitude, longitude, rteno, year) %>%
      rename(lon = longitude, lat = latitude)
  }else{}


  # get times
  y = suncalc::getSunlightTimes(data = x.new)



}
