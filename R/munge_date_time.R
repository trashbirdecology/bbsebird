#' Munge Dates and Times of the eBird and BBS Datasets
#' @param dat data set with at least the column `date` and of class "date"
#' @param min.yday minimum day of the year to include in resulting dataset
#' @param max.yday maximum day of the year to include in resulting dataset
#' @param sunlight logical If TRUE will calculate all sunlight, moonlight, rise and set times. This is computationally demanding for the eBird data, so do not set to TRUE unless needed.
#' @param base.date character or date string (YYYY-MM-DD) to use as the origin date for calculating Julian date.
#' @importFrom lubridate as_date yday
#' @importFrom hms as_hms
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom parallel splitIndices
#' @importFrom suncalc getSunlightTimes
#' @export munge_date_time

munge_date_time <- function(dat, base.date, min.yday=0, max.yday=365, sunlight=FALSE){

  if(!class(base.date) == "Date") stop("`base.date` is not of class `date`. please check specification of base.date. \nConsider using `lubridate::ymd()` or `lubridate::as_date()` to change base.date to class date. ")

  names=names(dat)

  if("date" %in% names){
    dat$date   <- lubridate::as_date(dat$date)
    dat$yday   <- lubridate::yday(dat$date)
    dat$julian <- julian(dat$date, origin = lubridate::as_date(base.date))
    ## filter on the ydays (if provided)
    dat <- dat %>% dplyr::filter((yday >= min.yday & yday <= max.yday)|is.na(yday)) ## keep the NA values.
    } # end dates munging

  # bbs data has starttime and endtime
  if("starttime" %in% names){
    dat$starttime=hms::as_hms(as.POSIXct(dat$starttime, format="%H%M"))
    }
  if("endtime" %in% names){
    dat$endtime=hms::as_hms(as.POSIXct(dat$endtime, format="%H%M"))
  }
  # ebird data has time_obs_started
  if("time_observations_started" %in% names){
    dat$time_observations_started <- hms::as_hms(as.POSIXct(dat$time_observations_started, format="%H:%M:%S"))
  }

#### SUNLIGHT/MOONLIGHT/RISE/SET
if(sunlight){

sunlight.keep <- c("dawn", "solarNoon", "sunrise","sunriseEnd") # change these later if you need other information.

cat("Calculating astronomical statistics for each observation. This may take a few minutes.\n")

  ## ebird is so large that I need to split up b/c calculating astronomical info takes forever.
  ## i'd like to use kit::funique, but cannot figure out how to do that with >1 columns.
  ### so, am resorting to this method.
  x <- dat %>% dplyr::select(date, lat, lon) %>%
    filter(!is.na(date))
  chunks <- parallel::splitIndices(nrow(x), 100)

  for(i in seq_along(chunks)){
    if(i==1) dat.sun <- NULL
    rows = as.data.frame(chunks[i])
    if(nrow(rows)==0) next()
    chunk.start = min(rows[1])
    chunk.end   = max(rows[1])
    y = x[chunk.start:chunk.end, ]
    y = suncalc::getSunlightTimes(data=y,
                                    keep = sunlight.keep)

    dat.sun <- dplyr::bind_rows(dat.sun, y)
    rm(chunk.end, chunk.start, rows, y)
  }#end chunk for loop

  ### turn vars in sunlight.keep into time only (otherwise they are in YYYY-MM-DD HH-MM-SS; we need only HH-MM)
  dat.sun <- dat.sun %>%
    dplyr::mutate(across(sunlight.keep, hms::as_hms))

  ### add sunlight information to spatial data sets
  dat <- left_join(dat, dat.sun)

} # END SUNLIGHT



return(dat)

}

