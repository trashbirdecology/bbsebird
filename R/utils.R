# time_to_decimal ---------------------------------------------------------
#' Convert the ebird zero-filled date to decimal times

#' @param x the data to to convert to H:M:S
#' @keywords internal
#' @noRd
time_to_decimal <- function(x) {
  x <- lubridate::hms(ebd_zf$time_observations_started)
  # x <- lubridate::hms(x)
  # hour(x) + minute(x) / 60 + second(x) / 3600
}


# clean_zf ----------------------------------------------------------------
#' Clean up variables for the zer-filled ebird data
#'
#' @param ebd_zf The zero-filled ebird data object (flat)
#' @keywords internal
#' @noRd
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



# convert_cols  --------------------------------------------------------
#' Convert columns
#'
#' Converts columns classes based on names for the bbs and ebird data
#' @param x The data frame with one or more columns.
#' @keywords internal
#' @export
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


# split_table ---------------------------------
#' Split a dataframe or tibble into a list
#'
#' This is essentially a splice but list elements remain tibble or data.frame, instead of matrices or arrays.
#' @param tibble the flat data object
#' @param col the name(s) of the column(s) used to splice the table into a list
#' @noRd
split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])


# SE ----------------------------------------------------------------------
##' Standard Error
#'
#' Computes standard error of a vector
#' @param x Numeric vector
#' @examples
#' x <- rnorm(100)
#' se(x)
#' @keywords internal
#' @noRd
se <- function(x){
  sd(x)/sqrt(length(x))
}



# .make.integer -----------------------------------------------------------
#' @keywords internal
#' @noRd
make.integer <- function(x, var=c("AOU", "aou")){
  for(i in seq_along(var)){
    ind=var[i]
    if(ind %in% names(x)){
      x=as.data.frame(x)
      x[,paste(ind)] = as.integer(dplyr::pull(x, ind))
    }else{return(x)}
  }
}



# garbage collection and clear junk -----------------------------------------------------------
#' Keep Only Wanted Objects in Memory
#'
#' Removes all items except those specified. Returns a new list of the arguments you want to always retain in memory while removing duplicates.
#' @param args.save A list of new or previously specified arguments.
#' @param new.args.save Optional. One or more new object names to save (in addition to args.save)
#' @export junk_it
junk_it <- function(args.save, new.args.save=NULL){
  args.save <- c(args.save, new.args.save, "args.save") %>% unique()
  rm(list=setdiff(ls(envir = .GlobalEnv), args.save), envir = .GlobalEnv)
  return(args.save)
}

# mode --------------------------------------------------------------------
#' @keywords internal
#' @noRd
#' @export
.get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# function to convert time observation to hours since midnight ------------------------------------------------------------------
#' @keywords internal
#' @noRd
#' @export
.time_to_decimal <- function(x) {
  x <- hms(x)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

#'
#' # a song to tell me something has stopped ------------------------------------------------------------------
#' #' @keywords internal
#' #' @noRd
#' .song <- function() {
#'   for(i in 1:2) {
#'     for(i in 1:4) {
#'       for(i in 1:4) {
#'         beep(7)
#'         Sys.sleep(0.25)
#'         beep()
#'         Sys.sleep(0.22)
#'       }
#'       beep(2)
#'     }
#'     beep(11)
#'   }
#'   beep(4)
#' }
#'
#'
#'
#' # windows alert -----------------------------------------------------------
#' #' @keywords internal
#' #' @noRd
#' .windows_alert <- function(message=rep("The really long process you started has completed. Hopefully you got it right the first time and don't have to redo it....that took a while.....BEEEEEEEEEEEEEEEP", 3)){
#'   if(.Platform$OS.type == "windows"){return()} # only run for windows OS
#'   system2(command = "PowerShell",
#'           args = c("-Command",
#'                    "\"Add-Type -AssemblyName System.Speech;",
#'                    "$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;",
#'                    paste0("$speak.Speak('", message, "');\"")
#'           ))
#' }
#'


# evaluate parameter lists ------------------------------------------------
#' Spruce up R Markdown YAML parameter values that are lists into clean vectors. Called for convenience inside .RMD files.
#' @param x The parameter object from YAML `params`
#' @keywords internal
#' @export eval_params
eval_params <- function(x=params){
  y <- list() #make empty list to store new objects

for(i in 1:length(x)){
  obj  <- x[[i]]
  name <- names(x)[i]

  if(is.logical(obj) | is.numeric(obj)){skip=TRUE}else{skip=FALSE} # skip the logicals and numerics

  if(name %in% c("year.range") & !skip){
    yrs <- as.integer(unlist(strsplit(obj, split = ":")))
    obj <- yrs[1]:yrs[2]
  }

  if(length(obj) == 1){
    if(stringr::str_detect(obj, ",")){
      obj <- trimws(unlist(strsplit(obj, split = ",")))
      }}

  # if(obj == x[[i]])stop(print(i))
  y[[i]] <- obj
  names(y)[i] <- name
  }#end for loop
# browser()
  return(y)
}


# trim --------------------------------------------------------------------
#' Remove column if exists as object's rownames
#' @param x a data.frame, matrix, or array of two dimensions
#' @noRd
#' @export .trim
.trim <- function(x){
  if(is.null(rownames(x))){return(x)} # do nothing if inappropriate for data object
  .rowtrim <- which(rownames(x)=="NA")
  if(length(.rowtrim)==0) return(x)
  x <- x [-.rowtrim,]
  return(x)
}


# proj-shorthand ----------------------------------------------------------
#' Auto generate a subdirectory name based on project parameters
#' @param species list of species names. will use the longest value as the name
#' @param states list of state/province names. will use the shortest values in the name
#' @param year.range Vector of years. will take the min and max value
#' @export
proj.shorthand <- function(species, states, grid.size, year.range){

  x <- paste0(
  species[nchar(species)==(max(nchar(species)))][1],#take min or max to assign species to dir name
  "_",
  paste0(states[nchar(states)==2], collapse = "-"), # regions
  "_",
  grid.size*111,"km", # size of grid cells
  "_",
  min(year.range), "-", max(year.range) # time period
)

return(x)
}
