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
#' @keywords internal
#' @export
junk_it <- function(args.save, new.args.save=NULL){
  args.save <- c(args.save, new.args.save, "args.save") %>% unique()
  rm(list=setdiff(ls(envir = .GlobalEnv), args.save), envir = .GlobalEnv)
  return(args.save)
}

# mode --------------------------------------------------------------------
#' @keywords internal
#' @noRd
.get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# function to convert time observation to hours since midnight ------------------------------------------------------------------
#' @keywords internal
#' @noRd
.time_to_decimal <- function(x) {
  x <- hms(x)
  hour(x) + minute(x) / 60 + second(x) / 3600
}


# a song to tell me something has stopped ------------------------------------------------------------------
#' @keywords internal
#' @noRd
.song <- function() {
  for(i in 1:2) {
    for(i in 1:4) {
      for(i in 1:4) {
        beep(7)
        Sys.sleep(0.25)
        beep()
        Sys.sleep(0.22)
      }
      beep(2)
    }
    beep(11)
  }
  beep(4)
}



# windows alert -----------------------------------------------------------
#' @keywords internal
#' @noRd
.windows_alert <- function(message=rep("The really long process you started has completed. Hopefully you got it right the first time and don't have to redo it....that took a while.....BEEEEEEEEEEEEEEEP", 3)){
  if(.Platform$OS.type == "windows"){return()} # only run for windows OS
  system2(command = "PowerShell",
          args = c("-Command",
                   "\"Add-Type -AssemblyName System.Speech;",
                   "$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;",
                   paste0("$speak.Speak('", message, "');\"")
          ))
}



# evaluate parameter lists ------------------------------------------------
#' Spruce up R Markdown YAML parameter values that are lists into clean vectors
#' @param x The parameter object from YAML `params`
#' @keywords internal
#' @noRd
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



# dir specification -------------------------------------------------------
#' Specify directories for outputs
#' @noRd
#' @keywords  internal

dir_spec <- function(dir.orig.data){
if(!endsWith(dir.orig.data,"/")) dir.orig.data <- paste0(dir.orig.data,"/")
## Where is your original eBird data stored?
dir.ebird.in <- paste0(dir.orig.data, "ebird")
## Where are the BBS route shapefiles stored?
cws.routes.dir <- paste0(dir.orig.data, "/bbs/route_shapefiles/cws")
usgs.routes.dir <- paste0(dir.orig.data, "/bbs/route_shapefiles/usgs")

if(!any(length(list.files(cws.routes.dir))>0)) stop("No files exist `cws.routes.dir` or `usgs.routes.dir`. Please check directory specification.\n")

if(!length(list.files(dir.ebird.in)>0)) stop("No files exist in `dir.ebird.in`. Please check directory specification.\n")

## automatically creates a new directory for storing munged data, results, figures, etc. based on project shorthand name.
if(!"proj.shorthand" %in% names(params)) proj.shorthand <- getwd()
dir.proj.out <- paste0(proj.shorthand,"-example-", round(grid.size*111.111), "km/")
# dir.proj.out <- paste0(getwd(),"/")
## where to store the JAGS objects
dir.jags <- paste0(dir.proj.out, "jags/")
dir.models <- paste0(dir.jags, "models/")  # save model files
dir.bbs.out <- paste0(dir.proj.out,"bbs/")
dir.ebird.out <- paste0(dir.proj.out,"ebird/")
dir.spatial.out <- paste0(dir.proj.out,"spatial/")
dir.plots <- paste0(dir.proj.out, "plots/")
sapply(c(dir.proj.out, dir.bbs.out, dir.ebird.out,
         dir.spatial.out, dir.jags, dir.models, dir.plots), FUN=function(x) dir.create(x, showWarnings = FALSE))

cat("Project directory output files will go to ", dir.proj.out)

names <- c(paste0("dir.",
                c("jags",
                  "plots",
                  "models",
                  "bbs.out",
                  "ebird.out",
                  "spatial.out",
                  "proj.out")),
                  "cws.routes.dir",
                  "usgs.routes.dir"
                )
dirs <- make_list(names)

return(dirs)

}



# trim --------------------------------------------------------------------
#' Remove column if exists as object's rownames
#' @param x a data.frame, matrix, or array of two dimensions
#' @noRd
#' @keywords internal
.trim <- function(x){
  if(is.null(rownames(x))){return(x)} # do nothing if inappropriate for data object
  .rowtrim <- which(rownames(x)=="NA")
  if(length(.rowtrim)==0) return(x)
  x <- x [-.rowtrim,]
  return(x)
}
