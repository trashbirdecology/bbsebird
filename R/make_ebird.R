#' Munge eBird for Use Case
#'
#' @param fns.samps Filenames for import sampling events
#' @param fns.obs  Filenames or directory for
#' @param dir.ebird.in Where the original eBird data are stored. A single directory only...
#' @param dir.out  Path to directory for where to save compressed outputs of munged data. If NULL will save to a subdirectory within current working directory.
#' @param countries countries
#' @param mmyyyy month-year used for searching ebird files.
#' @param overwrite if TRUE will overwrite any existing files.
#' @param ...
#'
#' @export make_ebird
#'
make_ebird <-
  function(dir.ebird.in,
           dir.out = NULL,
           fns.samps = NULL,
           fns.obs = NULL,
           mmyyyy  = "fEb-2022",
           overwrite = FALSE,
           countries = c("US", "CA"),
           states    = NULL,
           species = NULL,
           complete.only = TRUE,
           protocol = c("Traveling", "Stationary"),
           remove.bbs.obs = TRUE,
           years = NULL,
           max.effort.km = NULL,
           max.effort.mins = NULL,
           max.birds.checklist = 55,
           max.num.observers = 10,
           ncores  = NULL,
           out.filetype = ".csv.gz",
           ydays = NULL
           ) {
# ## FOR DEV PURPOSES
# fns.samps = NULL
# fns.obs = NULL
# complete.only = TRUE;
# protocol = c("Traveling", "Stationary");
# remove.bbs.obs = TRUE;
# # years = NULL;
# max.effort.km = NULL;
# max.effort.mins = NULL;
# max.num.observers = 10;
# zerofill = TRUE
# ncores  = NULL
# out.filetype = ".csv.gz"
# overwrite = FALSE
# dir.ebird.in = dirs$dir.ebird.in
# dir.out = dirs$project
# ARGS
  if(!grep("-", mmyyyy)==1){stop("argument `mmyyyy` must include hyphen between month and year (i.e. mm-yyyy).")}
  mmyyyy <- tolower(mmyyyy)
  countries <- tolower(countries)
  if(is.null(dir.out)) dir.out <- paste0(getwd(),"/data/ebird/")
  dir.create(dir.out, recursive=TRUE, showWarnings = FALSE)
  if(is.null(ncores)) ncores <- parallel::detectCores()-1

## SAMPLING FILENAMES
  ### this identifies, unpacks (if necessary), and paritions the sampling events data into
  ### country-level files. the sampling events df can be one of the limiting  factor
  ### w.r.t. memory capacity.....
  ### this function also grabs filenames for previously-partitioned data if
  ### overwrite==FALSE && data exists for mmyyyy && countries...
  # browser()

if(is.null(fns.samps)){ fns.samps <-  partition_ebird_events(dir.ebird.in = dir.ebird.in,
                                                      mmyyyy = mmyyyy,
                                                      outpath = NULL,
                                                      overwrite = overwrite,
                                                      out.filetype = ".csv.gz",
                                                      countries = countries)
}
#### I need to run twice on the first go around -- this is a really lazy and gross fix.....
#### I should go into partition_ebird_events() and re-eval the directory for the fns.samps
if(is.null(fns.samps)) {fns.samps <-  partition_ebird_events(dir.ebird.in = dir.ebird.in,
                                     mmyyyy = mmyyyy,
                                     outpath = NULL,
                                     overwrite = FALSE,
                                     out.filetype = ".csv.gz",
                                     countries = countries)
}
## OBSERVATIONS FILENAMES
# browser()
if (is.null(fns.obs)) fns.obs   <-  get_ebird_obs_files(
      dir.ebird.in = dir.ebird.in,
      mmyyyy = mmyyyy,
      dir.out = dir.out,
      species = species,
      countries = countries
    )
stopifnot(length(fns.obs) > 0 )
## IMPORT AND MUNGE THE DATA
## this function will import the munged .csv.gz if it exists and !overwrite and gets this far down...
munged <- munge_ebird(
    fns.obs = fns.obs,
    fns.samps = fns.samps,
    dir.out = dir.out,
    complete.only = complete.only,
    years = years,
    countries = countries,
    states = states,
    overwrite = overwrite,
    species = species,
    protocol = protocol,
    remove.bbs.obs = remove.bbs.obs,
    max.effort.km = max.effort.km,
    max.effort.mins = max.effort.mins,
    max.num.observers = max.num.observers,
    ydays = ydays,
    max.birds.checklist = max.birds.checklist
  )

## RETURN OBJEcT
cat(nrow(munged), " rows remain after munging ebird sampling events and observations\n")

return(munged)
} # END FUNCTION

