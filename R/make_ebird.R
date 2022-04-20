#' Munge eBird for Use Case
#' @param fns.samps Filenames for import sampling events
#' @param fns.obs  Filenames or directory for
#' @param dir.ebird.in Where the original eBird data are stored. A single directory only...
#' @param dir.out  Path to directory for where to save compressed outputs of munged data. If NULL will save to a subdirectory within current working directory.
#' @param countries countries
#' @param overwrite if TRUE will overwrite any existing files.
#' @export make_ebird
make_ebird <-
  function(dir.ebird.in,
           dir.out = NULL,
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
           max.num.observers = 10,
           ncores  = NULL
           ) {
  # ARGS
  if(!grep("-", mmyyyy)==1){stop("argument `mmyyyy` must include hyphen between month and year (i.e. mm-yyyy).")}
  mmyyyy <- tolower(mmyyyy)
  countries <- tolower(countries)
  if(is.null(dir.out)) dir.out <- paste0(getwd(),"/data/ebird/")
  dir.create(dir.out, recursive=TRUE, showWarnings = FALSE)
  if(is.null(ncores)) ncores <- parallel::detectCores()-1

  ## SAMPLING FILES
  fns.samps <-  partition_ebird_events(dir.ebird.in = dir.ebird.in,
                                                      mmyyyy,
                                                      outpath = NULL,
                                                      overwrite = FALSE,
                                                      countries = countries)

  ## OBSERVATIONS FILES
    ## first, check for the combined data
    # pattern=paste0("ebird-obs_")
  fns.obs <- tolower(list.files(path=dir.out, "ebird-obs_", recursive=TRUE, full.names=TRUE, ignore.case = TRUE))
  if(length(fns.obs)>0) fns.obs <- fns[grepl(mmyyyy, fns.obs)]
  if(length(fns.obs)==0){
    fns.obs <- tolower(list.files(path=dir.ebird.in, pattern="ebd_", recursive = TRUE, full.names=TRUE, ignore.case = TRUE))
    fns.obs <- fns.obs[setdiff(1:length(fns.obs), which(grepl(pattern="sampling",    x=fns.obs)))]
    fns.obs <- fns.obs[grepl(mmyyyy, fns.obs)]
    ## If grabbing the entire dataset (not yet tested..)
    grab.full.obs <- ifelse((is.null(species) && is.null(countries)), TRUE, FALSE)
    if(grab.full.obs){
      fns.obs <- fns.obs[grepl(paste0("ebd_rel", mmyyyy), fns.obs)]
    }

    ##Filter fns.obs by species, country
    #### species first...
    if(!is.null(species)){
      species <- tolower(species)
      fns.obs <- fns.obs[grepl(species, fns.obs)]
      if(length(fns.obs)==0)stop("no ebird observations files found for ", species, ": ", species,
                                 ". Please check ", "'species'"," argument.\n")
    }
    stopifnot(length(fns.obs)>0)
    #### species first...
    if(!is.null(countries)){
      countries <- tolower(countries)
      fn.new<-NULL
      for(i in seq_along(countries)) {
        fn.new <- c(fn.new, fns.obs[grepl(paste0("_",countries[i]), fns.obs)])
            if(length(fns.obs)==0)stop("no ebird observations files found for ", "countries " , ": ", countries[i],
                                 ". Please check ", "'countries'"," argument.\n")
      }
      fns.obs <- fn.new
      rm(fn.new)
      }
    fns.obs.zip <- fns.obs[grepl(".zip", fns.obs)]
    fns.obs.txt <- fns.obs[grepl(".txt", fns.obs)]
    fns.obs.to.unzip <- setdiff(gsub(pattern = ".txt", replacement = "", x=fns.obs.txt),
                        gsub(pattern = ".zip", replacement = "", x=fns.obs.zip))
    if(length(fns.obs.to.unzip)>0){
      for(i in seq_along(fns.obs.to.unzip)){
        x=fns.obs.to.unzip[i]
        y   <- unzip(x,overwrite = TRUE, list = TRUE)$Name
        f   <- y[grep(x=y, pattern = "ebd_")]
        suppressWarnings(unzip(zipfile=x, files = y, exdir = dir.ebird.in, overwrite = FALSE))
      }
    fns.obs <- fns.obs.txt
    }else{fns.obs <- fns.obs.txt}
  } # END make fn.obs if its NULL/empty
  stopifnot(all(file.exists(fn.obs)))

## IMPORT THE DATA
  # fn.obs;fns.samp
  # munge_ebird()
} # END FUNCTION

