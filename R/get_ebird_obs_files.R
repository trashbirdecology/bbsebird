#' Identify and Unpack eBird Observations Files
#'
#' @param dir.out first, search for existing .rds files in output directory.
#' @param dir.ebird.in ...
#' @param species ...
#' @param countries ...
#' @param mmyyyy ...
#' @export get_ebird_obs_files
get_ebird_obs_files <- function(dir.ebird.in,
                                mmyyyy="feb-2022",
                                dir.out=NULL,
                                species = NULL,
                                countries=NULL
                                ){
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
      s=which(nchar(species)==nchar(gsub(" ", "", species)))
      if(length(s)>0) species <- species[s]
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
  if(length(fns.obs)==0) stop("no files found. ")
  stopifnot(all(file.exists(fns.obs)))

  return(fns.obs)
}
