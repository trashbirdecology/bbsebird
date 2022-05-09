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
  if(is.null(dir.out)) dir.out <- ""
  fns.obs <- tolower(list.files(path=dir.out, "ebird-obs_", recursive=TRUE, full.names=TRUE, ignore.case = TRUE))
  if(length(fns.obs)>0) fns.obs <- fns[grepl(mmyyyy, fns.obs, ignore.case = TRUE)]
  if(length(fns.obs)==0){
    fns.obs <- (list.files(path=dir.ebird.in, pattern="ebd_", recursive = TRUE, full.names=TRUE, ignore.case = TRUE))
    fns.obs <- fns.obs[setdiff(1:length(fns.obs), which(grepl(pattern="sampling",    x=tolower(fns.obs))))]

  ## Filter by mmyyyy
  fns.obs <- fns.obs[grepl(mmyyyy, tolower(fns.obs))]
  ## Import all data if desired
    grab.full.obs <- ifelse((is.null(species) && is.null(countries)), TRUE, FALSE)
    if(grab.full.obs){
      fns.obs.all <- fns.obs <- fns.obs[grepl(paste0("ebd_rel", mmyyyy), tolower(fns.obs))]
    }else{fns.obs.all <- NULL}
  ## Filter fns.obs by species
    if(!is.null(species)){
      species <- tolower(species)
      ss       <- species[which(nchar(species)==nchar(gsub(" ", "", species)))]
      s       <- unlist(lapply(ss, function(x) {
        paste0("ebd_(?=.*",
               x,
               ")(?=.*rel)")

      }))
      for(i in seq_along(s)){
        if(i==1) f.out <- NULL
        f.out <- c(f.out, fns.obs[which(grepl(pattern=paste(s[i]), x=fns.obs, perl=TRUE, ignore.case = TRUE))])
      }
      fns.out <- unique(f.out)
    }else{fns.out <- NULL} # end species IF

  ## Filter by country  or grab all data
      if ((!is.null(species) && length(fns.out) ==0)||is.null(species)) {

      p <- unlist(lapply(countries, function(x) {paste0("ebd_", x, "_rel", mmyyyy)}))

      for(i in seq_along(p)){
        if(i==1) f.out <- NULL
        f.out <-
          c(f.out, fns.obs[which(grepl(pattern=paste(p[i]), x=tolower(fns.obs), perl=TRUE, ignore.case = TRUE))])
      }
      fns.obs <- unique(f.out)
      }else{fns.obs <- fns.out}

  }
####
if(length(fns.obs)<length(countries)){
  message(
    "Country-level data was not found for all countries. If the entire eBird observations dataset for ",
    mmyyyy,
    " exists it will be added to the output of filepaths.\n",
    "Please check the availability of data in dir.ebird.in directory for selections listed in arguments: countries and/or species and mmyyyy."
  )
}


# Unzip or grab decompressed filepaths ------------------------------------
fns.obs.zip      <- fns.obs[grepl(".zip", tolower(fns.obs))] ## zip are STATE-LEVEL OBS
fns.obs.tar      <- fns.obs[grepl(".tar", tolower(fns.obs))] ## TARS are ALL OTHER OBS + sampling events...
fns.obs.txt      <- fns.obs[grepl(".txt", tolower(fns.obs))] ## .txt.gz must be extracted from the observations .tars, currently no way to reach inside a .tar and import a single file...
fns.obs.txt.gz   <- fns.obs[grepl(".txt.gz", tolower(fns.obs))] ## .txt.gz must be extracted from the observations .tars, currently no way to reach inside a .tar and import a single file...
fns.obs.to.unzip <- setdiff(
  gsub(pattern = ".zip", replacement = "", x=fns.obs.zip, ignore.case = TRUE),
  gsub(pattern = ".txt", replacement = "", x=fns.obs.txt, ignore.case = TRUE))
fns.obs.tar.to.unpack <- setdiff(
                                gsub(pattern = ".tar", replacement = "", x=fns.obs.tar, ignore.case = TRUE),
                                gsub(pattern = ".txt.gz", replacement = "", x=fns.obs.txt.gz, ignore.case = TRUE))
fns.obs.txt      <- setdiff(fns.obs.txt, fns.obs.txt.gz)
##UNPACK TARBALLS ------------------------------------------------------------------
if(length(fns.obs.tar.to.unpack)>0){
  cat("attempting to unpack", length(fns.obs.tar.to.unpack), "tarballs\n")
  lapply(fns.obs.tar.to.unpack, function(x){
    fns <- untar(paste0(x, ".tar"), list=TRUE)
    fn <-  fns[grepl(pattern="_rel", tolower(fns))]
    untar(tarfile = paste0(x, ".tar"), exdir = dir.ebird.in, files = fn)
})
  fns.obs.txt.gz <- c(fns.obs.txt.gz, paste0(fns.obs.tar.to.unpack, ".txt.gz"))

} # end TAR unpacking for OBS

# GRAB FILENAMES FROM WITHIN THE .ZIPs ------------------------------------
if(length(fns.obs.to.unzip)>0){
  fns.obs.to.unzip <- paste0(fns.obs.to.unzip, ".zip")
  fns.out <- unlist(lapply(fns.obs.to.unzip, function(x){
    fns <- unzip(x, list = TRUE)[,1]
    fn  <- fns[grepl(pattern="ebd_", x = fns, ignore.case=TRUE)]
    cat("unziping file...")
    unzip(x, files=fn, exdir=dir.ebird.in, overwrite=FALSE)
    cat("...done\n")
    return(list.files(dir.ebird.in, fn, full.names=TRUE))
  }))
fns.obs.txt <- c(fns.out, fns.obs.txt)
}


# LIST FILES --------------------------------------------------------------
stopifnot(all(file.exists(fns.obs.txt.gz)))
stopifnot(all(file.exists(fns.obs.txt)))
#### SO FAR I HAVE TESTED THIS ON TEH FULL COUNTRY-LEVEL OBSERVATIONS DAATA (US and CA feb 2022)
#### now i need to test on state-level data....


# END FUNCTION ------------------------------------------------------------
return(c(fns.obs.txt.gz, fns.obs.txt))
}
