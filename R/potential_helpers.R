
# FOR MUNGING EBIRD DATA --------------------------------------------------
## convert time observation to HOURS SINCE MIDNIGHT
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}


# Unpack ebird files in a directory ---------------------------------------------
#' @title unpack_ebird
#' @description Unpacks the eBird files (EBD and SamplingEvent data). Saves all unpacked data to a subdirectory (see: parameter dir)
#' @param dir Directory where the packed ebird data files are stored and into which files will unpack.
#' @param overwrite Logical. Default=FALSE will not overwrite existing files in dir.
#' @param spp.ind A species indicator (using eBird terminology), used to identify which .zip files should be unpacked. Defaults to 'doccor' (Double-crested Cormorant).
unpack_ebird <- function(dir="data-raw/ebird-data",
                         overwrite=FALSE,
                         spp.ind="doccor" ## defaults to double-crested cormorant
                         ){
  suppressWarnings(dir.create(dir))
  ebd.files <- grep(spp.ind, list.files(dir, full.names=TRUE), value=TRUE)
  ebd.packed <- grep(".zip", ebd.files, value=TRUE)

  for(i in seq_along(ebd.packed)){
      ebd.unpacked <- stringr::str_replace(ebd.files[i], ".zip", ".txt")
      if(!ebd.unpacked %in% list.files(dir, full.names = TRUE)) ind=TRUE else(ind=FALSE)
      if(ebd.unpacked %in% list.files(dir, full.names = TRUE) & overwrite) ind=TRUE
      if(ind) {message(paste("...unpacking file ", ebd.packed[i], " ", i," of ", length(ebd.packed)))
      unzip(ebd.packed[i], exdir=dir)
  }
  }
}

# END HELPER FUNS -----------------------------------------------------------------


