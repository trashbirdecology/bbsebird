#' Partition the eBird Sampling Events Data File by States/Provinces
#'
#' Relies heavily on package 'data.table' to import, filter, and write the sampling events data for each country of interest.
#' @param dir.ebird.in directory path to where ebird sampling events is/are saved
#' @import data.table
#' @importFrom stringr str_detect str_replace
#' @importFrom bit chunk
#' @param cols.remove which columns will be removed upon import. For internal use primarily. Changing my disrupt the workflow as this feature has not been tested downstream.
#' @param mmyyyy month and year associated with the sampling events file.
#' @param countries list of 2-letter country codes. Only sampling events from these countries will be partitioned into indiviudal sampling event files on local disk. Specify countries=NULL to partition all countries (this will take a while)
partition_ebird_events <-
  function(dir.ebird.in,
           mmyyyy,
           overwrite=FALSE,
           countries = c("US", "CA", "MX"),
           ncores = NULL
  ){

    stopifnot(memory.limit()[1] > 57e3) # i think i acutally needed like 55GB...

    mmyyyy <- tolower(mmyyyy)
    if(unlist(gregexpr('-', mmyyyy)[1] == -1)) stop("please place a hyphen between month and year in arg mmyyyy (e.g., 'dec-2022')")
    if(is.null(ncores)) ncores <- parallel::detectCores()-2

    fns <- tolower(list.files(dir.ebird.in, pattern="sampling_rel", recursive=TRUE, full.names=TRUE))
    fn.txt <- fns[stringr::str_detect(fns, mmyyyy) & stringr::str_detect(fns, ".txt.gz")]


    ## if fn.xtxt ==0 then NO need to unpack....
    if(length(fn.txt) == 0){
      fn.tar <- fns[stringr::str_detect(fns, mmyyyy) & stringr::str_detect(fns, ".tar")]
      fn.tar.contents <- untar(tarfile=fn.tar, list=TRUE) # this just lists files doesnt unpack them....
      fn.txt <- fn.tar.contents[stringr::str_detect(fn.tar.contents, pattern=".txt.gz")]
      stopifnot(length(fn.txt) > 0)
      cat("attempting to unpack tarball contents ", fn.txt, "\n")
      untar(tarfile=fn.tar, files=fn.txt, exdir = dir.ebird.in)
    } # if the fn.txt.gz doesn't already exist

    ## Resample the directory
    fns    <- tolower(list.files(dir.ebird.in, pattern="sampling", recursive=TRUE, full.names=TRUE))
    fn.txt <- fns[stringr::str_detect(fns, mmyyyy) & stringr::str_detect(fns, ".txt.gz")]
    stopifnot(length(fn.txt)==1)

    cat("Importing the ebird sampling events data. This process takes ~2-3 mins on 15 cores\n")
    ## import two columns then grab only event identifiers where country code is in countries
    samps <- data.table::fread(file=fn.txt)#, select=c("COUNTRY CODE","SAMPLING EVENT IDENTIFIER"))
    samps <- samps[which(samps$'COUNTRY CODE' %in% countries),]
### STOPPE DHERE



data.table::setkey(samps, "COUNTRY CODE")
stopifnot(data.table::haskey(samps))
cat("splitting data by country\n")
samps <- split(samps, by="COUNTRY CODE")
fn <- NULL
cat("saving data in chunks by country\n")
for (i in seq_along(samps)) {
  fn[i] <-
    paste0(dir.ebird.in,
           "/partitioned-sampling-events_",
           names(samps)[i],
           "_",
           mmyyyy,
           ".txt")
  if((file.exists(fn[i]) & overwrite)||!file.exists(fn[i])){
  chunks <- bit::chunk(1:nrow(samps[[i]]), length = 5000)
  for (j in seq_along(chunks)) {
    headerindex <- ifelse(j == 1, TRUE, FALSE)
    data.table::fwrite(
      samps[[i]][chunks[[j]][1]:chunks[[j]][2], ],
      file = fn[i],
      append = !headerindex,
      row.names = FALSE,
      col.names = headerindex,
      nThread = ncores
    )
    if (j %% 500 == 0)
      cat("\t",names(samps)[i], "file iter:", j, "of", length(chunks), "\n")
  }}else{message(fn[i], "exists and overwrite=FALSE. Not overwriting existing files.\n")}
  samps[[i]] <- NULL #attempt remove data from memory after saving...
  rm(chunks)
}

return(fn) ### return the filenames for use in eBird import/munging functions

} # END FUNCTION
