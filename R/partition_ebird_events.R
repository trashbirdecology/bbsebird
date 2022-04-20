#' Partition the eBird Sampling Events Data File by States/Provinces
#'
#' Relies heavily on package 'data.table' to import, filter, and write the sampling events data for each country of interest.
#' @param dir.ebird.in directory path to where ebird sampling events is/are saved
#' @import data.table
#' @importFrom stringr str_detect str_replace
#' @importFrom data.table setkey haskey
#' @importFrom bit chunk
#' @param outpath path location to where output files will be saved. If NULL will save to a directory within dir.ebird.in for each mmyyyy
#' @param overwrite if TRUE, will overwrite existing partitioned file for country and mmyyyy combination
#' @param ncores number of processors to engage during data import and export (using data.table)
#' @param cols.remove which columns will be removed upon import. For internal use primarily. Changing my disrupt the workflow as this feature has not been tested downstream.
#' @param mmyyyy month and year associated with the sampling events file.
#' @param countries list of 2-letter country codes. Only sampling events from these countries will be partitioned into indiviudal sampling event files on local disk. Specify countries=NULL to partition all countries (this will take a while)
#' @export partition_ebird_events
partition_ebird_events <-
  function(dir.ebird.in,
           mmyyyy,
           outpath = NULL,
           overwrite = FALSE,
           countries = c("US", "CA", "MX"),
           ncores = NULL) {
    stopifnot(memory.limit()[1] > 57e3) # i think i acutally needed like 55GB...

    mmyyyy <- tolower(mmyyyy)
    if (unlist(gregexpr('-', mmyyyy)[1] == -1))
      stop("please place a hyphen between month and year in arg mmyyyy (e.g., 'dec-2022')")
    if (is.null(ncores))
      ncores <- parallel::detectCores() - 2

    if(is.null(outpath)) outpath <- paste0(dir.ebird.in, "/", "partitioned_",mmyyyy,"/")
    dir.create(outpath, showWarnings=FALSE)
    ## FIRST CHECK TO SEE IF PARTITIONS ALREADY EXIST WHEN OVERWRITE IS FALSE
    fns.temp <-
        tolower(
          list.files(
            outpath,
            pattern = ".csv.gz",
            ignore.case = TRUE,
            recursive = FALSE,
            full.names = TRUE
          )
        )
    if (!overwrite & length(fns.temp)>0) {
      pattern <-
        paste0("partitioned-sampling-events_",
               tolower(countries),
               "_",
               mmyyyy, ".csv.gz")
      x = NULL
      for (i in seq_along(pattern)) {
        x = c(x, any(stringr::str_detect(fns.temp[i], pattern)))
      }
      if (all(x)) {
        message(
          "Overwrite is FALSE and partitioned files exist for specified countries. Not overwriting existing files and returning list of partitioned filenames.\n"
        )
        return(fns.temp)
      } else{
        rm(fns, pattern, x)
      }
    } # end first test


    fns <-
      tolower(
        list.files(
          dir.ebird.in,
          pattern = "sampling_rel",
          recursive = FALSE,
          full.names = TRUE
        )
      )
    fn.txt <-
      fns[stringr::str_detect(fns, mmyyyy) &
            stringr::str_detect(fns, ".txt.gz")]

    ## if fn.xtxt ==0 then NO need to unpack....
    if (length(fn.txt) == 0) {
      fn.tar <-
        fns[stringr::str_detect(fns, mmyyyy) &
              stringr::str_detect(fns, ".tar")]
      fn.tar.contents <-
        untar(tarfile = fn.tar, list = TRUE) # this just lists files doesnt unpack them....
      fn.txt <-
        fn.tar.contents[stringr::str_detect(fn.tar.contents, pattern = ".txt.gz")]
      stopifnot(length(fn.txt) > 0)
      cat("attempting to unpack tarball contents ", fn.txt, "\n")
      untar(tarfile = fn.tar,
            files = fn.txt,
            exdir = dir.ebird.in)
    } # if the fn.txt.gz doesn't already exist

    ## Resample the directory
    fns    <-
      tolower(list.files(
        dir.ebird.in,
        pattern = "sampling",
        recursive = FALSE,
        full.names = TRUE
      ))
    fn.txt <-
      fns[stringr::str_detect(fns, mmyyyy) &
            stringr::str_detect(fns, ".txt.gz")]
    stopifnot(length(fn.txt) == 1)

    cat("Importing the ebird sampling events data. This process takes ~3-4 mins on 15 cores....hang in there buddy...\n")
    data.table::setDTthreads(ncores)
## IMPORT THE SAMPLING EVENTS
    samps <-
      data.table::fread(file = fn.txt, nThread = ncores)#, select=c("COUNTRY CODE","SAMPLING EVENT IDENTIFIER"))
    gc() # ~3GB saved maybe

## FIND ROW NUMBERS ASSOCIATED WITH SAMPS DT for each country of interest
    cat("Removing unwanted data using countries arg\n")
    vec <- samps[,'COUNTRY CODE'][,`:=`(rownum=1:nrow(samps))] ## grab ctry col + add rownumber
    data.table::setkey(vec, "COUNTRY CODE"); stopifnot(haskey(vec))
    stopifnot(all(countries %in% unique(vec$`COUNTRY CODE`)))

    rowinds <- which(vec$`COUNTRY CODE` %in% countries)
    samps <- samps[rowinds,]
    rm(rowinds, vec)
    gc() # definitely keep this one!
    cat("Splitting data by country\n")
    samps <- split(samps, by = "COUNTRY CODE")
    # gc()
    fn <- NULL
    cat("saving data in chunks by country. This takes ~10mins for CAN and USA on 15 threads.\n")
    for (i in seq_along(samps)) {
      fn <-
        paste0(
          dir.ebird.in,
          "/partitioned-sampling-events_",
          names(samps)[1],
          "_",
          mmyyyy,
          ".csv"
        )
      if ((file.exists(fn) & overwrite) || !file.exists(fn)) {
        chunks <- bit::chunk(1:nrow(samps[[1]]), length = 5000)
        for (j in seq_along(chunks)) {
          headerindex <- ifelse(j == 1, TRUE, FALSE)
          data.table::fwrite(
            samps[[1]][chunks[[j]][1]:chunks[[j]][2],],
            file = fn,
            append = !headerindex,
            row.names = FALSE,
            col.names = headerindex,
            nThread = ncores
          )
          if (j %% 500 == 0)
            cat("\t",
                names(samps)[1],
                "file iter:",
                j,
                "of",
                length(chunks),
                "\n")
        }
      rm(chunks)
      } else{
        message(fn,
                " exists and overwrite=FALSE. Not overwriting existing files.\n")
      }
      samps[[1]] <- NULL #attempt remove data from memory after saving...
      gc()
    }

    return(fn) ### return the filenames for use in eBird import/munging functions

  } # END FUNCTION
