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
#' @param out.filetype if writing new files, what file extension to search for? (e.g., .csv, .txt, .csv.gz).
#' @param mmyyyy month and year associated with the sampling events file.
#' @param countries list of 2-letter country codes. Only sampling events from these countries will be partitioned into indiviudal sampling event files on local disk. Specify countries=NULL to partition all countries (this will take a while)
#' @export partition_ebird_events
partition_ebird_events <-
  function(dir.ebird.in,
           mmyyyy,
           outpath = NULL,
           overwrite = FALSE,
           out.filetype= ".csv.gz",
           countries = c("US", "CA", "MX"),
           ncores = NULL) {

    if(!"Linux" %in% Sys.info()[1])stopifnot(memory.limit()[1] > 57e3) # i think i acutally needed like 55GB...
    country.ind <- countries <- toupper(countries)
    out.filetype <- tolower(out.filetype)
    stopifnot(out.filetype %in% c(".csv.gz", ".csv", ".txt"))

    mmyyyy <- tolower(mmyyyy)
    if (unlist(gregexpr('-', mmyyyy)[1] == -1))
      stop("please place a hyphen between month and year in arg mmyyyy (e.g., 'dec-2022')")
    if (is.null(ncores))
      ncores <- parallel::detectCores() - 1

    if(is.null(outpath)) outpath <- paste0(dir.ebird.in, "/", "partitioned_",mmyyyy,"/")
    outpath <- gsub(pattern = "//", replacement = "/", x=outpath)
    dir.create(outpath, showWarnings=FALSE)
    ## FIRST CHECK TO SEE IF PARTITIONS ALREADY EXIST WHEN OVERWRITE IS FALSE
    fns.temp <-
          list.files(
            outpath,
            pattern = out.filetype,
            ignore.case = TRUE,
            recursive = TRUE,
            full.names = TRUE
          )

    fns.temp <- gsub(pattern = "//", replacement = "/", x=fns.temp)

    if (!overwrite & length(fns.temp)>0) {
      pattern <-
        tolower(paste0(outpath, "partitioned-sampling-events_",
               tolower(countries),
               "_",
               mmyyyy, out.filetype))

      for (i in seq_along(pattern)) {
        z=any(stringr::str_detect(string = tolower(fns.temp), pattern=pattern[i] ))
        if(i==1) x = z else x=c(x, z);rm(z)
      }

      if (any(x)) {
        message(
          "Overwrite is FALSE and partitioned files exist for specified countries. Not overwriting existing files for countries: \n",
          countries[x], "\n")
        if(all(x)) return(fns.temp)
        else{countries <- countries[!x]}
      }
      rm(pattern, x)
    } # end first test

    fns <-
        list.files(
          dir.ebird.in,
          pattern = "sampling_rel",
          recursive = FALSE,
          full.names = TRUE
        )
    fn.txt <-
      fns[stringr::str_detect(tolower(fns), mmyyyy) &
            stringr::str_detect(tolower(fns), ".txt.gz")]
    cat("Partitioning the sampling events data into country-level files for specified countries. This will take ~15 mins.\n")
    ## if fn.xtxt ==0 then NO need to unpack....


if (length(fn.txt) == 0) {
      fn.tar <-
        fns[stringr::str_detect(tolower(fns), mmyyyy) &
              stringr::str_detect(tolower(fns), ".tar")]
      fn.tar.contents <-
        untar(tarfile = fn.tar, list = TRUE) # this just lists files doesnt unpack them....
      fn.txt <-
        fn.tar.contents[stringr::str_detect(tolower(fn.tar.contents), pattern = ".txt.gz")]
      stopifnot(length(fn.txt) > 0)
      cat("Attempting to unpack tarball contents ", fn.txt, "\n")
      untar(tarfile = fn.tar,
            files = fn.txt,
            exdir = dir.ebird.in)
    } # if the fn.txt.gz doesn't already exist

    ## Resample the directory
    fns    <-
      list.files(
        dir.ebird.in,
        pattern = "sampling",
        recursive = FALSE,
        full.names = TRUE
      )
    fn.txt <-
      fns[stringr::str_detect(tolower(fns), mmyyyy) &
            stringr::str_detect(tolower(fns), ".txt.gz")]
    stopifnot(length(fn.txt) == 1)


## IMPORT THE SAMPLING EVENTS
    cat("Importing the ebird sampling events data. This process takes ~3-4 mins on 15 cores....hang in there buddy...\n")
    samps <-
      data.table::fread(file = fn.txt, nThread = ncores,
                        drop = c("SPECIES COMMENTS","V48", "TRIP COMMENTS", "REASON", "REVIEWED", "HAS MEDIA", "AGE/SEX"))
    message("ignore COLUMN name X not found warnings...\n")
    gc() # ~3GB saved maybe

## FIND ROW NUMBERS ASSOCIATED WITH SAMPS DT for each country of interest
    cat("Removing non-target countries from sampling events data...\n")
    vec <- samps[,'COUNTRY CODE'][,`:=`(rownum=1:nrow(samps))] ## grab ctry col + add rownumber
    temp <- countries[which(!countries %in% unique(vec$`COUNTRY CODE`))]
    if(length(temp)>0)
      warning(
        "the following countries were not found in sampling data. please check specification is correct for countries:\n\t",
          temp
      )
    rowinds <- which(vec$`COUNTRY CODE` %in% countries)
    samps <- samps[rowinds,] ## filter down sampling events
    rm(rowinds, vec, temp)
    gc() # definitely keep this one!
    cat("Splitting data by country")
    samps <- split(samps, by = "COUNTRY CODE")
    cat("...done\n")

    cat("To try to avoid memory overload, I, robot, am saving data in chunks by country. \nThis takes ~10mins for CAN and USA on 15 threads.\n")
    for (i in seq_along(samps)) {
      if(!toupper(names(samps)[i]) %in% countries) next()
      fn <-
        paste0(
          outpath,
          "/partitioned-sampling-events_",
          names(samps)[1],
          "_",
          mmyyyy,
          out.filetype
        )
      if((file.exists(fn) & !overwrite)){
        print(fn, "already exists and overwrite is FALSE. Not overwriting...\n")
        samps[[1]] <- NULL
        next()
      }
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
      rm(fn)
    }#end i loop
fns.out <- tolower(list.files(outpath, full.names = TRUE))
fns.out <- fns.out[grep(pattern = out.filetype, x=fns.out)]
p = unlist(lapply(countries, function(c){
  paste0("(?=.*", c,")(?=.*", eval(mmyyyy),")(?=.*", eval(out.filetype) ,")")

}))
for(i in seq_along(p)){
  if(i==1) f.out <- NULL
  f.out <- c(f.out, fns.out[which(grepl(pattern=paste(p[i]), x=fns.out, perl=TRUE, ignore.case = TRUE))])
}
fns.out <- unique(f.out)

return(fns.out) ### return the filenames for use in eBird import/munging functions

  } # END FUNCTION
