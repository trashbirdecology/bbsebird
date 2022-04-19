#' Partition the eBird Sampling Events Data File by States/Provinces
#'
#' Relies heavily on package 'data.table' to import, filter, and write the sampling events data for each country of interest.
#' @param dir.ebird.in directory path to where ebird sampling events is/are saved
#' @import data.table
#' @importFrom stringr str_replace
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
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

    ## import two columns then grab only event identifiers where country code is in countries
    checklists.keep <- data.table::fread(file=fn.txt)#, select=c("COUNTRY CODE","SAMPLING EVENT IDENTIFIER"))
    checklists.keep <- checklists.keep[which(checklists.keep$'COUNTRY CODE' %in% countries),]
### STOPPE DHERE

    cat("Importing the ebird sampling events data. This process takes ~2-3 mins on 15 cores\n")
    vec <- checklists.keep$`COUNTRY CODE`
    N=length(countries)
    out <- vector("list",N)
    names(out) <- countries

data.table::setkey(checklists.keep, "COUNTRY CODE")
stopifnot(data.table::haskey(checklists.keep))
samps <- split(checklists.keep, by="COUNTRY CODE")

# chunks <- bit::chunk(1:nrow(samps), length=100)
#
#
# comb <- function(listout){
#   n=names(listout)
#   u=unique(n)
#   newlistout <- vector("list", length(u))
#   names(newlistout) <- u
#   for(i in seq_along(u)){
#      grab <-  which(n %in% u[i])
#      newlistout[[u[i]]] <- data.table::rbindlist(listout[grab])
#   }
#   return(listout)
# }
#     cl <- parallel::makeCluster(ncores)
#     doParallel::registerDoParallel(cl)
#     listout <- foreach::foreach(i=seq_along(chunks),
#                                 # .combine=comb,
#                                 .packages = "data.table") %dopar% {
#       start = chunks[[i]][1]
#       end   = chunks[[i]][2]
#       rows  = samps[start:end,]
#       rows  = data.table::setkey(rows, "COUNTRY CODE")
#       listout <- split(rows, by="COUNTRY CODE")
#       names(listout) <- unique(rows$`COUNTRY CODE`) ## this works on data table because setting a key sorts it...
#       return(listout)
#     }
#     parallel::stopCluster(cl)
#
#     # samps <- data.table::fread(file=fn.txt, nThread = ncores)
#
#     # samps <- vroom::vroom(file=fn.txt, num_threads = ncores)#, n_max = 2e5)
#     # samps <- data.table::as.data.table(samps)
#     cat(".....done importing sampling events...\n")
#         ### fyi:: data.table::fread took ~1.5 times longer than vroom....
#   # Make Files for each Country
#   {fn <- NULL
#       header <- samps[1,]; header <- header[-1,]
#       for(i in 1:length(countries)){
#         fn[i] <- paste0(dir.ebird.in,"/partitioned-sampling-events_", countries[i], "_",mmyyyy, ".txt")
#         if(file.exists(fn[i]) & overwrite){
#              file.remove(fn[i])
#           index=TRUE
#         }else{
#           if(file.exists(fn[i]) & !overwrite)
#             cat("file", fn[i],"exists and overwrite=FALSE. Not overwriting.\n")
#           if(!exists("index")) index <- TRUE
#           if(index) data.table::fwrite(x=header ,file=fn[i])
#         }
#     }
#     stopifnot(length(countries)==length(fn))
#     }
#
#     ### filter using data.table and save
#     samps <- data.table::setkey(samps, "COUNTRY CODE")
#     samps <- samps[countries] ## filter by key
#   cat("Partitioning sampling events and writing to file. This will take a while...\n")
#   for(jj in seq_along(countries)){ # there may be multiple countries
#     cat("partitioning out country: ", countries[jj])
#   ### need to go along row-wise in chunks because country-wise eats way too much memory!
#     chunks <- bit::chunk(1:nrow(samps), length=1e4)
#     cl <- parallel::makeCluster(ncores)
#     doParallel::registerDoParallel(cl)
#     rownums <-
#     foreach::foreach(i=seq_along(chunks), .combine="c") %dopar% {
#           start = chunks[[i]][1]
#           end   = chunks[[i]][2]
#           rows  = samps[start:end,]
#           rows  = data.table::setkey(rows, "COUNTRY CODE")
#           rows  = rows[countries[jj], which=TRUE] + start
#     }
#     parallel::stopCluster(cl)
#     rownums <- na.omit(rownums)
#     data.table::fwrite(x = samps[rownums, ], file = fn[jj], append = FALSE)## write to file
#     ## remove those rows since we no longer need them...
#     rows.keep <- setdiff(1:nrow(samps), rownums)
#     samps <- samps[rows.keep,]
#     cat("\n\t....done")
#   }
  gc()
  return(fn) ### return the filenames for use in eBird import/munging functions

} # END FUNCTION
