#' @title
#' @description The eBird sampling events data is quite large, often maxing out available RAM even when R's memory.limit() is set to max. This function splits the sampling event data into smaller files and uses `auk` functions to import.
#' @param fn Filename for the sampling .txt file. Either fn or dir must be specified.
#' @param dir Directory for where the sampling event file(s) are stored. Either fn or dir must be specified.
#' @param dir.out Directory for where to store the subsets of the sampling events data. If not specified will default to data-local/ebird.
#' @param n.amer Logical. If working with North American countries, significantly reduces the file sizes of the sampling events data.
#' @param overwrite Logical. If .RDS file, "sampling_events.rds" exists in dir or dir.out, will NOT overwrite when FALSE.
#' @export get_ebird_sampling_events
#'

get_ebird_sampling_events <-
  function(fn = NULL,
           dir = NULL,
           dir.out = "data-local/ebird",
           n = 8000000,
           n.amer = TRUE,
           complete = TRUE,
           protocol.type = c("Traveling", "Stationary")
           ){

  # create directories if necessary for storing data.
  suppressWarnings(dir.create('data-local')); suppressWarnings(dir.create(dir.out))

  if(is.null(fn) & is.null(dir))stop("Both `fn` and `dir` cannot be NULL. Please specify one of the arguments. ")

  if(!is.null(dir)){
    fns.ebird.in=list.files(dir, full.names=TRUE)
    fn=fns.ebird.in[!str_detect(fns.ebird.in, ".zip|.tar|.gz")]
    fn=fn[str_detect(fn, "sampling")]
  }
  if(is.null(dir.out)) dir.out <- dir


# if multuiple sampling files exist let user decide which to use.
if(length(fn)>1){
  choice=menu(choices=c(fn), title="Multiple sampling events files detected. Please choose which to import:")
  if(choice==0|!exists("choice")){message("No choice specified. Defaulting to first in list."); choice=1}
  fn <- fn[choice]
  }

# break sampling file up using one of two methods: count lines, or guess (risky!)
if(is.null(n)){
  message("Counting the number of lines in file ", fn,". This may take a couple of minutes...")
  n=R.utils::countLines(fn, chunkSize=1000)
  }



  #setup parallel backend to use many processors
  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer
  doParallel::registerDoParallel(cl)

  # create indices for splitting up file
  n.lines.total = n
  n.lines.df.max = 500000 # max nu lines to import per df at a time
  n.dfs.out = as.integer(round(n.lines.total/n.lines.df.max)+1)
  # create an interval for reading lines
  interval.start = seq(2,n.lines.total, n.lines.df.max) # start at row two to avoid importing the header.
  interval.end = interval.start+n.lines.df.max
  # create headers
  headers <- read.csv(fn, header=FALSE, sep="\t", skip=0, nrows = 1)#import the header..


  foreach(i=interval.start, j=interval.end, .combine='c') %dopar% {
    library(tidyverse)
    library(auk)
    df = read.csv(
      fn,
      header = FALSE,
      sep = "\t",
      skip = i,
      nrows =n.lines.df.max)
    colnames(df) <- headers
    ebird.events <- df %>%
      filter(country %in% c("United States", "Mexico", "Canada"))
    saveRDS(ebird.events, file=paste0(dir.out, "/sampling_events_",
                                      Sys.Date(),"_",
                                      i,"_to_",j,
                                      ".rds"), compress=TRUE)


  }


# Grab the list of shit
samp.fns <- list.files(dir.out, pattern = "sampling_events_", full.names=TRUE)
return(samp.fns)

} # END FUNCTION
