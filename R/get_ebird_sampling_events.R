#' @title Retrieve the eBird sampling events from local directory.
#' @description The eBird sampling events data is quite large, often maxing out available RAM even when R's memory.limit() is set to max. This function splits the sampling event data into smaller files and uses `auk` functions to import.
#' @param f_samp Filename for the sampling .txt file. Either f_samp or dir must be specified.
#' @param dir Directory for where the sampling event file(s) are stored. Either f_samp or dir must be specified.
#' @param dir.out Directory for where to store the subsets of the sampling events data. If not specified will default to data-local/ebird.
#' @param n.amer Logical. If working with North American countries, significantly reduces the file sizes of the sampling events data.
#' @param overwrite Logical. If .RDS file, "sampling_events.rds" exists in dir or dir.out, will NOT overwrite when FALSE.
#' @export get_ebird_sampling_events
#'

get_ebird_sampling_events <-
  function(f_samp = NULL,
           dir = NULL,
           dir.out = "data-local/ebird/sampling-out/",
           n = 8000000,
           n.amer = TRUE,
           complete = TRUE,
           protocol.type = c("Traveling", "Stationary")
           ){
# browser()
  # create directories if necessary for storing data.
  suppressWarnings(dir.create('data-local')); suppressWarnings(dir.create(dir.out))

  if(is.null(f_samp) & is.null(dir))stop("Both `f_samp` and `dir` cannot be NULL. Please specify one of the arguments. ")

  if(!is.null(dir) & is.null(f_samp)){
    f_samps.ebird.in=list.files(dir, full.names=TRUE)
    f_samp=f_samps.ebird.in[!str_detect(f_samps.ebird.in, ".zip|.tar|.gz")]
    f_samp=f_samp[str_detect(f_samp, "sampling")]
  }
  if(is.null(dir.out)) dir.out <- dir


# if multuiple sampling files exist let user decide which to use.
if(length(f_samp)>1){
  choice=menu(choices=c(f_samp), title="Multiple sampling events files detected. Please choose which to import:")
  if(choice==0|!exists("choice")){message("No choice specified. Defaulting to first in list."); choice=1}
  f_samp <- f_samp[choice]
  }

# break sampling file up using one of two methods: count lines, or guess (risky!)
if(is.null(n)){
  message("Counting the number of lines in file ", f_samp,". This may take a couple of minutes...")
  n=R.utils::countLines(f_samp, chunkSize=1000)
  }

#setup parallel backend to use many processors
cores=parallel::detectCores()
cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer
doParallel::registerDoParallel(cl)

# create indices for splitting up file
n.lines.df.max =50000 # max nu lines to import per df at a time
# (n.dfs.out = as.integer(round(n/n.lines.df.max)+1))
# create an interval for reading lines
interval.start = seq(2,n, n.lines.df.max) # start at row two to avoid importing the header.
# create headers
header <- read.csv(f_samp, header=TRUE, sep="\t", nrows = 1) #import the header..# for some reason when i do nrows=0 it takes a long ass time...
header <- names(header)
interval.start=interval.start[1:2]
foreach(i=interval.start, .combine='c') %dopar% {
    library(magrittr)
    library(dplyr)
    library(auk)
    library(vroom)


  df = vroom(file= f_samp,delim = "\t")
      df <- df %>%
      filter(country %in% c("United States", "Mexico", "Canada"))
    saveRDS(df, file=paste0(dir.out, "/sampling_events_",
                                      Sys.Date(),"_startrow_",
                                      i,".rds"), compress=TRUE)

  rm(df)
  gc()
  }


# Grab the list of shit
samp.f_samps <- list.files(dir.out, pattern = "sampling_events_", full.names=TRUE)
return(samp.f_samps)

} # END FUNCTION
