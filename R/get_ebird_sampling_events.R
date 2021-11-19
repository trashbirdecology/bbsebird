#' @title
#' @description The eBird sampling events data is quite large, often maxing out available RAM even when R's memory.limit() is set to max. This function splits the sampling event data into smaller files and uses `auk` functions to import.
#' @param fn Filename for the sampling .txt file. Either fn or dir must be specified.
#' @param dir Directory for where the sampling event file(s) are stored. Either fn or dir must be specified.
#' @param dir.out Directory for where to store the subsets of the sampling events data. If not specified will default to data-local/ebird.
#' @param n.amer Logical. If working with North American countries, significantly reduces the file sizes of the sampling events data.
#' @param overwrite Logical. If .RDS file, "sampling_events.rds" exists in dir or dir.out, will NOT overwrite when FALSE.
#' @export get_ebird_sampling_events
#'

get_ebird_sampling_events <- function(fn=NULL, dir=NULL, dir.out="data-local/ebird", n=NULL, n.amer=TRUE, complete=TRUE, protocol.type=c("Traveling", "Stationary"),
                                      overwrite=FALSE
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

  # check to see if .rds file containing smapling data already exists.
  if(overwrite==FALSE){
    temp=c(list.files(dir,pattern="sampling_events.rds", full.names=TRUE),
           list.files(dir.out,pattern="sampling_events.rds", full.names=TRUE))
    if(length(temp)>0){
      message("Sampling events compressed RDS already exists. Importing from file, ", temp[1])
      # read in the file if it exists
      ebird.events.df <- readRDS(temp)
      return(ebird.events.df)
    }
  }

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

n.lines.total = n
n.lines.df.max = 5000 # max nu lines to import per df at a time
n.dfs.out = n.lines.total/n.lines.df.max
n.dfs.per.list = 50 # iteratively save the data when we hit every Nth df
n.files.out = round(n.dfs.out/n.dfs.per.file)+1
# create an interval for reading lines
ind.lines.interval = seq(2,n.lines.total, n.lines.df.max) # start at row two to avoid importing the header.
ind.lines.subset = ind.lines.interval[ c( rep(FALSE, n.dfs.per.list), TRUE ) ]


# Initializes the progress bar
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = n.files.out)
pb$tick(0)
# Create headers
headers <- read.csv(fn, header=FALSE, sep="\t", skip=0, nrows = 1)#import the header..
# create an index for the i-loop over which we will create a single output file, comprising n.dfs.per.file dfs smashed together..
for(j in 1:n.files.out){
pb$tick(1) # progress bar
  if(j==1){lines.start=2}else(lines.start=ind.lines.subset[j-1]+1)
  lines.end=ind.lines.subset[j]
  interval=seq(lines.start, lines.end, n.lines.df.max)
  ebird.events <- list() # init empty list on each j-loop

# create empty list
# loop over the entire .txt file for sampling events.
for(i in seq_along(interval)) { # go along the entire sampling events dataframe
  # print(i)
    df = read.csv(
      fn,
      header = FALSE,
      sep = "\t",
      skip = interval[i],
      nrows =n.lines.df.max
    )

  colnames(df) <- headers
  ebird.events[[i]] <- df %>%
    filter(country %in% c("United States", "Mexico", "Canada"))

}

ebird.events.df <- bind_rows(ebird.events)
saveRDS(ebird.events.df, file=paste0(dir.out, "/sampling_events_",j,"_.rds"), compress=TRUE)
rm(ebird.events.df)
} # end j-loop

samp.fns <- list.files(dir.out, pattern = "sampling_events_", full.names=TRUE)
return(samp.fns)

} # END FUNCTION
