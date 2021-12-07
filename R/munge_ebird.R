
#'
#' munge_ebird <- function(dir.ebird.in, dir.ebird.out, interest.species=NULL, interest.spatial=NULL){
#'
#'   ebird.fns <- tolower(list.files(dir.ebird.in, full.names=TRUE))
#'   ebird.fns.spp.subset <- ebird.fns[str_detect(ebird.fns, pattern=paste(interest.species, collapse="|"))]
#'
#'   ebird.fns.in <- NULL
#'   if(!is.null(interest.spatial)){
#'     ebird.fns.spp.state.subset <- ebird.fns.spp.subset[str_detect(ebird.fns.spp.subset,
#'                                                                   pattern=paste(tolower(interest.spatial), collapse="|"))]
#'   if(!length(ebird.fns.spp.state.subset)>0)stop(paste0("No files matching the regions specified in `interest.spatial` were found in directory ( ",dir.ebird.in,
#'                                                        " Please verify the files are unpacked in the correct directory, or remove argument `interest.spatial` if the directory includes the entire dataset."))
#'     else(ebird.fns.in<-c(ebird.fns.in, ebird.fns.spp.state.subset))
#'
#'     }
#'   if(is.null(interest.spatial)){
#'     ctry.ind=c(paste0("ebd_ca_", interest.species), paste0("ebd_us_", interest.species))
#'     # find files for Canada and US if no states are specified
#'     ebird.fns.spp.ctry.subset <- ebird.fns.spp.subset[str_detect(ebird.fns.spp.subset, pattern=paste(tolower(ctry.ind), collapse="|"))]
#'     #remove .zip and .tar files
#'     ebird.fns.spp.ctry.subset <- ebird.fns.spp.ctry.subset[!str_detect(ebird.fns.spp.ctry.subset, pattern=".zip|.tar")]
#'
#'     ebird.fns.in <- c(ebird.fns.in, ebird.fns.spp.ctry.subset)
#'   }
#'
#' ## Import the ebird files
#'   ## Auk seems REALLY slow, so for now I am going to avoid it..but i do need to follow-up on that..
#' ebird.fns.in <- unique(ebird.fns.in) # ensure no duplicate filenames.
#'
#' ebird.data <- list()
#' # This takes ~15-20 minutes, so import the .RDS if one is available.
#' ebird.data$events <- read.csv("C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird/ebd_sampling_relSep-2021.txt", sep="\t")
#'
#' s.read=Sys.time()
#' for(i in seq_along(ebird.fns.in)){
#'   message("importing file ", i, " of ", length(ebird.fns.in))
#'   ebird.data[[i]] <- read.csv(ebird.fns.in[i], sep="\t")
#' }
#' ebird.data <- bind_rows(ebird.data)
#' s.read=s.read-Sys.time()
#' s.auk=Sys.time()
#' s.auk=s.auk-Sys.time()
#'
#' hist(as.POSIXct.Date(samp$OBSERVATION.DATE))
#'
#' }
