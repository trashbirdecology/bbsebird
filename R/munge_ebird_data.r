#' @title Munge eBird reference database (EBD)
#' @description
#' @param dir.ebird.in Local directory location for eBird refernce database.
#' @param dir.ebird.out Directory for storing the munged/subsetted eBird data. This directory will also be scanned to ensure the data munging hasn't already occurred. User is asked to confirm whether they want to re-manipulate the data or import the existing .RDS.
#' @param interest.species
#' @param interest.temporal
#' @param interest.spatial
#' @param state.by.species Logical. TRUE when the file(s) in the dir.ebird.in are separated by state and/or species. FALSE when the entire EBD is in directory.
#' @export

munge_ebird_data <- function(dir.ebird.in, dir.ebird.out=NULL, interest.species=NULL, interest.temporal=NULL, interest.spatial=NULL, state.by.species=TRUE){

  # create directories if they DNE
  if(is.null(dir.ebird.out)){
    suppressWarnings(dir.create("data-local"))
    dir.ebird.out="data-local/ebird"
    suppressWarnings(dir.create(dir.ebird.out))
    }

  # scan dir.ebird.out for existing files to ensure user wants to proceed with this time-consuming task. if not, import the files.
  if(length(list.files(dir.ebird.out, pattern=".rds"))>0){
    ind=menu(title=paste(".rds files exist in ",dir.ebird.out, ". Do you wish to proceed with this length operation?"),
                                                    choices=c("yes","no"))
    if(ind==2){
      fns = list.files(dir.ebirdout, full.names=TRUE, pattern=".rds")
      ebird=list()
      for(i in 1:length(fns)) ebird[[i]] = readRDS(fns[i])
      }
  }

# If species of interest are specified, check ebird.dir.in for decompressed versions.
  if(!is.null(interest.species)){
  fns <- tolower(list.files(dir.ebird.in, full.names = TRUE))
  interest.species=tolower(interest.species)
  to.unpack <- NULL
  for(i in seq_along(fns)){
    for(j in seq_along(interest.species)){
     ind=str_detect(fns[i], interest.species[j])
    if(ind){
      to.unpack=c(fns[i], to.unpack)}
} #j loop
  if(i == length(fns)){to.unpack=unique(to.unpack)}
  }# i loop
}
# unpack all relevant files in the directory
sapply(to.unpack, unzip, overwrite=FALSE, exdir=dir.ebird.in)
# There may be a lot of files....
fns <- tolower(list.files(dir.ebird.in, pattern=".txt", full.names=TRUE))

ebird<-list()
# Import eBird metadata files
ebird$iba_codes <- read.table(fns[str_detect(fns, "ibacode|iba code")], sep = "\t", header=TRUE)
ebird$fws_codes <- read.csv(fns[str_detect(fns, "fwscode|fws code")], sep = "\t", header=TRUE)

# Filter fns by state-region
fns[grep("ca", fns)]
paste(tolower(interest.spatial),collapse="|")
str_detect(fns, "doc")

} # end function

