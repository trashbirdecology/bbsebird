#' @title ...
#' @description ...
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param regions Character vector comprising state/province or country abbreviations. If not specified, will identify the files associated with the country-level data.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#' @param sampling.events Logical.
#' @param mmyyyy The month (mm) and year (yyyy) of the ebird dataset you wish to use. Suggested to use most recent on file.
#' @param get.full.data Logical if TRUE will retrieve the filenames for the ENTIRE database, thereby ignoring the species and regions.
#' @export
id_ebird_files <-function(dir.ebird.in, mmyyyy="oct-2021", regions=NULL,
                          species="doccor", country.code.identifier="iso2c",
                          sampling.events=TRUE,
                          get.full.data=FALSE
                          ){

# mmyyyy.collapse <- paste(mmyyyy, str_remove(mmyyyy, "-"), sep="|")
mmyyyy <- tolower(mmyyyy)
mm.string = paste0("(?=.*",substr(mmyyyy,1,3),")(?=.*txt)")

## Get filename for the sampling events in directory
fn_samp <- tolower(list.files(dir.ebird.in, full.names=TRUE))#no need for fullnames because the auk package doesnt handle it well. needs a filename and a directory.
fn_samp <- fn_samp[stringr::str_detect(fn_samp,"sampling")]  ## get sampling files
fn_samp <- fn_samp[stringr::str_detect(fn_samp,".tar|.gz|.zip")==FALSE]  ## remove compressed files.
fn_samp <- fn_samp[stringr::str_detect(tolower(fn_samp), mmyyyy)] ## keep only relevant month/year

# Filename(s) for eBird input data (EBD)
fns_ebd <- tolower(list.files(dir.ebird.in, full.names=FALSE)) #no need for fullnames because the auk package doesnt handle it well. needs a filename and a directory.
fns_ebd <- fns_ebd[grepl(mm.string, tolower(fns_ebd), perl = TRUE)]
### If we dont want the entire dataset...
if(!get.full.data){
## If species are provided, find those files
if(!is.null(species)) fns_ebd <- fns_ebd[stringr::str_detect(fns_ebd, paste0(tolower(species), collapse="|"))]
## If regions are provided, find those files
if(!is.null(regions)){
  # remove the national-level data (too cumbersome)
  fns_ebd <- fns_ebd[!stringr::str_detect(fns_ebd, c("US_|CA_|sampling"))]
  # keep only the states we need.
  fns_ebd <- fns_ebd[stringr::str_detect(fns_ebd, paste(tolower(states), collapse="|"))]
}else(fns_ebd <- fns_ebd[stringr::str_detect(fns_ebd, tolower(c("US_|CA_|sampling")))])}
if(get.full.data){
  str <- paste0("ebd_rel", mmyyyy, ".txt") ## this assumest he filenames do not change on ebird's part...
  fns_ebd_full <- fns_ebd[stringr::str_detect(fns_ebd, str)]
  fns_ebd <- fns_ebd_full[!stringr::str_detect(fns_ebd_full, ".gz|.zip")]
}

## a test to ensure we have at least one file for ebird
if(length(fns_ebd)==0) "no ebd files found in dir.ebird.in. please check direcotry or specifications for id_ebird_files()"

# return the filenames
fns.final <- c(paste0(dir.ebird.in,"/", fns_ebd), fn_samp)
fns.final <- fns.final[stringr::str_detect(fns.final,".tar|.gz|.zip")==FALSE]  ## remove compressed files.
fns.final <- fns.final[stringr::str_detect(fns.final,".txt|.csv")==TRUE]  ## remove compressed files.


# throw message stating these are the target files import
cat(
  "The following files were identified as target eBird files for import, according to your regions and species filters (or lack thereof):\n",
  paste(fns.final,"\n"))


return(fns.final)

}
