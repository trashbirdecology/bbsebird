#' @title ...
#' @description ...
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param regions Character vector comprising state/province or country abbreviations. If not specified, will identify the files associated with the country-level data.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#' @param sampling.events Logical.

id_ebird_files <-function(dir.ebird.in, mmyyyy="sep-2021", regions=NULL,
                          species="doccor", country.code.identifier="iso2c",
                          sampling.events=TRUE){

# mmyyyy.collapse <- paste(mmyyyy, str_remove(mmyyyy, "-"), sep="|")
mmyyyy <- tolower(mmyyyy)

## Get filename for the sampling events in directory
fn_samp <- tolower(list.files(dir.ebird.in, full.names=TRUE))#no need for fullnames because the auk package doesnt handle it well. needs a filename and a directory.
fn_samp <- fn_samp[str_detect(fn_samp,"sampling")]  ## get sampling files
fn_samp <- fn_samp[str_detect(fn_samp,".tar|.gz|.zip")==FALSE]  ## remove compressed files.
fn_samp <- fn_samp[str_detect(tolower(fn_samp), mmyyyy)] ## keep only relevant month/year

# Filenames for eBird input data (EBD)
fns_ebd <- tolower(list.files(dir.ebird.in, full.names=FALSE)) #no need for fullnames because the auk package doesnt handle it well. needs a filename and a directory.
## If species are provided, find those files
mm.string = paste0("(?=.*",substr(mmyyyy,1,3),")(?=.*txt)")
fns_ebd <- fns_ebd[grepl(mm.string, tolower(fns_ebd), perl = TRUE)]
fns_ebd <- fns_ebd[str_detect(fns_ebd, paste0(tolower(species), collapse="|"))]

## If regions are provided, find those files
if(!is.null(regions)){
  # remove the national-level data (too cumbersome)
  fns_ebd <- fns_ebd[!str_detect(fns_ebd, c("US_|CA_|sampling"))]
  # keep only the states we need.
  fns_ebd <- fns_ebd[str_detect(fns_ebd, paste(tolower(states), collapse="|"))]
}else(fns_ebd <- fns_ebd[str_detect(fns_ebd, tolower(c("US_|CA_|sampling")))])


# return the filenames
fns.final <- c(paste0(dir.ebird.in,"/", fns_ebd), fn_samp)
fns.final <- fns.final[str_detect(fns.final,".tar|.gz|.zip")==FALSE]  ## remove compressed files.

# throw message stating these are the target files import
cat(
  "The following files were identified as target eBird files for import, according to your regions and species filters (or lack thereof):\n",
  paste(fns.final,"\n"))


return(fns.final)

}
