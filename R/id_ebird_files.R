#' @title ...
#' @description ...
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#' @param sampling.events Logical.
#' @param mmyyyy The month (mm) and year (yyyy) of the ebird dataset you wish to use. Suggested to use most recent on file.
#' @param get.full.data Logical if TRUE will retrieve the filenames for the ENTIRE database, thereby ignoring the species
#' @export
id_ebird_files <-function(dir.ebird.in,
                          mmyyyy = "sep-2021",
                          species="doccor",
                          country.code.identifier="iso2c",
                          sampling.events=TRUE,
                          get.full.data=FALSE
                          ){

mmyyyy <- tolower(mmyyyy)
regions <- paste(tolower(apply(expand.grid(paste0(c("us", "ca", "usa", "mx", "mex"), sep="_"), species), 1, paste, collapse="")), collapse="|")

# Grab all relevant filenames in directory
## no need for fullnames because the auk package doesnt handle it well. auk requires a filename and a directory....
fns <- tolower(list.files(dir.ebird.in, full.names=FALSE))
fns <- fns[stringr::str_detect(fns, ".tar|.zip")]
fns <- fns[stringr::str_detect(fns, mmyyyy)]


# Sampling Events
fns_samp <- fns[stringr::str_detect(fns,"sampling")]  ## get sampling files
fns_samp <- fns_samp[stringr::str_detect(tolower(fns_samp), mmyyyy)] ## keep only relevant month/year


# Observations
## filename(s) for eBird input data (EBD)
fns_ebd <- tolower(list.files(dir.ebird.in))
fns_ebd <- fns_ebd[stringr::str_detect(tolower(fns_ebd), mmyyyy)] ## keep only relevant month/year

# Species
if(!is.null(species) & get.full.data){
  fns_ebd <- fns_ebd[stringr::str_detect(fns_ebd, paste0(tolower(species), collapse="|"))]
} # end species


# sampling events data (doesnt matter whether get.full.data)
fns_samp <- fns_samp[stringr::str_detect(fns_samp, "sampling_rel")]
fns_samp <- fns_samp[stringr::str_detect(fns_samp, ".tar")]

# If get.full.data==TRUE
if(get.full.data){
##observations
  str <- paste0("ebd_rel", mmyyyy, ".txt") ## this assumes the filenames do not change on ebird's part...
  fns_ebd_full <- fns_ebd[stringr::str_detect(fns_ebd, str)]
  } # end get.full.data

# If get.full.data==FALSE
if(!get.full.data){
  fns_ebd <- fns_ebd[stringr::str_detect(fns_ebd, regions)]
  fns_ebd <- fns_ebd[stringr::str_detect(fns_ebd, ".zip")]
}


## a test to ensure we have at least one file for ebird
if(length(fns_ebd)==0) "no ebd files found in dir.ebird.in. please check direcotry or specifications for id_ebird_files()"

# return the filenames
fns.final <- c(paste0(dir.ebird.in,"/", fns_ebd), fn_samp)
fns.final <- fns.final[stringr::str_detect(fns.final,".tar|.zip")==TRUE]  ## remove compressed files.


return(fns.final)

}
