#' @title
#' @description
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param spatial Character vector comprising state/province or country abbreviations. If not specified, will identify the files associated with the country-level data.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#'

id_ebird_files <-function(dir.ebird.in, spatial=NULL, species=NULL, country.code.identifier="iso2c"){

# browser()

## List all files in ebird in dir
fns.ebird.in <- tolower(list.files(dir.ebird.in))
fns.ebird.in <- fns.ebird.in[str_detect(fns.ebird.in, pattern=".zip|.tar|.gz")==FALSE]# remove the .zip/.tar
# ## throw a warning stating that there are no decompressed files in this directory
#     if (!any(str_detect(fns.ebird.in, ".zip|.tar|.gz"))) {
#       message(
#         paste0(
#           "No decompressed versions of ebird files are available in ",
#           dir.ebird.in,
#           ". Please unpack files before attempting to import data from this directory."
#         )
#       )
#     }

## create indices for country, state, prov
data(codelist, package="countrycode") #load country code dataframe
cntry=tolower(codelist[[country.code.identifier]]) #create an index for country scanning
us=paste(paste0("US-",state.abb), collapse="|")
ca=paste0("CA-",c("MB","NB", "ON", "AB","BC","YT",
                  "NL","NT","NS","NU","PE","QC","SK"),collapse = "|")
stateprov=tolower(paste(c(us,ca), collapse="|"))

## subset by species
if(!is.null(species)){
  species<-tolower(paste(species, collapse="|"))
  fns.ebird.in <- fns.ebird.in[str_detect(fns.ebird.in, pattern=species)]

}


## subset by spatial filters
  if(!is.null(spatial)){
    # grab files with spatial identifiers (states, provinces, territories) in name.
    str_detect(fns.ebird.in, stateprov)
  }
  # if state/prov aren't indicated, search for country data.
  if(is.null(spatial)){
    fns.ebird.in <- fns.ebird.in[str_detect(fns.ebird.in, stateprov)==FALSE]# remove the state-level ones
    fns.ebird.in <- fns.ebird.in[str_detect(fns.ebird.in, pattern=paste(paste0(cntry,"_"), collapse="|"))]
  }

## remove terms of use, code of conduct.
fns.ebird.in=fns.ebird.in[str_detect(fns.ebird.in, "conduct|terms_of_use|metadata")==FALSE]

# throw message stating these are the target files import
cat(
  "The following files were identified as target eBird files for import, according to your spatial and species filters (or lack thereof):\n",
  paste(fns.ebird.in, "\n")
)

# return the full path
fns.ebird.in = paste0(dir.ebird.in, fns.ebird.in)
return(fns.ebird.in)

}
