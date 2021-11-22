#' @title
#' @description
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param spatial Character vector comprising state/province or country abbreviations. If not specified, will identify the files associated with the country-level data.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#'

id_ebird_files <-function(dir.ebird.in,mmyyyy="oct-2021", spatial=NULL,
                          species=NULL, country.code.identifier="iso2c", sampling.events=TRUE){

mmyyyy <- paste(mmyyyy, str_remove(mmyyyy, "-"), sep="|")


## List all the relevant files in ebird in dir
fns.ebird.in <- tolower(list.files(dir.ebird.in))
fns.ebird.in <- fns.ebird.in[str_detect(fns.ebird.in, pattern=".zip|.tar|.gz")==FALSE]# remove the .zip/.tar
## remove terms of use, code of conduct.
fns.ebird.in=fns.ebird.in[str_detect(fns.ebird.in, "conduct|terms_of_use|metadata")==FALSE]
# grab files with spatial identifiers (states, provinces, territories) in name.
fns.ebird.in=fns.ebird.in[str_detect(fns.ebird.in, mmyyyy)]

## sampling events files
sampling.fn <-  fns.ebird.in[str_detect(fns.ebird.in, pattern="sampling")] # grab this/these files for later.
sampling.fn <-  sampling.fn[str_detect(sampling.fn, pattern=mmyyyy)] # grab this/these files for later.
if(!length(sampling.fn)>0) stop(warning(paste0("No files found for month and year, ", mmyyyy,". Please ensure argument `mmyyyy` is correct and that files exist in ", dir.ebird.in)))

## create indices for country, state, prov
data(codelist, package="countrycode") #load country code dataframe
cntry=tolower(codelist[[country.code.identifier]]) #create an index for country scanning
us=paste(paste0("US-",state.abb), collapse="|")
ca=paste0("CA-",c("MB","NB", "ON", "AB","BC","YT",
                  "NL","NT","NS","NU","PE","QC","SK"),collapse = "|")
stateprov=tolower(paste(c(us,ca), collapse="|"))


## subset by spatial filters
  if(!is.null(spatial)){
    # grab files with spatial identifiers (states, provinces, territories) in name.
    fns.ebird.in.stateprov=fns.ebird.in[str_detect(fns.ebird.in, stateprov)]
  }else(fns.ebird.in.stateprov=NULL)

# if state/prov aren't indicated, search for country-level data.
  if(is.null(spatial)){
    fns.ebird.in.ctry <- fns.ebird.in[str_detect(fns.ebird.in, stateprov)==FALSE]# remove the state-level ones
    fns.ebird.in.ctry <- fns.ebird.in[str_detect(fns.ebird.in, pattern=paste(paste0(cntry,"_"), collapse="|"))]
  }else(fns.ebird.in.ctry = NULL)

## subset by species
if(!is.null(species)){
  species <- tolower(paste(species, collapse="|"))
  fns.ebird.in.spp <- fns.ebird.in[str_detect(fns.ebird.in, pattern=species)]

}else(fns.ebird.in.spp=NULL)

fns.ebird.in <- unique(c(fns.ebird.in, fns.ebird.in.ctry, fns.ebird.in.stateprov, sampling.fn))

# throw message stating these are the target files import
cat(
  "The following files were identified as target eBird files for import, according to your spatial and species filters (or lack thereof):\n",
  paste(fns.ebird.in,"\n")
)

# return the full path
fns.ebird.in <- paste0(dir.ebird.in,"/", fns.ebird.in) %>% tolower()

return(fns.ebird.in)

}
