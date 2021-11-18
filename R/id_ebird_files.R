#' @title
#' @description
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param spatial Character vector comprising state/province or country abbreviations. If not specified, will identify the files associated with the country-level data.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#'

id_ebird_files <-function(dir.ebird.in, spatial=NULL, species=NULL, country.code.identifier="iso2c"){
 ## List all files in ebird in dir
fns.ebird.in <- list.files(dir.ebird.in, full.names=TRUE)

## create indices for country, sttae, prov
data(codelist, package="countrycode") #load country code dataframe
cntry=paste(codelist[[country.code.identifier]], collapse="|") #create an index for country scanning
stateprov=paste(paste(paste0("US-",state.abb), collapse="|"),
                 paste0("CA-",c("MB","NB", "ON", "AB","BC","YT",
                                "NL","NT","NS","NU","PE","QC","SK")),
                 collapse = "|")




## throw a warning stating that there are no decompressed files in this directory
    if (!any(str_detect(fns.ebird.in, c(".zip", ".tar")))) {
      message(
        paste0(
          "No decompressed versions of ebird files are available in ",
          dir.ebird.in,
          ". Please unpack files before attempting to import data from this directory."
        )
      )
    }

## subset by spatial filter
  if(!is.null(spatial)){
    # grab files with spatial identifiers in name.
    str_detect(fns.ebird.in, pattern=paste(spatial, collapse="|"))
    # keep only the decompressed files.(future woudl like to just unpack if no .txt/csv exist for the file)
    fns.ebird.in[str_detect(fns.ebird.in, pattern=".zip|.tar")]
  }
  if(is.null(spatial))



}
