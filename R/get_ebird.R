#' @title Create the Zero-filled eBird Data
#' @description Creates zero-filled data from select eBird EBD (ebd_rel) and Sampling Events (ebd_sampling) files. If the files already exist in dir.ebird.munged
#' @param fns File paths for the EBD and SamplinEvents data to import. Character vector of filenames for original files.
#' @param mmyyyy Month and year of desired input data as specified in fns. This format is default for eBird downloads.
#' @param species Names of species of interest.
#' @param overwrite Logical. If true,
#' @param dir.ebird.munged Location of where to save and find the filtered, zero-filled data. If not specified will default to subdir in project directory.
get_zerofilled_ebird <- function(fns=NULL, mmyyyy="oct-2021", species=NULL, overwrite=FALSE, dir.ebird.munged="data-local/ebird/"){

f_ebd_in  <- fns[str_detect(fns, paste0("ebd_rel",mmyyyy))]
f_samp_in <- fns[str_detect(fns, paste0("sampling_rel",mmyyyy))]
if(!length(f_ebd_in)>0) stop(paste0("No ebd file identified. "))
if(!length(f_samp_in)>0) stop(paste0("No sampling file identified. "))

## filenames for outputs
f_samp_out <- paste0(dir.ebird.munged,"ebd_samp_", mmyyyy,"_out.txt")
f_ebd_out <- paste0(dir.ebird.munged,"ebd_", mmyyyy, "_out.txt")
f_ebd_zf_out <- paste0(dir.ebird.munged,"zf_", mmyyyy,"_out.txt")

## filter the ebd data and saves to local file if DNE or overwrite is TRUE. otherwise, does nothing.
if (!file.exists(f_ebd_out)|(file.exists(f_ebd_out) & overwrite==TRUE)) {
  print("This process will take 2 or 3 minutes.")
    #point to relevant files for auk
    ebd <- auk_ebd(file=f_ebd_in, file_sampling = f_samp_in)
    #define the filters for use in AWK
    ebd_filters <- ebd %>%
      auk_species("Double-crested Cormorant") %>% ## will add this later
      auk_protocol(c("Traveling","Stationary")) %>% ## will add this later
      auk_country(c("United States", "Canada")) %>% ## will add this later
      auk_complete()
    #apply the filters using AWK
    ebd_filtered <-
      auk_filter(
        ebd_filters,
        file = f_ebd_out,
        file_sampling = f_samp_out,
        overwrite = overwrite
      )
    }else(ebd_filtered <- read_ebd(f_ebd_out))


## zero-fill the data if the zero-filled .txt doesn't already exist (or if overwrite is true)
if (!file.exists(f_ebd_zf_out)|(file.exists(f_ebd_zf_out) & overwrite==TRUE)) {
  print("Creating the zero-filled data. This takes like 10-20 minutes. ")
    ebd_zf <- auk_zerofill(ebd_filtered, # the output filenames should already be defined in ebd_filtered$output
        collapse = TRUE) # collapse turns the list into a single dataframe. false will return a list.
    # write the files to avoid wait times for zero-filling
    # this will take about two minutes.
    vroom::vroom_write(ebd_zf, file=f_ebd_zf_out) # write as .txt. Reading with vroom is MUCH faster than reading an RDS

}else(paste0("File ",  f_ebd_zf_out, " already exists.\nIf you wish to overwrite, specify overwrite=TRUE or remove existing files."))
##import the ebd_zf if necessary. first try using RDS, then .txt
if(!exists("ebd_zf")){
  # takes about 2 minutes
ebd_zf <- vroom::vroom(f_ebd_zf_out)}


return(ebd_zf)

} # END FUNCTION
