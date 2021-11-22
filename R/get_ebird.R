
get_ebird <- function(fns, mmyyyy="oct-2021", species=NULL, overwrite=FALSE, dir.ebird.in, dir.ebird.out="data-local/ebird/"){

## gonna have to add arugments/logicals for when .RDS files exist.
fns.rds <- list.files(dir.ebird.out, pattern=".rds")

f_ebd_in  <- fns[str_detect(fns, paste0("ebd_rel",mmyyyy))]
f_samp_in <- fns[str_detect(fns, paste0("sampling_rel",mmyyyy))]
if(!length(f_ebd_in)>0) stop(paste0("No ebd file identified. "))
if(!length(f_samp_in)>0) stop(paste0("No sampling file identified. "))

## filenames for outputs
f_samp_out <- paste0(dir.ebird.out,"ebd_samp_", mmyyyy,"_out.txt")
f_ebd_out <- paste0(dir.ebird.out,"ebd_", mmyyyy, "_out.txt")
f_ebd_zf_out <- paste0(dir.ebird.out,"zf_", mmyyyy,"_out.txt")
f_ebd_rds <- paste0(dir.ebird.out,"ebd_", mmyyyy,".rds")
f_ebd_zf_rds <- paste0(dir.ebird.out,"zf_", mmyyyy,".rds")


## Would be good to parallelize this for x numbers of f_ebd files in (e.g., Canada and United States subsets)

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
    vroom::vroom_write(ebd_zf, file=f_ebd_zf_out) # write as .txt just in case RDS acts up
    # saveRDS(ebd_zf, file=f_ebd_zf_rds)

    # do some munging
    ebd_zf <- clean_zf(ebd_zf)

}else(paste0("File ", f_ebd_zf_rds," or  ", f_ebd_zf_out, " already exists.\nIf you wish to overwrite, specify overwrite=TRUE or remove existing files."))


# Import the ebd_zf if necessary. first try using RDS, then .txt
if(!exists("ebd_zf")){
print(paste0("Reading the zero-filled EBD data from file ", f_ebd_zf_rds ,". This could take a while......"))
  t3=Sys.time()
  ebd_zf <- read
# ebd_zf <- readRDS(f_ebd_zf_rds)
  print(paste0("Reading the  zero-filled .RDS took time: ", t))
  t3=Sys.time()-t3
}else("Object ebd_zf already exists. Use `rm(ebd_zf)` if you wish to overwrite.")

## Create alarms
windows_alert()
song()
print(paste0("Zero-filled EBD file saved to:\n",f_ebd_zf_rds))

return(f_ebd_zf_rds)




} # END FUNCTION
