# #################################################################
# ## The ebird data datasets are pretty large, so will need to be updated manually.
# ## The EBD must be manually downloaded with permission through ebird.org/data/request
# ## Therefore, this function relies on a locally-stored version of the eBird data.
# ## For this modeling framework we need zero-filled data, which requires both the EBD and the Sampling Events
# ##
# ## This function allows for one species and one or more regions (e.g. CA and US).
# ### FOR NOW, HOWEVER, I AM JUST WRITING THIS FUNCTION TO WORK FOR THE US-STATES IN THE
# ### PACIFCI FLWAY
# library(auk)
# library(lubridate)
# library(sf)
# library(gridExtra)
# library(tidyverse)
# # Adjust memory limit to handle large eBird data files depending on OS
#   if(.Platform$OS.type=="unix") library(unix)
#   if(.Platform$OS.type=="windows") round(memory.limit()/2^20, 2)
#   if(.Platform$OS.type=="unix") unix::rlimit_as(1e12)#prob unnecessary
#
# #### SPECIFICATIONS FOR THIS EXAMPLE --TO BE CHANGED EVENTUALLY TO BE GENERALIZABLE
# states<-paste0("US-", c("OR", "CA","WA", "ID", "AZ", "NV"))
# country="US"
#
#
# # Unpack ebird data for a given species -------------------------------------------
# unpack_ebird(spp.ind="doccor") # unpack the .zip files


# Stage ebird data ---------------------------------------------------------
# index for quick reference
fns <- list.files("data-raw/ebird-data", full.names = TRUE)
# ebird basic database (ebd) datasets
ebd.fns <- grep("ebd_", fns, value=TRUE)
ebd.fns <- grep(".txt", ebd.fns, value=TRUE) # i don't know the grep command to search string for both patterns so this is it for now
ebd.fns <- grep(country, ebd.fns, value=TRUE) # i don't know the grep command to search string for both patterns so this is it for now

# sampling events
samp.fns <- grep("sampling", fns, value=TRUE)
samp.fns <- grep(".txt", samp.fns, value=TRUE) # i don't know the grep command to search string for both patterns so this is it for now
if(length(samp.fns)>1)warning("More than one ebd_sampling_rel file exists. Fix this before proceeding") # we should only have one sampling file

# Filtering ---------------------------------------------------------------
f_smp <- samp.fns
ebd <- auk::auk_ebd(ebd.fns,
               file_sampling = f_smp)
# specify output files
f_ebd <- paste0("data-raw/ebird-data/", "ebd_out",i , ".txt")
f_sampling <- paste0("data-raw/ebird-data/", "checklists_out.txt")

# Specify filters for the ebd file ----------------------------------------
ebd_filters <- ebd %>%
auk_protocol(c("Stationary", "Traveling")) %>%
  # Specify the taxonomy, species and country for good measure.
  # restrict to DCCO, taxonomy version 2021
  auk_species("Double-crested Cormorant", taxonomy_version = 2021) %>%
  # auk_country(c("CA", "United States")) %>%
  auk_state(state=states) %>%
  # restrict to complete checklists (needed for zero-filled data)
  auk_complete() ## JLB: need to make sure this has zeroes..

auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse=TRUE)




# export to package -------------------------------------------------------
usethis::use_data(ebird, overwrite = TRUE)
