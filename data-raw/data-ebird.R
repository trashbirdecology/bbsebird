#################################################################
## The ebird data datasets are pretty large, so will need to be updated manually.
## The EBD must be manually downloaded with permission through ebird.org/data/request
## Therefore, this function relies on a locally-stored version of the eBird data.
## For this modeling framework we need zero-filled data, which requires both the EBD and the Sampling Events
##
## This function allows for one species and one or more regions (e.g. CA and US).
### FOR NOW, HOWEVER, I AM JUST WRITING THIS FUNCTION TO WORK FOR THE US-STATES IN THE
### PACIFCI FLWAY
library(lubridate)
library(sf)
library(gridExtra)

library(tidyverse)


#### IMPORTANT:::SPECIFICATIONS FOR THIS EXAMPLE --TO BE CHANGED EVENTUALLY TO BE GENERALIZABLE
states<-paste0("US-", c("OR", "CA","WA", "ID", "AZ", "NV"))

# Unpack ebird data for a given species -------------------------------------------
unpack_ebird(spp.ind="doccor") # unpack the .zip files

# Define reference to local files -----------------------------------------
# local filenames
fns <- list.files("data-raw/ebird-data", full.names = TRUE)
f_ebd <-grep(".txt", grep("ebd_US_doccor_rel" ,fns, value = TRUE), value=TRUE) # paige's f_ebd
f_samp <-grep(".txt", grep("sampling" ,fns, value = TRUE), value=TRUE) # paige's f_ebd

# Set up output files
ebd.out <- "data-raw/ebird-data/ebd_dcco_filtered.txt"
checklist.out <- "data-raw/ebird-data/checklist_dcco_filtered.txt"



# Define filters ----------------------------------------------------------
ebd <-  auk_ebd(f_ebd,f_samp)
ebd_filters <- ebd %>%
  auk_state(paste0("US-", c("OR", "CA","WA", "ID", "AZ", "NV"))) %>%
  # restrict to Stationary and Traveling checklists
  auk_protocol(c("Stationary", "Traveling")) %>%
  # restrict to complete checklists (needed for zero-filled data)
  auk_year(c(2008:2020)) %>%
  auk_complete()

auk_filter(ebd_filters, file = ebd.out, file_sampling = checklist.out, overwrite=TRUE)


# Zero-fill the text file (takes a while)
ebd_zf <- auk_zerofill(ebd.out, checklist.out, collapse=TRUE)

