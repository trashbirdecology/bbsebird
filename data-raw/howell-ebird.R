# Libraries and functions -----------------------------------------------------

# Load libraries
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)

# Increase memory limit
# memory.limit(100000) ## THIS IS WINDOWS SPECIFIC SO WILL NEED TO CHECK IF MACHIEN IS WINDOWS THEN DO IT

# Load the most recently retrieved eBird Basic Dataset -----------------------------------------
## This dataset is pretty large, so will need to be updated manually.

# To produce zero-filled data, provide an EBD and sampling event data file

# eBird Basic Dataset (EBD)

# Sampling events
f_smp <-"./data/ebird/ebd_dcco_relAug-2021/ebd_sampling_relAug-2021.txt"

# Create a reference to an eBird Basic Dataset (EBD) and sampling event file in
# preparation for filtering using AWK.
ebd <- auk_ebd(f_ebd,
               file_sampling = f_smp)

# Set up filters
ebd_filters <- ebd %>%
  # restrict to DCCO, taxonomy version 2021
  auk_species("Double-crested Cormorant", taxonomy_version = 2021) %>%
  # restrict to North America
  auk_country(c("CA", "United States")) %>%
  # restrict to Stationary and Traveling checklists
  auk_protocol(c("Stationary", "Traveling")) %>%
  # restrict to complete checklists (needed for zero-filled data)
  auk_complete()

ebd_filters

# Set up output files
f_ebd <- "./data/ebird/ebd_dcco_relAug-2021/ebd_dcco_US_CA_2003_2021.txt"
f_sampling <- "./data/ebird/ebd_dcco_relAug-2021/ebd_checklists_US_CA_2003_2021.txt"


# only run if the files don't already exist - this will take several hours
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

# Zero-fill
ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse=TRUE)

# clean up variables
ebd_zf <- ebd_zf %>%
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X",
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling",
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),

    # convert duration in minutes to duration in hours
    effort_hours = round(duration_minutes/60, 3),

    # Convert the protocol_type to be a 1 for traveling and 0 for stationary
    I_stationary = ifelse(protocol_type == "Traveling", 1, 0),

    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date),

    # Add extra empty column for observer score
    score = NA
  )

# additional filtering
# ebd_zf_filtered <- ebd_zf %>%
#  filter(
# effort filters
#    duration_minutes <= 10 * 60, #This is very coarse - we usually keep this to 5 hrs or less
#    effort_distance_km <= 15, #This is coarse also, typically 5km or less
#    year >= 2008, # failsafe to get only date from 2008 onward in case I put in the date filter incorrectly above
# 10 or fewer observers
#    number_observers <= 10)


ebird <- # ebd_zf_filtered %>%
  ebd_zf %>%
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed,
         state_code, locality_id, latitude, longitude,
         #protocol_type,
         I_stationary,
         all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started,
         # duration_minutes,
         effort_hours,
         effort_distance_km,
         number_observers,
         score)

write_csv(ebird, "./data/formatted/ebd_dcco_zf_2021-10-05.csv", na = "")


# Old eBird data carpentry-----------------------------------------------------

## Load data
#dcco_ebird_db <- DBI::dbConnect(RSQLite::SQLite(), "./data/ebird/doccor-ERD2019-ORIN-20201008-3e741b65_data.db")
#src_dbi(dcco_ebird_db)

## Make a dataframe out of the SQLite database
#dcco_ebird_erd <- tbl(dcco_ebird_db, "erd") %>%
#collect()

#DBI::dbDisconnect(dcco_ebird_db) # call when finished working with a connection

#dcco_ebird <- dcco_ebird_erd %>%
#  filter(!is.na(obs)) # remove checklists with missing counts

#rm(dcco_ebird_erd)

## Remove duplicate dcco_ebird sampling events. These are normally removed by the AUK package from Cornell, but here I am using the raw data.
#dcco_ebird <- dcco_ebird[!duplicated(dcco_ebird$SAMPLING_EVENT_ID),]
#saveRDS(dcco_ebird, file="./data/formatted/dcco_ebird_20201008_2021-03-23.rds")

