# I CANT DO THIS ON MY MACHINE

# Housekeeping ------------------------------------------------------------
# Load libraries ### need to put into package
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
if(.Platform$OS.type=="unix") library(unix)

# Adjust memory limit to handle large eBird data files depending on OS
if(.Platform$OS.type=="windows") round(memory.limit()/2^20, 2)
if(.Platform$OS.type=="unix") unix::rlimit_as(1e12)#prob unnecessary


# LOAD EBIRD DATA ---------------------------------------------------------------
## The ebird data datasets are pretty large, so will need to be updated manually.
## The EBD must be manually downloaded with permission through ebird.org/data/request
## Therefore, this function relies on a locally-stored version of the eBird data.
## For this modeling framework we need zero-filled data, which requires both the EBD and the Sampling Events

# eBird Basic Dataset (EBD) -----------------------------------------
# Sampling events
f_smp <-"/data-raw/ebd_sampling_relAug-2021.txt"

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
  auk_complete() ## JLB: need to make sure this has zeroes..


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


ebird <-  ebd_zf %>%
  select(
    checklist_id,
    observer_id,
    sampling_event_identifier,
    scientific_name,
    observation_count,
    species_observed,
    state_code,
    locality_id,
    latitude,
    longitude,
    #protocol_type,
    I_stationary,
    all_species_reported,
    observation_date,
    year,
    day_of_year,
    time_observations_started,
    # duration_minutes,
    effort_hours,
    effort_distance_km,
    number_observers,
    score
  )


# Save data to package -------------------------------------------------------------

