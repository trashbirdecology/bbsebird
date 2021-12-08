#' @title Create and Write or Load In the Filtered eBird Data
#' @description Filter the eBird data and sampling events using R package AUK.
#' @param fns.ebird File paths for the EBD and SamplingEvents data to import. Character vector of filenames for original files.
#' @param overwrite Logical. If true will overwrite existing filtered data objects in project directory.
#' @param dir.ebird.out Location of where to save and find the filtered/subsetted data.
#' If not specified will default to subdir in project directory.
filter_ebird_data <-
  function(fns.ebird,
           dir.ebird.out,
           countries = NULL,
           states = NULL,
           complete.only = TRUE,
           protocol = c("Traveling", "Stationary"),
           species = "Double-crested Cormorant",
           overwrite = FALSE,
           remove.bbs.obs = TRUE,
           years = NULL,
           method = "data.table",
           # how to read in sampling data ... vroom often crashes
           cols.keep =
             c(
               "all_species_reported",
               "country",
               "common_name",
               "country_code",
               "county",
               "county_code",
               "duration_minutes",
               "effort_area_ha",
               "effort_distance_km",
               "group_identifier",
               "latitude",
               "longitude",
               "number_observers",
               "observation_date",
               "observation_count",
               "observer_id",
               "protocol_code",
               "protocol_type",
               "sampling_event_identifier",
               "state",
               "scientific_name",
               "state_code",
               "time_observations_started"
             ),
           f_obs_out = paste0(dir.ebird.out, 'ebird_obs_filtered.txt'),
           f_samp_out  = paste0(dir.ebird.out, 'ebird_samp_filtered.txt')) {


    f_samp_in  <- fns.ebird[str_detect(fns.ebird, "sampling_rel")]
    f_obs_in <- setdiff(fns.ebird, f_samp_in)
    if (!length(f_obs_in) > 0)
      stop(paste0("No ebd file identified. "))
    if (!length(f_samp_in) > 0)
      stop(paste0("No sampling file identified. "))


    #specifying the column types helps with vroom::vroom(f_samp_in), which takes a couple of minutes...
    cols_samp <- list(
      `LAST EDITED DATE` = col_datetime(),
      country = col_character(),
      `COUNTRY CODE` = col_character(),
      STATE = col_character(),
      `STATE CODE` = col_character(),
      COUNTY = col_character(),
      `COUNTY CODE` = col_character(),
      `IBA CODE` = col_character(),
      `BCR CODE` = col_double(),
      `USFWS CODE` = col_character(),
      `ATLAS BLOCK` = col_character(),
      LOCALITY = col_character(),
      `LOCALITY ID` = col_character(),
      `LOCALITY TYPE` = col_character(),
      LATITUDE = col_double(),
      LONGITUDE = col_double(),
      `OBSERVATION DATE` = col_date(),
      `TIME OBSERVATIONS STARTED` = col_time(),
      `OBSERVER ID` = col_character(),
      `sampling event identifier` = col_character(),
      `protocol type` = col_character(),
      `PROTOCOL CODE` = col_character(),
      `PROJECT CODE` = col_character(),
      `duration minutes` = col_double(),
      `EFFORT DISTANCE KM` = col_double(),
      `EFFORT AREA HA` = col_double(),
      `NUMBER OBSERVERS` = col_double(),
      `ALL SPECIES REPORTED` = col_double(),
      `GROUP IDENTIFIER` = col_character(),
      `TRIP COMMENTS` = col_character()
    )

    ## Read in / filter sampling data frame
    if (file.exists(f_samp_out) & !overwrite) {
      sampling <- vroom::vroom(f_samp_out)
    } else{
      if (!exists("sampling"))
        cat("Importing the eBird sampling events data.
            This may take a minute.")
      if (method == "vroom") {
        sampling <- vroom::vroom(f_samp_in, col_types = cols_samp)
      }
      if (method == "data.table") {
        sampling <- data.table::fread(f_samp_in) %>% as_tibble()
      }

      ##force colnames to lower and replace spaces with underscore (_)
      colnames(sampling) <-
        str_replace_all(tolower(colnames(sampling)),
                        pattern = " ",
                        replacement = "_")
      ## keep only useful columns
      sampling <- sampling[names(sampling) %in% cols.keep]
      cat("Filtering sampling events. This takes a minute.")
      # trying to keep in order of largest cut to smaller to help with memory issues.
      sampling <- sampling %>%
        filter(if (!is.null(countries))
          country %in% countries) %>%
        filter(if (!is.null(states))
          state %in% states)
      sampling <- sampling %>%
        filter(if (complete.only)
          all_species_reported %in% c("TRUE", "True", 1))
      sampling <- sampling %>%
        filter(if (!is.null(protocol))
          protocol_type %in% protocol)

      # filter by year
      sampling <- sampling %>%
        mutate(year = lubridate::year(observation_date)) %>%
        filter(if (!is.null(years))
          year %in% years)


      # remove duplicate checklists for same birding party.
      # for good measure..
      sampling <- auk_unique(sampling, checklists_only = TRUE)


      # attempt to remove BBS observations if specified
      ### THIS IS A BIG ASSUMPTION SO WILL NEED TO REVISIT EVENTUALLY!!!
      if (remove.bbs.obs) {
        sampling <- sampling %>%
          filter(protocol_type != "Stationary" &
                   duration_minutes != 3)
      ## perhaps also remove the longer (3+hours and >Xkm) observations...
        # sampling <-
      }

      # ensure consistency in col types
      sampling <- convert_cols(sampling)


      ## write the filtered sampling data
      if (method == "vroom")
        vroom::vroom_write(sampling, f_samp_out)
      if (method == "data.table")
        data.table::fwrite(sampling, f_samp_out)

      ## remove the files that vroom creates in R session's temp directory just in case
      try(fs::file_delete(list.files(tempdir(), full.names = TRUE)), silent =
            TRUE)
      gc()

    }

    ## Read in / filter observations data frame
    if (file.exists(f_obs_out) & !overwrite) {
      if (method == "vroom")
        observations <- vroom::vroom(f_obs_out, col_types = col_types)
      if (method == "data.table")
        observations <- data.table::fread(f_obs_out)
    } else{
      # only use vroom to make it easier to read many files at once...
      observations <- vroom::vroom(f_obs_in, col_types = cols_samp)

      ##force colnames to lower and replace spaces with underscore (_)
      colnames(observations) <-
        str_replace_all(tolower(colnames(observations)),
                        pattern = " ",
                        replacement = "_")
      ## keep only useful columns
      observations <-
        observations[names(observations) %in% cols.keep]

      # filter by year
      observations <- observations %>%
        mutate(year = lubridate::year(observation_date)) %>%
        filter(if (!is.null(years))
          year %in% years)

      if (remove.bbs.obs) {
        observations <- observations %>%
          filter(protocol_type != "Stationary" &
                   duration_minutes != 3)
      }


      # trying to keep in order of largest cut to smaller to help with memory issues.
      observations <- observations %>%
        filter(if (complete.only)
          all_species_reported %in% c("TRUE", "True", 1)) %>%
        filter(if (!is.null(species))
          common_name %in% species) %>%
        filter(if (!is.null(countries))
          country %in% countries) %>%
        filter(if (!is.null(states))
          state %in% states) %>%
        filter(if (!is.null(protocol))
          protocol_type %in% protocol)

      # collapse duplicate checklists into one, taking the max number identified by the group during an event
      observations <- auk_unique(observations)
      ## this function will add variable "checklist_id"

      observations <- convert_cols(observations)

      # save to file
      ## write the filtered sampling data
      if (method == "vroom")
        vroom::vroom_write(observations, f_obs_out)
      if (method == "data.table")
        data.table::fwrite(observations, f_obs_out)
    }

    # create output file as a list
    ebird_filtered <- list("observations" = as_tibble(observations),
                           "sampling" = as_tibble(sampling))

    # rm(observations, sampling)

    return(ebird_filtered)

  } # END FUNCTION
