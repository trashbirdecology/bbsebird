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
           max.effort.km = NULL,
           max.effort.mins = NULL,
           max.num.observers = 10,
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
               # "group_identifier",
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
           f_samp_out  = paste0(dir.ebird.out, 'ebird_samp_filtered.txt')
           ) {

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
      sampling <- vroom::vroom(f_samp_out, col_types = cols_samp)
    } else{
      if (!exists("sampling"))
        cat("Importing the eBird sampling events data.\nThis may take a minute.")
      if (method == "vroom") {
        sampling <- vroom::vroom(f_samp_in, col_types = cols_samp)
      }
      if (method == "data.table") {
        sampling <- data.table::fread(f_samp_in) %>% as_tibble()
      }
    ### The base sampling df is large so this script tries to prioritize commands that
    ### will remove the most data to the least.
    ### Try to keep the filtering/subsetting in that order.

      ##force colnames to lower and replace spaces with underscore (_)
      colnames(sampling) <-
        str_replace_all(tolower(colnames(sampling)),
                        pattern = " ",
                        replacement = "_")


      # trying to keep in order of largest cut to smaller to help with memory issues.
      cat("Filtering sampling events. This takes a minute.\n")
      sampling <- sampling[names(sampling) %in% cols.keep]#keep only useful columns
      if(complete.only) sampling <- sampling %>%
        filter(all_species_reported %in% c("TRUE", "True", 1))
      if(!is.null(countries)) sampling <- sampling %>%
        filter(country %in% countries)
      if(!is.null(states)) sampling <- sampling %>%
        filter(state %in% states)
      if(!is.null(protocol)) sampling <- sampling %>%
        filter(protocol_type %in% protocol)
      if(!is.null(max.num.observers)) sampling <- sampling %>%
        filter(number_observers<=max.num.observers)
      if(!is.null(max.effort.km)) sampling <- sampling %>%
        filter(effort_distance_km<=max.effort.km)
      if(!is.null(max.effort.mins)) sampling <- sampling %>%
        filter(duration_minutes<=max.effort.mins)


      # create year and then filter by year
      sampling <- sampling %>%
        mutate(year = lubridate::year(observation_date))
      if(!is.null(years)) sampling <- sampling %>%
        filter(year %in% years)

      # attempt to remove BBS observations if specified
      ### THIS IS A BIG ASSUMPTION SO WILL NEED TO REVISIT EVENTUALLY!!!
      if (remove.bbs.obs)
        sampling <- sampling %>%
        filter(protocol_type != "Stationary" &
                 duration_minutes != 3)



      # remove duplicate checklists for same birding party.
      # for good measure..
      cat("Taking out the garbage...\n")
      gc()
      cat("Running auk_unique() on checklists\n")
      sampling <- auk_unique(sampling, checklists_only = TRUE)

      # ensure consistency in col types
      sampling <- convert_cols(sampling)


      ## read the filtered sampling data
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

      # trying to keep in order of largest cut to smaller to help with memory issues.
      ## keep only useful columns
      observations <-
        observations[names(observations) %in% cols.keep]
      ## begin the filtering by params
      if(!is.null(countries)) observations <- observations %>%
        filter(country %in% countries)
      if(!is.null(states)) observations <- observations %>%
        filter(state %in% states)
      if(complete.only) observations <- observations %>%
        filter(all_species_reported %in% c("TRUE", "True", 1))
      if(!is.null(species)) observations <- observations %>%
        filter(common_name %in% species)
      # create and then filter by year/date
      observations <- observations %>%
        mutate(year = lubridate::year(observation_date))
      if(!is.null(years)) observations <- observations %>%
        filter(year %in% years)
      if(!is.null(protocol)) observations <- observations %>%
        filter(protocol_type %in% protocol)
      if(!is.null(max.num.observers)) observations <- observations %>%
        filter(number_observers<=max.num.observers)
      if(!is.null(max.effort.km)) observations <- observations %>%
        filter(effort_distance_km<=max.effort.km)
      if(!is.null(max.effort.mins)) observations <- observations %>%
        filter(duration_minutes<=max.effort.mins)
      if (remove.bbs.obs) {
        observations <- observations %>%
          filter(protocol_type != "Stationary" &
                   duration_minutes != 3)
      }
      ## since we dropped group id, there may be duplicates to remove
      observations <-
        observations %>% distinct(observer_id, common_name, observation_count, observation_date, .keep_all=TRUE)


      cat("throwing out the trash --- one sec..")
      gc()
      # collapse duplicate checklists into one, taking the max number identified by the group during an event
      ### This takes FOREVER....not sure why. going to do this manually...
      # cat("Running auk::auk_unique(). This takes a few minutes for some reason")
      # observations <- auk_unique(observations)
      ## this function will add variable "checklist_id"

    #####NEED TO CHEcK ABOUT X....does this mean they didn't count the number, or does it mean NO DATA?!!?!?
      observations <- observations %>% mutate(
        # convert X to NA
        observation_count = if_else(observation_count == "X",
                                    NA_character_, observation_count), # must use NA_character_ instead of NA!
        # effort_distance_km to 0 for non-travelling counts
        effort_distance_km = if_else(protocol_type %in% c("stationary", "Stationary", "STATIONARY"),
                                     0, effort_distance_km)
      )

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
    cat("Output of `filter_ebird_data()` may contain duplicate observations where multiple observers exist.")
    # rm(observations, sampling)

    return(ebird_filtered)

  } # END FUNCTION
