#' @title Create and Write or Load In the Filtered eBird Data
#' @description Filter the eBird data and sampling events using R package AUK.
#' @param fns.ebird File paths for the EBD and SamplingEvents data to import. Character vector of filenames for original files.
#' @param overwrite Logical. If true will overwrite existing filtered data objects in directory `dir.ebird.out`
#' @param dir.ebird.out Location of where to save and/or find the filtered/subsetted eBird data.
#' @param f_obs_out Filename for saving the filtered eBird observations data
#' @param f_samp_out Filename for saving the filtered eBird sampling data
#' @export
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
           f_samp_out  = paste0(dir.ebird.out, 'ebird_samp_filtered.txt')
           ) {

    f_samp_in  <- fns.ebird[stringr::str_detect(fns.ebird, "sampling_rel")]
    f_obs_in <- setdiff(fns.ebird, f_samp_in)
    if (!length(f_obs_in) > 0)
      stop(paste0("No ebd file identified. "))
    if (!length(f_samp_in) > 0)
      stop(paste0("No sampling file identified. "))


    #specifying the column types helps with vroom::vroom(f_samp_in), which takes a couple of minutes...
    cols_samp <- list(
      `LAST EDITED DATE` = readr::col_datetime(),
      country = readr::col_character(),
      `COUNTRY CODE` = readr::col_character(),
      STATE = readr::col_character(),
      `STATE CODE` = readr::col_character(),
      COUNTY = readr::col_character(),
      `COUNTY CODE` = readr::col_character(),
      `IBA CODE` = readr::col_character(),
      `BCR CODE` = readr::col_double(),
      `USFWS CODE` = readr::col_character(),
      `ATLAS BLOCK` = readr::col_character(),
      LOCALITY = readr::col_character(),
      `LOCALITY ID` = readr::col_character(),
      `LOCALITY TYPE` = readr::col_character(),
      LATITUDE = readr::col_double(),
      LONGITUDE = readr::col_double(),
      `OBSERVATION DATE` = readr::col_date(),
      `TIME OBSERVATIONS STARTED` = readr::col_time(),
      `OBSERVER ID` = readr::col_character(),
      `sampling event identifier` = readr::col_character(),
      `protocol type` = readr::col_character(),
      `PROTOCOL CODE` = readr::col_character(),
      `PROJECT CODE` = readr::col_character(),
      `duration minutes` = readr::col_double(),
      `EFFORT DISTANCE KM` = readr::col_double(),
      `EFFORT AREA HA` = readr::col_double(),
      `NUMBER OBSERVERS` = readr::col_double(),
      `ALL SPECIES REPORTED` = readr::col_double(),
      `GROUP IDENTIFIER` = readr::col_character(),
      `TRIP COMMENTS` = readr::col_character()
    )

    ## Read in / filter sampling data frame
    if (file.exists(f_samp_out) & !overwrite) {
      cat("Importing the filtered eBird data. This takes 1-3 minutes, depending on size of object.")
      sampling <- vroom::vroom(f_samp_out, col_types = cols_samp)
    } else{
      if (!exists("sampling")) # this check is used for development purposes. should be removed prior to publish
        cat("Importing the eBird sampling events data.\nThis may take 3-5 minutes...\n\n")
      if (method == "vroom") {
        sampling <- vroom::vroom(f_samp_in, col_types = cols_samp)
      }
      if (method == "data.table") {
        sampling <- data.table::fread(f_samp_in) %>% as_tibble()
      }
    ### The base sampling df is large so this script tries to prioritize commands that
    ### will remove the most data to the least.
    ### Try to keep the filtering/subsetting in that order.

      # trying to keep in order of largest cut to smaller to help with memory issues.
      cat("Now filtering the sampling events. This takes a minute or two. \n\n")
      ## force column names to lower and replace spaces with underscore (_) for my sanity
      colnames(sampling) <-
        stringr::str_replace_all(tolower(colnames(sampling)),
                        pattern = " ",
                        replacement = "_")
      ## remove useless (to us) columns to help with memory issues.
      sampling <- sampling[names(sampling) %in% cols.keep]#keep only useful columns
      if(complete.only) sampling <- sampling %>%
        dplyr::filter(all_species_reported %in% c("TRUE", "True", 1))
      if(!is.null(countries)) sampling <- sampling %>%
        dplyr::filter(country %in% countries)
      if(!is.null(states)) sampling <- sampling %>%
        dplyr::filter(state %in% states)
      if(!is.null(protocol)) sampling <- sampling %>%
        dplyr::filter(protocol_type %in% protocol)
      if(!is.null(max.num.observers)) sampling <- sampling %>%
        dplyr::filter(number_observers<=max.num.observers)
      if(!is.null(max.effort.km)) sampling <- sampling %>%
        dplyr::filter(effort_distance_km<=max.effort.km)
      if(!is.null(max.effort.mins)) sampling <- sampling %>%
        dplyr::filter(duration_minutes<=max.effort.mins)

      # create year and then filter by year
      sampling <- sampling %>%
        dplyr::mutate(year = lubridate::year(observation_date))
      if(!is.null(years)) sampling <- sampling %>%
        dplyr::filter(year %in% years)

      # ~attempt to~ remove BBS observations if specified
      ### THIS IS A BIG ASSUMPTION SO WILL NEED TO REVISIT EVENTUALLY!!!
      ### in fact, i've got some checklists i need to cross-check against the BBS obserfvations data to see if this correctly removes them all.....
      if (remove.bbs.obs)
        sampling <- sampling %>%
        dplyr::filter(protocol_type != "Stationary" &
                 duration_minutes != 3)

      # for good measure..
      cat("Taking out the garbage because this data is massive.....\n\n")
      gc()

      # remove duplicate checklists for same birding party.
      cat("Running `auk::auk_unique()` on checklists\n\n")
      sampling <- auk::auk_unique(sampling, checklists_only = TRUE)
      # names(sampling)

      # ensure consistency in col types
      sampling <- convert_cols(sampling)

      ## save the filtered sampling data
      cat("Writing the filtered sampling data to file at location:", f_samp_out,"...\n\n")
      if (method == "vroom")
        vroom::vroom_write(sampling, f_samp_out)
      if (method == "data.table")
        data.table::fwrite(sampling, f_samp_out)

      ## remove the files that vroom creates in R session's temp directory just in case
      try(fs::file_delete(list.files(tempdir(), full.names = TRUE)), silent =
            TRUE)
  }

    ## Read in / filter observations data frame
    cat("Filtering the eBird observations.\n\n")
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
        stringr::str_replace_all(tolower(colnames(observations)),
                        pattern = " ",
                        replacement = "_")

      # trying to keep in order of largest cut to smaller to help with memory issues.
      ## keep only useful columns
      observations <-
        observations[names(observations) %in% cols.keep]
      ## begin the filtering by params
      if(!is.null(countries)) observations <- observations %>%
        dplyr::filter(country %in% countries)
      if(!is.null(states)) observations <- observations %>%
        dplyr::filter(state %in% states)
      if(complete.only) observations <- observations %>%
        dplyr::filter(all_species_reported %in% c("TRUE", "True", 1))
      if(!is.null(species)) observations <- observations %>%
        dplyr::filter(common_name %in% species)
      # create and then filter by year/date
      observations <- observations %>%
        dplyr::mutate(year = lubridate::year(observation_date))
      if(!is.null(years)) observations <- observations %>%
        dplyr::filter(year %in% years)
      if(!is.null(protocol)) observations <- observations %>%
        dplyr::filter(protocol_type %in% protocol)
      if(!is.null(max.num.observers)) observations <- observations %>%
        dplyr::filter(number_observers<=max.num.observers)
      if(!is.null(max.effort.km)) observations <- observations %>%
        dplyr::filter(effort_distance_km<=max.effort.km)
      if(!is.null(max.effort.mins)) observations <- observations %>%
        dplyr::filter(duration_minutes<=max.effort.mins)
      if (remove.bbs.obs) {
        observations <- observations %>%
          dplyr::filter(protocol_type != "Stationary" &
                   duration_minutes != 3)
      }
      ## since we dropped group id, there may be duplicates to remove
      observations <-
        observations %>% dplyr::distinct(observer_id, common_name, observation_count, observation_date, .keep_all=TRUE)

      # collapse duplicate checklists into one, taking the max number identified by the group during an event
      ### This takes FOREVER....not sure why. going to do this manually...
      # cat("Running auk::auk_unique(). This takes a few minutes for some reason")
      # observations <- auk_unique(observations)
      ## this function will add variable "checklist_id"

    #####NEED TO CHEcK ABOUT X....does this mean they didn't count the number, or does it mean NO DATA?!!?!?
      observations <- observations %>% dplyr::mutate(
        # convert X to NA
        observation_count = dplyr::if_else(observation_count == "X",
                                    NA_character_, observation_count), # must use NA_character_ instead of NA!
        # effort_distance_km to 0 for non-travelling counts
        effort_distance_km = dplyr::if_else(protocol_type %in% c("stationary", "Stationary", "STATIONARY"),
                                     0, effort_distance_km)
      )
      observations <- convert_cols(observations)

      # save to file
      ## write the filtered sampling data
      cat("Writing the filtered observations data to file at location:", f_obs_out,"...\n\n")
      if (method == "vroom")
        vroom::vroom_write(observations, f_obs_out)
      if (method == "data.table")
        data.table::fwrite(observations, f_obs_out)
    }

    # create output file as a list
    ebird_filtered <- list("observations" = dplyr::as_tibble(observations),
                           "sampling" = dplyr::as_tibble(sampling))
    cat("Output of `filter_ebird_data()` may contain duplicate observations where multiple observers exist.")
    # rm(observations, sampling)

    return(ebird_filtered)

  } # END FUNCTION
