#' @title Create and Write or Load In the Filtered eBird Data
#'
#' @description Filter the eBird data and sampling events using R package AUK.
#'
#' @param fns.ebird  File paths for the EBD and SamplingEvents data to import. Character vector of filenames for original files.
#' @param overwrite  logical If true will overwrite existing filtered data objects in directory 'dir.ebird.out'
#' @param dir.ebird.out Location of where to save and/or find the filtered/subsetted eBird data.
#' @param f_obs_out  Filename for saving the filtered eBird observations data
#' @param f_samp_out Filename for saving the filtered eBird sampling data
#' @param countries Character vector of country abbreviations for filtering ebird data against.
#' @param states Character vector of state/province abbreviations for filtering ebird data against.
#' @param complete.only logical if TRUE will remove incomplete checklists
#' @param protocol which eBird protocol to include
#' @param species species for filtering eBird data
#' @param remove.bbs.obs logical if TRUE will use crude and untested method for removing BBS data from eBird data (see function for details)
#' @param years years to include in data
#' @param max.effort.km maximum distance (kilometers) of birding events to include
#' @param max.effort.mins maximum number of minutes of birding events to include
#' @importFrom dplyr filter select mutate bind_rows distinct mutate if_else as_tibble
#' @importFrom auk auk_unique
#' @importFrom stringr str_replace_all str_detect
#' @importFrom parallel detectCores
#' @importFrom vroom vroom
#' @importFrom bit chunk
#' @importFrom readr col_character col_time col_date col_double
#' @importFrom lubridate year
#' @importFrom data.table fread fwrite
#' @param max.num.observers maximum number of observers declared at a birding event to include
#' @export munge_ebird_data
munge_ebird_data <-
  function(fns.ebird,
           dir.ebird.out,
           overwrite = FALSE,
           countries = NULL,
           states = NULL,
           complete.only = TRUE,
           protocol = c("Traveling", "Stationary"),
           species = "Double-crested Cormorant",
           remove.bbs.obs = TRUE,
           years = NULL,
           max.effort.km = NULL,
           max.effort.mins = NULL,
           max.num.observers = 10,
           # how to read in sampling data ... vroom often crashes
           f_obs_out = paste0(dir.ebird.out, 'ebird_obs_filtered.txt'),
           f_samp_out  = paste0(dir.ebird.out, 'ebird_samp_filtered.txt')
  ) {

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
      )

    # CHECK / IMPORT FILTERED DATA --------------------------------------------
    # First, check the filepaths for the munged and filtered data. If overwrite is FALSE and exists, will just import that and clal it a day.
    ind <- fns.ebird[stringr::str_detect(pattern="ebird_filtered.rds", fns.ebird)]
    if(length(ind)>=1){
      # if overwriting the data, remove this from filelist to be safe..
      if(overwrite){fns.ebird <- setdiff(fns.ebird, ind)}
      #import data and exist functio nif no overwrite specified
      if(!overwrite){
        cat("File ",ind, "exists. Importing. If you need to re-create the ebird data, specify overwrite=TRUE in `munge_ebird_data()`.\n")
        output <- readRDS(ind)
        return(output)
      }
    }

    # CHECK ARGS AND FILES ----------------------------------------------------
    # must have at least two files in here
    if(length(fns.ebird) < 2)stop("check arg `fns.ebird`: must have at least two files (one for sampling events, one for observations.")
    if(!any(file.exists(fns.ebird))) stop("One or more files specified in `fns.ebird` does not exist")

    ## filenames to import
    f_samp_in <- fns.ebird[stringr::str_detect(fns.ebird, "sampling_rel")]
    f_obs_in  <- setdiff(fns.ebird, f_samp_in)
    if (!length(f_samp_in) > 0)
      stop(paste0("No sampling file identified. \n"))
    if (!length(f_obs_in) > 0)
      stop(paste0("No ebd file identified.\n"))

    # MEMORY CHECKS  -----------------------------------------------------------
    # warn about potential memory issues
    if (parallel::detectCores() <= 4 |
        memory.limit() < 25000)
      warning(
        "You probably don't have enough RAM and/or CPU to munge the eBird data. Don't blame me if your machine crashes. \n\nIf `filter_ebird_data` takes longer than 20 minutes and your spatial extent <~5 u.s. states, something is probably wrong.\n\n"
      )
    # MUNGE ARGS --------------------------------------------------------------
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

    ## We need to borrow region codes from bbsassistant again to match country names in ebird
    rc <- bbsAssistant::region_codes
    ## if countries index is specified, filter by that
    if(!is.null(countries)){rc <- rc[tolower(rc$iso_a2) %in% tolower(countries),]}
    ## if states index is specified, filter by that
    if(!is.null(states)){
      s=paste(gsub(x=tolower(states), pattern="-", replacement=""), collapse="|")
      rc.i=gsub(x=tolower(rc$iso_3166_2), pattern="-", replacement="")
      rc <- rc[which(grepl(s, rc.i)),]
      rm(s, rc.i)
    }
    if(nrow(rc)<1)stop("Please ensure arguments `states` and `countries` contain elements witin bbsAssistant::region_codes. See ?filter_ebird_data for further instruction.")

    # SAMPLING EVENTS DATASETS ------------------------------------------------------------
    ## Read in / filter sampling data frame
    ### IMPORTANT: importing the sampling.txt.gz file for nov2021 takes about
    #### 2.2 minutes with vroom::vroom and XX minutes with data.table::fread!!!!
    if(file.exists(f_samp_out) & !overwrite) {
      cat("Overwrite is FALSE and filtered sampling events data already exists. Importing from file (",
          f_samp_out,")\n")
      sampling <- vroom::vroom(f_samp_out, col_types = cols_samp)
    } else{
      cat("Importing the sampling events dataset. This file should take ~2 mins to import. Ignore parsing errors.\n\n")
      sampling <- vroom::vroom(f_samp_in, col_types = cols_samp)

      ### The sampling df is large so this script tries to prioritize commands that
      ### will remove the most data to the least.
      ### Try to keep the filtering/subsetting in that order..need to run tests to ensure most efficient order...

      # trying to keep in order of largest cut to smaller to help with memory issues.
      cat("Filtering the sampling events. Takes a few more minutes, depending on size of desired data (i.e., spatial, temporal extents and number of species..\n")      ## force column names to lower and replace spaces with underscore (_) for my sanity
      colnames(sampling) <-
        stringr::str_replace_all(tolower(colnames(sampling)),
                                 pattern = " ",
                                 replacement = "_")
      ## remove useless (to us) columns to help with memory issues.
      sampling <- sampling[names(sampling) %in% cols.keep]#keep only useful columns
      ## do some serious filtering. there's def a cleaner way to write this up but is low priority and would need benefit from some benchmarking
      ## since we specific rc and rc should always be populated regardless of whether staties or countries is specified, the filter on ISO_3166-2 is FINE for both state and country fitler
      if(!is.null(countries) | !is.null(states)){sampling <- sampling %>%
        dplyr::filter(state_code %in% rc$iso_3166_2)}
      if(complete.only) sampling <- sampling %>%
        dplyr::filter(all_species_reported %in% c("TRUE", "True", 1))
      if(!is.null(protocol)) sampling <- sampling %>%
        dplyr::filter(protocol_type %in% protocol)
      if(!is.null(max.num.observers)) sampling <- sampling %>%
        dplyr::filter(number_observers<=max.num.observers)
      if(!is.null(max.effort.km)) sampling <- sampling %>%
        dplyr::filter(effort_distance_km<=max.effort.km)
      if(!is.null(max.effort.mins)) sampling <- sampling %>%
        dplyr::filter(duration_minutes<=max.effort.mins)

      # munge the year variable to ensure proper filtering, then filter
      sampling <- sampling %>%
        dplyr::mutate(year = lubridate::year(observation_date))
      if(!all(years %in% unique(sampling$year)))warning("Note that not all years in arg `years` were found in the sampling events dataset.\n")
      if(!is.null(years)) sampling <- sampling %>%
        dplyr::filter(year %in% years)

      # ~attempt to~ remove BBS observations if specified
      ### THIS IS A BIG ASSUMPTION SO WILL NEED TO REVISIT EVENTUALLY!!!
      ### in fact, i've got some checklists i need to cross-check against the BBS obserfvations data to see if this correctly removes them all.....
      if (remove.bbs.obs){
        sampling <- sampling %>%
          dplyr::filter(protocol_type != "Stationary" &
                          duration_minutes != 3)
      }

      # for good measure..
      cat("Taking out the garbage because this data can be massive.....\n")
      gc()

      # remove duplicate checklists for same birding party.
      cat("Running an alternative to `auk::auk_unique()` on checklists. This takes quite a few minutes for more than a few states/provinces. \n")
      ## Sometimes when I run auk_unique() on the full (filtered) sampling df I get bluescreen on DOI machine. So running this command in chunks just to be safe.
      ## I am running it by date because the data must be in the same day to be considered to be of the same checklist.
      dates <- unique(sampling$observation_date)
      date.chunk <- names(bit::chunk(dates, by=100))
      for(i in seq_along(date.chunk)){
        # print(i)
        if(i==1) mylist <- list()
        d.ind <- as.integer(unlist(date.chunk[i] %>% stringr::str_split(pattern=":")))
        d.min <- dates[min(d.ind)]
        d.max <- dates[max(d.ind)]
        tempdat <- sampling[sampling$observation_date >= d.min & sampling$observation_date <= d.max, ]
        mylist[[i]] <- auk::auk_unique(tempdat, checklists_only = TRUE)
        rm(tempdat, d.min, d.ind, d.max)
      }
      ## unlist the list
      sampling <- dplyr::bind_rows(mylist)
      # ensure consistency in col types
      sampling <- convert_cols(sampling)

      ## save the filtered sampling data (fwrite is superior to vrooom for writing (and reading usually))
      cat("Writing the filtered sampling data to: ", f_samp_out, "...\n")
      data.table::fwrite(x=sampling,file=f_samp_out)
      gc()
    } # end sampling events data import/munging

    # OBSERVATIONS DATA SETS -------------------------------------------------
    # Read in or create, and filter observations data frame
    ##fix the col types for observations df
    cols_obs <- cols_samp[!names(cols_samp) %in% c("country", "sampling event identifier","protocol type", "duration minutes")]
    if (file.exists(f_obs_out) & !overwrite) {
      ### i can't figure out how to silence vroom import warning re: parsing colnames
      #### (which is a result of not all names in cols_samp are in this file)
      ### consequently, I would like to remove the nusance names frm cols_samp here..
      cat("Overwrite is FALSE and filtered observations data already exists. Importing from file (",
          f_obs_out,")\n")

      # observations <- vroom::vroom(f_obs_out, col_types = cols_obs)
      observations <- data.table::fread(f_obs_out) # no huge difference when files are small.
    } else{
      #    browser()
      cat("Loading the original eBird observations. Please ignore parsing warnings.\n\n")
      # use vroom because its more elegant when reading multiple files at once
      observations <- vroom::vroom(f_obs_in, col_types = cols_obs)

      ##force colnames to lower and replace spaces with underscore (_)
      cat("Munging observations data\n\n")
      colnames(observations) <-
        stringr::str_replace_all(tolower(colnames(observations)),
                                 pattern = " ",
                                 replacement = "_")
      # trying to keep in order of largest cut to smaller to help with memory issues.
      ## keep only useful columns
      observations <-
        observations[names(observations) %in% cols.keep]

      ## begin the filtering by params
      ## since we specific rc and rc should always be populated regardless of whether staties or countries is specified, the filter on ISO_3166-2 is FINE for both state and country fitler
      if(!is.null(countries) | !is.null(states)){observations <- observations %>%
        dplyr::filter(state_code %in% rc$iso_3166_2)}
      if(complete.only) observations <- observations %>%
        dplyr::filter(all_species_reported %in% c("TRUE", "True", 1))

      species = tolower(species)
      if(!is.null(species)) observations <- observations %>%
        dplyr::filter(tolower(common_name) %in% species)
      stopifnot("no species in arg `species` found in observations data"=nrow(observations)>0)
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
      cat("Writing the filtered observations data to:", f_obs_out,"...\n\n")
      data.table::fwrite(observations, f_obs_out)
    }

    # COMBINE -----------------------------------------------------------------
    # create the list output file as a list of sam,pling events and observations
    ebird_filtered <- list("observations" = dplyr::as_tibble(observations),
                           "sampling" = dplyr::as_tibble(sampling))

    # ZERO-FILL DATA ----------------------------------------------------------
    ebird_filtered <- zerofill_ebird(list=ebird_filtered)

    # Final munging of columns  -----------------------------------------------
    ## munge column names
    ebird_filtered <- clean_ebird_colnames(df=ebird_filtered)

    # SAVE TO FILE ------------------------------------------------------------
    f.out =  paste0(dir.ebird.out, "ebird_filtered.rds")
    cat("Writing the filtered and zero-filled eBird data to:", f.out, "...\n\n")
    saveRDS(ebird_filtered, f.out)


    ###
    message("IGNORE PARSING ERRORS")
    # RETURNED OBJECT ---------------------------------------------------------
    return(ebird_filtered)

  } # END FUNCTION
