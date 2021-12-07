#' @title Create and Write or Load In the Filtered eBird Data
#' @description Filter the eBird data and sampling events using R package AUK.
#' @param fns.ebird File paths for the EBD and SamplingEvents data to import. Character vector of filenames for original files.
#' @param overwrite Logical. If true will overwrite existing filtered data objects in project directory.
#' @param dir.ebird.out Location of where to save and find the filtered/subsetted data.
#' If not specified will default to subdir in project directory.
filter_ebird_data <-
  function(fns.ebird,
           dir.ebird.out,
           countries=NULL,
           states=NULL,
           complete.only = TRUE,
           protocol=c("Traveling", "Stationary"),
           species="Double-crested Cormorant",
           overwrite = FALSE,
           remove.bbs.obs=TRUE,
           years=NULL,
           method="data.table" # how to read in sampling data ... vroom often crashes
           ) {
    f_samp_in  <- fns.ebird[str_detect(fns.ebird, "sampling_rel")]
    f_obs_in <- setdiff(fns.ebird, f_samp_in)
    if (!length(f_obs_in) > 0)
      stop(paste0("No ebd file identified. "))
    if (!length(f_samp_in) > 0)
      stop(paste0("No sampling file identified. "))


    # Make filenames for output (not using RDS for sampling data because too large...)
    f_obs_out <- paste0(dir.ebird.out, 'ebird_obs_filtered.rds')
    f_samp_out  <- paste0(dir.ebird.out, 'ebird_samp_filtered.txt')

    #specify teh columns that we will want to keep
    cols.keep <-
      c(
        "all_species reported",
        "country",
        "country_code",
        "county",
        "county_code",
        "duration_minutes",
        "effort_area ha",
        "effort_distance km",
        "group_identifier",
        "latitude",
        "longitude",
        "number_observers",
        "observation_date",
        "observer_id",
        "protocol_code",
        "protocol_type",
        "sampling_event identifier",
        "state",
        "state_code",
        "time_observations started"
      )

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
      if(method=="vroom"){ sampling <- vroom::vroom(f_samp_in, col_types=cols_samp)}
      if(method=="data.table"){ sampling <- data.table::fread(f_samp_in) %>% as_tibble()}

      ##force colnames to lower and replace spaces with underscore (_)
      colnames(sampling) <- str_replace(tolower(colnames(sampling),
                                                pattern=" ",
                                                replacement="_"))
      ## keep only useful columns
      sampling <- sampling[names(sampling) %in% cols.keep]
      cat("Filtering sampling events. This takes a minute.")
      # trying to keep in order of largest cut to smaller to help with memory issues.
      sampling <- sampling %>%  #breaking this up to try to help wtih mem issues
        filter(if(!is.null(countries)) country %in% countries) %>%
        filter(if(!is.null(states)) state %in% states)
      sampling <- sampling %>%
        filter(if(complete.only) `all species reported` %in% c("TRUE","True", 1))
      sampling <- sampling %>%
        filter(if(!is.null(protocol))`protocol type` %in% protocol)
      gc()

      # extract events with >1 checklist (i.e. group birding;)
      ### this doesn't make sense to me, because sampling event
      # identifiers are all unique and not tied to a group identifier
      duplicates <- sampling %>%
        filter(!is.na(`group identifier`),
               `group identifier` != "") %>%
        distinct(`group identifier`, `observation date`, `state`, `country`)


test=auk_unique(sampling)

      # remove BBS observations if specified
      if(remove.bbs.obs){
        sampling <- sampling %>%
        filter(`protocol type` != "Stationary" &
               `duration minutes` != 3)
        }

      ## write the filtered sampling data
      vroom::vroom_write(sampling, f_samp_out)

      ## remove the files that vroom creates in R session's temp directory just in case
      fs::file_delete(list.files(tempdir(), full.names=TRUE))
      }

    ## Read in / filter observations data frame
    if (file.exists(f_obs_out)| overwrite) {
      observations <- readRDS(f_obs_out)
    } else{
      observations <- vroom::vroom(f_obs_in)
      observations <- observations %>%
        filter(if(!is.null(species))`common name` %in% species) %>%
        filter(if(!is.null(countries)) country %in% countries) %>%
        filter(if(!is.null(states))state %in% region) %>%
        filter(if(!is.null(protocol))`protocol type` %in% protocol)
      observations <- observations %>%
        filter(if(complete.only) `sampling event identifier` %in% sampling$`sampling event identifier`) #keep only the complete cases
      saveRDS(observations, f_obs_out)
    }

  ebird_filtered <- list("observations" = observations,
                         "sampling" = sampling)

  rm(observations, sampling)

  return(ebird_filtered)

  } # END FUNCTION
