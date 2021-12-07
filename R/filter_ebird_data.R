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
           remove.bbs.obs=TRUE
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
        `SAMPLING EVENT IDENTIFIER` = col_character(),
        `PROTOCOL TYPE` = col_character(),
        `PROTOCOL CODE` = col_character(),
        `PROJECT CODE` = col_character(),
        `DURATION MINUTES` = col_double(),
        `EFFORT DISTANCE KM` = col_double(),
        `EFFORT AREA HA` = col_double(),
        `NUMBER OBSERVERS` = col_double(),
        `ALL SPECIES REPORTED` = col_double(),
        `GROUP IDENTIFIER` = col_character(),
        `TRIP COMMENTS` = col_character()
        )

    ## Read in / filter sampling data frame
    if (file.exists(f_samp_out) & !overwrite) {
      sampling <-
        vroom::vroom(f_samp_out)
    } else{
      if (!exists("sampling"))
      cat("Importing the eBird sampling events data.
            This may take a minute.")
      # sampling <- data.table::fread(f_samp_in)
      sampling <- vroom::vroom(f_samp_in, col_types = cols_samp)
      gc()
      cat("Filtering sampling events. This takes a minute.")
      sampling <- sampling %>%
        filter(if(complete.only) `ALL SPECIES REPORTED` %in% c("TRUE","True", 1)) %>%
        filter(if(!is.null(protocol))`PROTOCOL TYPE` %in% protocol)
      gc()
      sampling <- sampling %>%  #breaking this up to try to help wtih mem issues
        filter(if(!is.null(countries)) country %in% countries) %>%
        filter(if(!is.null(states)) STATE %in% region)
      gc()
      # remove BBS observations if specified
      if(remove.bbs.obs){
        sampling <- sampling %>%
        filter(`PROTOCOL TYPE` != "Stationary" &
               `DURATION MINUTES` != 3)
        }

      ## write the filtered sampling data
      vroom::vroom_write(sampling, f_samp_out)
      }

    ## Read in / filter observations data frame
    if (file.exists(f_obs_out)| overwrite) {
      observations <- readRDS(f_obs_out)
    } else{
      observations <- vroom::vroom(f_obs_in)
      observations <- observations %>%
        filter(if(!is.null(species))`COMMON NAME` %in% species) %>%
        filter(if(!is.null(countries)) COUNTRY %in% countries) %>%
        filter(if(!is.null(states))STATE %in% region) %>%
        filter(if(!is.null(protocol))`PROTOCOL TYPE` %in% protocol)
      observations <- observations %>%
        filter(if(complete.only) `SAMPLING EVENT IDENTIFIER` %in% sampling$`SAMPLING EVENT IDENTIFIER`) #keep only the complete cases
      saveRDS(observations, f_obs_out)
    }
  gc()

  ebird_filtered <- list("observations"=observations,
                         "sampling"=sampling)

  rm(observations, sampling)

  return(ebird_filtered)

  } # END FUNCTION
