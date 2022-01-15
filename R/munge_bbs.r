## ID LOVE to be able to throw all the BBS filtering into a single function within which I use
## across(fun_x(list elemnt )) and use lapply to apply the function to the entire list. but for now i am just gonna do it ,"by hand"
# if("citation" %in% names(list)) bbs$citation<-bbs$citation %>% as.data.frame() # this is required for quickly scanning and filtering lists, sorry
#' @title Subset BBS observations data by one or more species of interest.
#' @description ...
#' @param years value(s) of year(s) by which to subset the data. If not specified will return all data in the dataset.
#' @param list A list with element "species_list", obtained from running bbsAssistant::get_bbs_data()...
#' @param spp A vector of one or more species (using English Common Name) to subset the data by. Capitalization ignored.
#' @param zero.fill If TRUE and a single species is provided in 'spp', this function will output list$observations with zero-filled data.
#' @param active.only Logical. If TRUE keep only active routes. Discontinued routes will be discarded.
#' @export
munge_bbs <-
  function(list,
           states = NULL,
           spp = "Double-crested Cormorant",
           zero.fill = TRUE,
           active.only = FALSE,
           years = 1966:lubridate::year(Sys.Date()),
           keep.stop.level.data = TRUE,
           QualityCurrentID = 1,
           countrynums.keep=c(124, 840)) {
 # First, subset by political/geo
    # data(region_codes) # region codes from bbsAssistant package.
    region_codes.subset <- region_codes %>%
      filter(CountryNum %in% countrynums.keep)
    # have to split up the state num and country num process b/c of Mexico's character issues.
    if(!is.null(states)) region_codes.subset <-region_codes.subset %>% filter(tolower(State) %in% tolower(states))
    # Remove the citation object in bbs list
    if ("citation" %in% names(list))
      list$citation <- NULL

    # first, remove stop - level data if not needed  to help with mem issues
      # create a column index for all data not needed at stop-level

      cols.stop <- c(paste0("Stop", c(1:50))) # stop-level counts
      cols.noise <- c(paste0("Noise", c(1:50))) # stop-level noise covar
      cols.car <- c(paste0("Car", c(1:50))) # stop-level car covar
      # Before dropping stop data, create a column containing total and the average
      ## of detection covariates per route/yr
      list$observations <-
        list$observations %>%
        mutate(RouteTotal=rowSums(across(cols.stop))) %>%
        # filter out years
        filter(Year >= min(years) & Year <= max(years))


      list$vehicle_data <- list$vehicle_data %>%
        mutate(CarMean= round(rowSums(across(all_of(cols.car)))/50)) # rounded
      list$vehicle_data <- list$vehicle_data %>%
        mutate(NoiseMean= round(rowSums(across(all_of(cols.noise)))/50)) # rounded
    cols=c(cols.stop, cols.noise, cols.car)
    if (!keep.stop.level.data) {
      for (i in seq_along(list)) {
        list[[i]] <- list[[i]][,!(names(list[[i]]) %in% cols)]
      }
    }


    ## filter out by country and state!
    list$observations <- list$observations %>%
      filter(CountryNum %in% region_codes.subset$CountryNum &
               StateNum %in% region_codes.subset$StateNum
      )
    list$routes <- list$routes %>%
      filter(CountryNum %in% region_codes.subset$CountryNum &
               StateNum %in% region_codes.subset$StateNum
      )

    # grab the unique AOU codes associated with species in "spp"
    list$species_list <- list$species_list %>%
      mutate(across(starts_with("English_Common_Name"), tolower)) %>%
      filter(across(any_of("English_Common_Name"), ~ .x %in% tolower(spp)))

    # use the aou for target species to zero-fill the data
    myspp.obs <- list$observations %>%
      filter(as.double(AOU) %in% as.double(unique(list$species_list$AOU))) # ensure the variables are of same type


    # Zero-fill data
    ## ensure only a single species is provided when zero.fill=TRUE
    spp <- spp[tolower(spp) %in% list$species_list]
    if (zero.fill &
        length(spp) > 1) {
      stop(
        "When zero.fill=TRUE, only a single species should be provided in interest.species. Using first species in the list. "
      )
    }
    if (zero.fill) {
      # unused <- setdiff(list$observations, myspp.obs) ## setdiff takes twice as long as anti_join
      ## create a data frame comprising the route obs for NON TARGET SPECIES (i.e. the zeroes)
      zeroes <-
        anti_join(list$observations, myspp.obs) %>% ## anti_join twices as fast as setdiff in this situation
        distinct(RouteDataID, Year, RTENO, .keep_all = TRUE) %>%
        mutate(AOU = unique(myspp.obs$AOU)[1]) %>%  #apply target species to create zeroes
        dplyr::select(-RouteDataID) # for good measuure to ensure no joins/bind rows pick this up..
      zeroes[grepl("Stop|stop|STOP|RouteTotal", names(zeroes))] <-
        0 # force all values to zero regardless of whether its stop or route-level data (or both)
    } else{
      zeroes = NULL
    }


    # append zero-filled data and target species observations
    list$observations <-
      bind_rows(myspp.obs, zeroes) %>% distinct(RTENO, AOU, Year, .keep_all = TRUE)

    # remove discontinued routes if specified
    if (active.only) {
      # keep active in routes
      list$routes <- list$routes %>%
        filter(Active == 1)
    }

    # remove the removed routes from observations
    list$observations <- list$observations %>%
        filter(RTENO %in% unique(list$routes$RTENO))
    list$routes <- list$routes %>% # for good measure why not
      filter(RTENO %in% unique(list$observations$RTENO))

    ## Keep only the data bbs considers usable when ==1
    if (QualityCurrentID == 1){
      list$weather <- list$weather %>%
        filter(QualityCurrentID == 1)
      }

    ## keep only relevant weather, vehicle and observers
    list$weather <- list$weather %>%
      filter(RTENO %in% unique(list$routes$RTENO)) %>%
      filter(RTENO %in% unique(list$observations$RTENO))


    ## Remove the citation element
    list[which(tolower(names(list)) == "citation")] <- NULL

    # some more munging that needs to be moved into bbsAssistant ideally..
    list <- lapply(list, function(x) { x <- make.rteno(x) })
    list <- lapply(list, function(x) { x <- make.dates(x) })
    list <- lapply(list, function(x) x <- x[!(tolower(names(x)) %in% "routedataid")])
    list <- lapply(list, function(x) { x <- make.integer(x) })
    # glimpse(list[1])

    # Create a metadata list element called, observers
    list$metadata <- list$weather %>%
      ##create binary for if observer's first year on the BBS and on the route
      group_by(ObsN) %>% #observation identifier (number)
      mutate(ObsFirstYearBBS = ifelse(Date == min(Date), 1, 0)) %>%
      group_by(ObsN, RTENO, Year) %>%
      mutate(ObsFirstYearRoute = ifelse(Date == min(Date), 1, 0)) %>%
      ungroup() # to be safe

    gc()

    # create a data frame from the list if requested.
    ###
    ### some data missing from various components of BBS data (retrieved using bbsassistant)
    # cat("Number unique RTENO:\n (routes, observations, vehicle_data, weather)\n", c(bbs_orig$routes$RTENO %>% n_distinct(),
    #                               bbs_orig$observations$RTENO %>% n_distinct(),
    #                               bbs_orig$vehicle_data$RTENO %>% n_distinct(),
    #                               bbs_orig$weather$RTENO %>% n_distinct()))
    # cat("Number unique RouteDataID:\n (observations, vehicle_data, weather)\n", c(
    #                               bbs_orig$observations$RouteDataID %>% n_distinct(),
    #                               bbs_orig$vehicle_data$RouteDataID %>% n_distinct(),
    #                               bbs_orig$weather$RouteDataID %>% n_distinct()))

   # first, join the vehicle_data to the observations
    df <- inner_join(list$observations, list$vehicle_data)
    df <- inner_join(df, list$routes)
    df <- inner_join(df, list$metadata)
    df <- inner_join(df, list$weather)


    ## Finally, munge the spatial dataset a little for downstream integration with eBird
    # Munge the spatial data a bit for downstream alignment with ebird_spatial
    df  <- match_col_names(df)



   cat("The following species are in your BBS data: ", unique(spp), sep = "\n")

  return(df)

  }
