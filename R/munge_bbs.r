## ID LOVE to be able to throw all the BBS fitlering into a single function within which I use
## across(fun_x(list elemnt )) and use lapply to apply the function to the entire list. but for now i am just gonna do it ,"by hand"
# if("citation" %in% names(list)) bbs$citation<-bbs$citation %>% as.data.frame() # this is required for quickly scanning and filtering lists, sorry
#' @title Subset BBS observations data by one or more species of interest.
#' @description ...
#' @param list A list with element "species_list", obtained from running bbsAssistant::get_bbs_data()...
#' @param spp A vector of one or more species (using English Common Name) to subset the data by. Capitalization ignored.
#' @param zero.fill If TRUE and a single species is provided in 'spp', this function will output list$observations with zero-filled data.
#' @param active.only Logical. If TRUE keep only active routes. Discontinued routes will be discarded.
#' @export munge_bbs

munge_bbs <-
  function(list,
           states.keep = NULL,
           spp = "Double-crested Cormorant",
           zero.fill = TRUE,
           active.only = TRUE,
           keep.stop.level.data = FALSE,
           QualityCurrentID = 1,
           countrynums.keep=c(124, 840)) {
 # First, subset by political/geo
    # data(region_codes) # region codes from bbsAssistant package.
    region_codes.subset <- region_codes %>%
      filter(CountryNum %in% countrynums.keep)
    # have to split up the state num and country num process b/c of Mexico's character issues.
    if(is.null(states.keep))states.keep <- tolower(region_codes.subset$State)
    statenums.keep <-
      region_codes.subset$StateNum[tolower(region_codes.subset$State) %in% tolower(states.keep)]

    list$routes <- list$routes %>%
      filter(StateNum %in% statenums.keep)

    # Remove the citation object in bbs list
    if ("citation" %in% names(list))
      list$citation <- NULL

    # first, remove stop - level data if not needed  to help with mem issues
    if (!keep.stop.level.data) {
      # create a column index for all data not needed at stop-level
      cols <- c(paste0("Stop", c(1:50)), # stop-level counts
                paste0("Noise", c(1:50)), # stop-level noise information
                paste0("Car", c(1:50))) # stop-level car counts)
      # Before dropping stop data, create a column containing total count of species per route/yr
      if (!all(c("RouteTotal") %in% names(list$observations))) {
        cols.stop <- cols[str_detect(cols,"Stop")]
        list$observations$RouteTotal <-
          rowSums(list$observations[, cols.stop])
      }
      for (i in seq_along(list)) {
        list[[i]] <- list[[i]][,!(names(list[[i]]) %in% cols)]
      }
    }


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
      # remove the removed routes from observations
      list$observations <- list$observations %>%
        filter(list$observations$RTENO %in% unique(list$routes$RTENO))
    }

    ## Keep only the data bbs considers usable when ==1
    if (QualityCurrentID == 1){
      list$weather <- list$weather %>%
        filter(QualityCurrentID == 1)
      }

    ## Remove the citation element
    list[which(tolower(names(list)) == "citation")] <- NULL


    # some more munging that needs to ber moved into bbsasst ideally..
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


   cat("The following species are in your BBS data: ", unique(spp), sep = "\n")

  return(df)

  }
