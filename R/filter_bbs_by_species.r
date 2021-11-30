## ID LOVE to be able to throw all the BBS fitlering into a single function within which I use
## across(fun_x(list elemnt )) and use lapply to apply the function to the entire list. but for now i am just gonna do it ,"by hand"
# if("citation" %in% names(list)) bbs$citation<-bbs$citation %>% as.data.frame() # this is required for quickly scanning and filtering lists, sorry
#' @title Subset BBS observations data by one or more species of interest.
#' @description ...
#' @param list A list with element "species_list", obtained from running bbsAssistant::get_bbs_data()...
#' @param search A vector of one or more species (using English Common Name) to subset the data by. Capitalization ignored.
#' @param zero.fill If TRUE and a single species is provided in 'search', this function will output list$observations with zero-filled data.
#' @param active.only Logical. If TRUE keep only active routes. Discontinued routes will be discarded.
#' @export

filter_bbs_by_species <- function(list=bbs.orig, search="Double-crested Cormorant", zero.fill=TRUE, active.only=TRUE, keep.stop.level.data=FALSE){

  # first, remove stop-level data if not needed  to help with mem issues
  if(!keep.stop.level.data){
    # first, ensure the route totals are present in DF (if not, make it)
    cols <- paste0("Stop", c(1:50))
    cols <- cols[which(cols %in% names(list$observations))]
    if(!all(c("TotalSpp", "TotalSp") %in% names(list$observations))){
      list$observations$TotalSpp <- rowSums(list$observations[,cols])
    }
    # remove stop-level data
    list$observations <- list$observations %>%
       dplyr::select(-any_of(cols))
}
  # grab the unique AOU codes
  list$species_list <- list$species_list %>%
    mutate(across(starts_with("English_Common_Name"), tolower)) %>%
    filter(across(any_of("English_Common_Name"), ~ .x %in% tolower(search)))

  # remove routedataid
  list$observations <- list$observations%>%
    dplyr::select(-any_of("RouteDataID"))

  # use the aou to filter down the observations
  myspp.obs <- list$observations %>%
    filter(as.double(AOU) %in% as.double(unique(list$species_list$AOU))) # just hneed to ensure the variables are of same type

  # zeroes
  ## ensure only a single species is provided when zero.fill=TRUE
  search <- search[tolower(search) %in% list$species_list]
  if(zero.fill &
      length(search) > 1){
    warning(
      "When zero.fill=TRUE, only a single species should be provided in interest.species. Using first species in the list. ")
  }
  if(zero.fill){
    # unused <- setdiff(list$observations, myspp.obs) ## setdiff takes twice as long as anti_join
    unused <- anti_join(list$observations, myspp.obs) ## anti_join twices as fast as setdiff in this situation
    zeroes <- unused %>% distinct(RTENO, Year, .keep_all = TRUE) %>%
      mutate(AOU = unique(myspp.obs$AOU)[1])
    zeroes[grepl("Stop|stop|STOP|TotalSp", names(zeroes))] <- 0 # force all values to zero
  }else{zeroes = NULL}
# if(!zero.fill) zeroes = NULL # i know this doesnt make sense, but for some reason when i put this inside the if statement aobve it performs horrifically outside the function...no clue
  # create final observations df for the list
  list$observations <- bind_rows(myspp.obs, zeroes) %>% distinct(RTENO, AOU, Year, .keep_all = TRUE)
  list$observations$RTENO <- as.character(list$observations$RTENO) # just precaution
  list$routes$RTENO <- as.character(list$routes$RTENO) # just precaution


  # remove discontinued routes if specified
  if(active.only){
    # keep active in routes
    list$routes <- list$routes %>%
      filter(Active == 1)
    # remove the removed routes from observations
    list$observations <- list$observations %>%
      filter(list$observations$RTENO %in% unique(list$routes$RTENO))
  }

  print(paste0("The following species are in your BBS data: ", unique(list$species_list$English_Common_Name)))
  return(list) # return the entire list now as a subset of the original list
}
