## ID LOVE to be able to throw all the BBS fitlering into a single function within which I use
## across(fun_x(list elemnt )) and use lapply to apply the function to the entire list. but for now i am just gonna do it ,"by hand"
# if("citation" %in% names(list)) bbs$citation<-bbs$citation %>% as.data.frame() # this is required for quickly scanning and filtering lists, sorry
#' @title Subset BBS observations data by one or more species of interest.
#' @description
#' @param list A list with element "species_list", obtained from running bbsAssistant::get_bbs_data()...
#' @param search A vector of one or more species (using English Common Name) to subset the data by. Capitalization ignored.
#' @export
#'

filter_bbs_by_species <- function(list, search = interest.species){#, unid=FALSE) {
  # grab the unique AOU codes
  list$species_list <- list$species_list %>%
    mutate(across(starts_with("English_Common_Name"), tolower)) %>%
    filter(across(any_of("English_Common_Name"), ~ .x %in% tolower(search)))
  # use the aou to filter down the observations
  list$observations <- list$observations %>%
    filter(as.double(AOU) %in% as.double(unique(list$species_list$AOU))) # just hneed to ensure the variables are of same type

  print(cat("The following species are in your BBS data: ", paste(unique(list$species_list$English_Common_Name))))
  return(list) # return the entire list now as a subset of the original list
}
