#' Create a Zero-filled Data Object for eBird Observations
#'
#' Creates a zero-filled data object comprising the eBird observations and sampling events data supplied.
#'
#' @param myList A list containing two named data frames, c("observations", "sampling"). This object is the result of \code{filter_ebird_data()}.
#' @param keep.orig Logical. If FALSE will delete the original object, myList, from memory.
#' @param cols.remove A vector of column names to be excluded from the output file.
#' @param cols.to.lowercase Logical. If TRUE will export a data frame where all colnames are in lowercase. Capitalization does not matter.
#' @export zerofill_ebird
zerofill_ebird <-
  function(myList,
           keep.orig = FALSE,
           cols.remove = c(
             "SUBSPECIES_COMMON NAME",
             "TAXONOMIC_ORDER",
             "LAST_EDITED_DATE",
             "CATEGORY",
             "APPROVED",
             "REVIEWED",
             "SPECIES_COMMENTS",
             "...48",
             "HAS_MEDIA",
             "REASON",
             "TRIP_COMMENTS"
           )) {
    # Force columns to lowercase
    colnames(myList$sampling) <- tolower(colnames(myList$sampling))
    colnames(myList$observations) <- tolower(colnames(myList$observations))
    cols.remove <- str_replace_all(tolower(cols.remove), " ","_")

    # First remove the unwanted columns
    myList$observations <-
      myList$observations[!names(myList$observations) %in% cols.remove]
    myList$sampling     <-
      myList$sampling[!names(myList$sampling) %in% cols.remove]

    # Create observation count and species column names to add the zeroes to sampling data
    myList$sampling$common_name = unique(myList$observations$common_name)
    # myList$sampling$observation_count = 0

    # # Just going to force to lubridate::date() to be able to perform a full join
    # myList$observations$observation_date <- lubridate::as_date(myList$observations$observation_date)
    # myList$sampling$observation_date <- lubridate::as_date(myList$sampling$observation_date)


    myList$sampling <- convert_cols(myList$sampling)
    myList$observations <- convert_cols(myList$observations)

    # Full join the filtered sampling events to species observations
    ebird_zf <-
      full_join(myList$observations, myList$sampling) %>%
      mutate(observation_count = replace_na(observation_count, 0))
    #### full join needs to be double-checked... havent tested to ensure its not missing or falsely capturing non-detection and detection events.
    ### this check should include a review of filter_ebird_data and zerofill-ebird.
    ### next step ensures only one count per checklist is provided. I think this should have been don with auk_unique in filter_ebird_data...
    ### but perhaps the full_join introduces new shit.
    cat("still working...")
    ebird_zf <- ebird_zf %>%
      group_by(checklist_id) %>%
      filter(observation_count == max(observation_count)) %>%
      ungroup()

    # Remove original data object
    if(!keep.orig){rm(myList)}

    return(ebird_zf)
  }
