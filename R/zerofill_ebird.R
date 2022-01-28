#' Create a Zero-filled Data Object for eBird Observations
#'
#' Creates a zero-filled data object comprising the eBird observations and sampling events data supplied.
#'
#' @param myList A list containing two named data frames, c("observations", "sampling"). This object is the result of \code{filter_ebird_data()}.
#' @param keep.orig Logical. If FALSE will delete the original object, myList, from memory.
#' @param cols.remove A vector of column names to be excluded from the output file.
#' @param cols.to.lowercase Logical. If TRUE will export a data frame where all colnames are in lowercase. Capitalization does not matter.
#' @export
zerofill_ebird <-
  function(myList,
           overwrite=FALSE,
           keep.orig = TRUE,
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
    cols.remove <- stringr::str_replace_all(tolower(cols.remove), " ","_")

    # First remove the unwanted columns
    myList$observations <-
      myList$observations[!names(myList$observations) %in% cols.remove]
    myList$sampling     <-
      myList$sampling[!names(myList$sampling) %in% cols.remove]

    # Create observation count and species column names to add the zeroes to sampling data
    myList$sampling$common_name = unique(myList$observations$common_name)
    myList$sampling$scientific_name = unique(myList$observations$scientific_name)
    myList$sampling$observation_count = 0

    ## Remove the sampling events already in observations
    events.obs <- unique(myList$observations$sampling_event_identifier)
    events.samp <- unique(myList$sampling$sampling_event_identifier)
    events.zeroes.to.add <- setdiff(events.samp, events.obs)#which evenest are in sampling but NOT in obs
    ## filter those events out of sampling df prior to joining
    myList$sampling <- myList$sampling %>%
      dplyr::filter(sampling_event_identifier %in% events.zeroes.to.add)
## This should already be done in filter_ebird_data()--fpr some reason time wasn't working but needs to be double checked
myList$observations <- convert_cols(myList$observations)
myList$sampling     <- convert_cols(myList$sampling)
gc()
# Full join the filtered sampling events to species observations
    cat("joining observations and sampling data frames. takes a few minutes...\n")
    ebird_zf <- bind_rows(myList$observations, myList$sampling) # seems to be the quickest.
gc()
    #### full join needs to be double-checked... havent tested to ensure its not missing or falsely capturing non-detection and detection events.
    ### this check should include a review of filter_ebird_data and zerofill-ebird.
    ### next step ensures only one count per checklist is provided. I think this should have been don with auk_unique in filter_ebird_data...
    ### but perhaps the full_join introduces new shit.
    ebird_zf <- auk::auk_unique(ebird_zf)


    ## munge column names
    ebird_zf <- clean_ebird_colnames(df=ebird_zf)


    # Remove original data object
    if(!keep.orig){rm(myList)}

    return(ebird_zf)
  }
