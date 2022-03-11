#' Create a Zero-filled Data Object for eBird Observations
#'
#' Creates a zero-filled data object comprising the eBird observations and sampling events data supplied.
#'
#' @param list A list containing two named data frames, c("observations", "sampling"). This object is the result of \code{filter_ebird_data()}.
#' @param cols.remove A vector of column names to be excluded from the output file.
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter bind_rows
#' @importFrom auk auk_unique
#' @export zerofill_ebird
zerofill_ebird <-
  function(list,
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

    ## binding vars
    sampling_event_identifier <- NULL

    # Force columns to lowercase
    colnames(list$sampling) <- tolower(colnames(list$sampling))
    colnames(list$observations) <-
      tolower(colnames(list$observations))
    cols.remove <-
      stringr::str_replace_all(tolower(cols.remove), " ", "_")

    # First remove the unwanted columns
    list$observations <-
      list$observations[!names(list$observations) %in% cols.remove]
    list$sampling     <-
      list$sampling[!names(list$sampling) %in% cols.remove]

    # Create observation count and species column names to add the zeroes to sampling data
    list$sampling$common_name = unique(list$observations$common_name)
    list$sampling$scientific_name = unique(list$observations$scientific_name)
    list$sampling$observation_count = 0

    ## Remove the sampling events already in observations
    events.obs <-
      unique(list$observations$sampling_event_identifier)
    events.samp <- unique(list$sampling$sampling_event_identifier)
    events.zeroes.to.add <-
      setdiff(events.samp, events.obs)#which evenest are in sampling but NOT in obs
    ## filter those events out of sampling df prior to joining
    list$sampling <- list$sampling %>%
      dplyr::filter(sampling_event_identifier %in% events.zeroes.to.add)
    ## This should already be done in filter_ebird_data()--fpr some reason time wasn't working but needs to be double checked
    list$observations <- convert_cols(list$observations)
    list$sampling     <- convert_cols(list$sampling)
    gc()
    # Full join the filtered sampling events to species observations
    cat("joining observations and sampling data frames. takes a few minutes...\n")
    ebird_zf <-
      dplyr::bind_rows(list$observations, list$sampling) # seems to be the quickest.
    gc()
    #### full join needs to be double-checked... havent tested to ensure its not missing or falsely capturing non-detection and detection events.
    ### this check should include a review of filter_ebird_data and zerofill-ebird.
    ### next step ensures only one count per checklist is provided. I think this should have been don with auk_unique in filter_ebird_data...
    ### but perhaps the full_join introduces new shit.
    ebird_zf <- auk::auk_unique(ebird_zf)

    # Replace NA values with zero
    ebird_zf$observation_count[is.na(ebird_zf$observation_count)] <- 0

    return(ebird_zf)
  }
