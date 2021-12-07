#' Create a Zero-filled Data Object for eBird Observations
#'
#' Creates a zero-filled data object comprising the eBird observations and sampling events data supplied.
#'
#' @param myList A list containing two named data frames, c("observations", "sampling"). This object is the result of \code{filter_ebird_data()}.
#' @param keep.orig Logical. If FALSE will delete the original object, myList, from memory.
#' @param cols.remove A vector of column names to be excluded from the output file.
#' @param cols.to.lowercase Logical. If TRUE will export a data frame where all colnames are in lowercase.
#' @export zerofill_ebird
zerofill_ebird <-
  function(myList,
           keep.orig = FALSE,
           cols.remove = c(
             "SUBSPECIES COMMON NAME",
             "TAXONOMIC ORDER",
             "LAST EDITED DATE",
             "CATEGORY",
             "APPROVED",
             "REVIEWED",
             "SPECIES COMMENTS",
             "...48",
             "HAS MEDIA",
             "REASON"
           )) {
    # Force columns to lowercase
      colnames(myList$sampling) <- tolower(colnames(myList$sampling))
      colnames(myList$observations) <- tolower(colnames(myList$observations))
      message("column names forced to lowercase.")
      cols.remove <- tolower(cols.remove)


    # First remove the unwanted columns
    myList$observations <-
      myList$observations[!names(myList$observations) %in% cols.remove]
    myList$sampling     <-
      myList$sampling[!names(myList$sampling) %in% cols.remove]

    # Create observation count and species column names to add the zeroes to sampling data
    myList$sampling$`common name` = unique(myList$observations$`common name`)
    myList$sampling$`observation count` = 0
    myList$observations$`observation count` = as.integer(myList$observations$`observation count`)

    # Full join the filtered sampling events to species observations
    ebird_zf <-
      full_join(myList$observations, myList$sampling)


    # Create date and julian day variables
    ebird_zf <-
      ebird_zf %>% mutate(
        julian = lubridate::yday(`observation date`),
        year = lubridate::year(`observation date`)
      )



    # Remove original data object
    if(!keep.orig){rm(myList)}

    return(ebird_zf)
  }
