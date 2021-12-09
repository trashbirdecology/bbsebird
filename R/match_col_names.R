#' Ensure Consistency Among eBird and BBS Datasets
#'
#' ..
#' @param x a data frame
#' @keywords internal
#' e.g., x =
match_col_names <- function(x){

  col_names <- list(date = c("observation_date", "date"),
                    C =    c("observation_count", "count","routetotal"),
                    yday  = c("dayofyear"),
                    lat  = c("lati", "latitude"),
                    lon = c("longitude","long")
                    )

  for(i in seq_along(col_names)){
    newname=names(col_names)[i]
    oldnames=col_names[[i]]

  toreplace = names(x)[which(names(x) %in% oldnames)]
  x <- x %>%
    rename_with(~newname, toreplace)
}

return(x)

}
