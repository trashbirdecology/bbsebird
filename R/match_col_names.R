#' Munge eBird and BBS Column Names
#'
#' Munge the names of eBird and BBS data frames to ensure consistency.
#'
#' @param x a data frame or matrix of eBird or BBS observations. If no column names match function target, will return same object.
#' @keywords internal
match_col_names <- function(x){
  col_names <- list(date = c("observation_date", "date"),
                    C =    c("observation_count", "count","routetotal"),
                    yday  = c("dayofyear"),
                    lat  = c("lati", "latitude"),
                    lon = c("longitude","long")
                    )

  names(x) <- tolower(names(x))

  for(i in seq_along(col_names)){
    newname=names(col_names)[i]
    oldnames=col_names[[i]]

  toreplace = names(x)[which(names(x) %in% oldnames)]
  x <- x %>%
    rename_with(~newname, toreplace)
}

return(x)

}
