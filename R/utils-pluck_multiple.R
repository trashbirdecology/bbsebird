#' Pluck Multiple Elements from List Object
#'
#' @param x list object
#' @param keep   character or integer vector of one or more list element names or numbers to include in list output
#' @param remove character or integer vector of one or more list element names or numbers to remove from list output
#' @export pluck_multiple
#' @source https://stackoverflow.com/questions/46983716/does-a-multi-value-purrrpluck-exist

pluck_multiple <- function(x, keep=NULL, remove=NULL) {
  if(!is.null(keep) & !is.null(remove)){
    warning("Both `keep` and `remove` specified. Ignoring argument `remove`.\n") # both cant be populated..
    remove <- NULL
  }


  if(!is.null(remove)){
    keep <- which(!names(x) %in% remove)
  }

  y <- `[`(x, keep)
  return(y)
}


