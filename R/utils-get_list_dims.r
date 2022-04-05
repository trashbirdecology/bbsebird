#' Get Dimensions of List Elements
#'
#'
#' @param list List of elements
#' @param print logical if TRUE will print list element dimensions to screen.
#' @export get.list.dims
get.list.dims <- function(list, print=TRUE){
  out <- list()
  for(i in seq_along(list)){
    d=dim(list[[i]])
    if(is.null(d)){d = length(list[[i]])}
    # if(print)  print(d)
    out[[i]] <- d
  }
  names(out) <- names(list)
  return(out)

}

