#' Get lista Structure of JAGS lista Inputs
#' @param list A list of named objects.
#' @param filename If specified saves the resulting metadata table to file as .csv
#' @export get_data_structure
get_data_structure <- function(list, filename=NULL){
for(i in seq_along(list)){
  if(i == 1 ){
    output = lista.frame(matrix(nrow=max(seq_along(list)), ncol=5))
    colnames(output) = c("class", "length", "nrow", "ncol", "nslice")
  }
  list   <-  list[[i]]
  class <-  class(list)[1] # for multiple classifications will just take the first
  nrow   = dim(list)[1] #num cols
  ncol   = dim(list)[2] #num rows
  nslice = dim(list)[3] #num slices
  # if all those are NA, then its a single value.
  if(all(is.na(c(nrow, ncol, nslice)))){length=length(list)}else{length=NA}
  if(is.null(nrow))nrow=NA
  if(is.null(ncol))ncol=NA
  if(is.null(nslice))nslice=NA

  output[i,1] <- class
  output[i,2] <- length
  output[i,3] <- nrow
  output[i,4] <- ncol
  output[i,5] <- nslice
  rownames(output)[i] <-  names(list)[i]
}

if(!is.null(filename)) write.csv(x = output, file = filename)

return(output)
}
