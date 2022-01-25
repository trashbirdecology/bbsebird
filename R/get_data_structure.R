#' Get Data Structure of JAGS List Inputs
#' @param list A list of named objects.
#' @param dir.output If specified saves the resulting metadata table to file as .csv in this directory
#' @param fn.out If specified will save the resulting table in `dir.output` with filename `fn.out` and extension (".csv")
#' NEEDS WORK
get_data_structure <- function(list, dir.output=NULL, fn.out=NULL){

  ### NEED TO MAKE THS WORK FOR LIST OF LISTS........


  for(i in seq_along(list)){


  if(i == 1 ){
    output = matrix(nrow=max(seq_along(list)), ncol=5)
    colnames(output) = c("class", "length", "nrow", "ncol", "nslice")
  }


  temp   <-  list[[i]]
suppressWarnings(  if(class(temp)=="units") temp <- units::drop_units(temp))
  if(is.factor(temp)) temp <- as.character(temp)
  class  <-  class(temp)[1] # for multiple classifications will just take the first
  if(any(c("numeric", "integer") %in% class)) class <- "vector"
  if(class %in% c("tbl_df", "data.frame", "tibble")) class <- "data frame"
  nrow   = dim(temp)[1] #num rows
  ncol   = dim(temp)[2] #num cols
  nslice = dim(temp)[3] #num slices
  length = length(temp)
  # if all those are NA, then the object is a single
  if(is.null(nrow))nrow=NA
  if(is.null(ncol))ncol=NA
  if(is.null(nslice))nslice=NA
  if(any(c("array", "data.frame", "tbl_df") %in% class)) length <- NA

  output[i,1] <- class
  output[i,2] <- length
  output[i,3] <- nrow
  output[i,4] <- ncol
  output[i,5] <- nslice
}
rownames(output) <- names(list)

# sort output
output <- output[order(rownames(output)),]


if(!is.null(dir.output)){
fn=paste0(dir.output, "/",fn.out,"-structure" ,".csv")
cat("saving data structure to file: ", fn)
write.csv(output, fn)}
# View(output)
return(output)
}
