#' Get Data Structure of JAGS List Inputs
#' @param list A list of named objects.
#' @param dir.output If specified saves the resulting metadata table to file as .csv in this directory
#' @export get_data_structure
get_data_structure <- function(list, dir.output=NULL){
for(i in seq_along(list)){
  if(i == 1 ){
    output = matrix(nrow=max(seq_along(list)), ncol=5)
    colnames(output) = c("class", "length", "nrow", "ncol", "nslice")
  }
  temp   <-  list[[i]]
  if(is.factor(temp)) temp <- as.character(temp)
  class  <-  class(temp)[1] # for multiple classifications will just take the first
  nrow   = dim(temp)[1] #num rows
  ncol   = dim(temp)[2] #num cols
  nslice = dim(temp)[3] #num slices
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
}
rownames(output) <- names(list)

if(is.null(dir.output)) dir.output <- "/"
fn=paste0(dir.output,"jdat-structure" ,".csv")
cat("saving data structure to file: ", fn)
write.csv(output, fn)

return(output)
}
