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


# ----FUN: save.runtime ----------------------------------------------------
#' Write Model Runtimes to Working Directory
save.runtime <- function(name, time,fn.mod = "mymodel.rds", fn="model-runtimes.txt"){
  make.file <- ifelse(file.exists(fn), FALSE, TRUE)

  time <- round(as.numeric(time), 2)

  if(make.file){
    suppressMessages(file.create(fn, showWarnings = FALSE))
    line=paste("name","runtime (minutes)", "output filename", sep=",")
    #if first time, add headers
    write(line, fn, append=FALSE)
  }

  last <- paste0(fn.mod, "")
  line <- paste(name, round(time), last, sep=",")

  write(line,
        file = fn, append = TRUE)


}
