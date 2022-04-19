
# ----FUN: save.runtime ----------------------------------------------------
#' Write Model Runtimes to Working Directory
#' @param name description of model to be saved under heading 'name'
#' @export save.runtime
#' @param time optional runtime as numeric or integer
#' @param other optional information to be saved under heading 'other' (e.g., number iterations, number chains, etc.)
#' @param fn path to existing or desired output file. This is where the runtimes and arguments will be saved. If file DNE, this function will create it.
save.runtime <- function(name=NULL, time=NULL, other = NULL, fn="model-runtimes.txt"){
  make.file <- ifelse(file.exists(fn), FALSE, TRUE)

  if(make.file){
    suppressMessages(file.create(fn, showWarnings = FALSE))
    line=paste("name","runtime", "other", "rundate",sep=",")
    #if first time, add headers
    write(line, fn, append=FALSE)
  }

  line <- paste(name, time, other, Sys.Date(), sep=",")

  write(line,
        file = fn, append = TRUE)

}
