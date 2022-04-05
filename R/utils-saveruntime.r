
# ----FUN: save.runtime ----------------------------------------------------
#' Write Model Runtimes to Working Directory
save.runtime <- function(name, time, fn.mod = "mymodel.rds", fn="model-runtimes.txt"){
  make.file <- ifelse(file.exists(fn), FALSE, TRUE)
  
  time <- round(as.numeric(time), 2)
  
  if(make.file){
    suppressMessages(file.create(fn, showWarnings = FALSE))
    line=paste("name","runtime (minutes)", "output filename", "rundate",sep=",")
    #if first time, add headers
    write(line, fn, append=FALSE)
  }
  
  last <- paste0(fn.mod, "")
  line <- paste(name, round(time), last, Sys.date(), sep=",")
  
  write(line,
        file = fn, append = TRUE)
  
  
}
