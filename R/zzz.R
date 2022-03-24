# attach message ---------------------------------------------------------
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "This package is in active development phase. Currently functionality is limited to those who have access to BBS route shapefiles."
  )
}


# package data and models ---------------------------------------------------------
.onAttach <- function(libname, pkgname) {
  path <- system.file(package = "bbsebird")
  ipath <- paste0(path, "/inst/")
  d <- paste0(ipath,
              c("models", # scripts for producing model files, model parameters, inits, and monitors
              "models/nimble", "models/jags", "models/stan")  # package model file outputs
              )
  dirs <- as.list(d)
  names(dirs) <- c("models", "nimble", "jags", "stan")
  fns <- NULL
  for (i in seq_along(d)) {
    dir.create(d[i], recursive = TRUE, showWarnings = FALSE)

    fns <- c(fns, list.files(d[i], recursive=TRUE, full.names=TRUE, pattern=".r|.txt", ignore.case = TRUE))
  }



#   x=lapply(dirs, function(x){})#, pattern = ".txt", full.names=TRUE)
# x
}
