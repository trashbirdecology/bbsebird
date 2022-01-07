#' Specify input and output directories.
#'
#' @description  Produces a list comprising directories for munged data, JAGS data lists, figures, and model outputs. Should be used with `list2env()` to assign directories to desired environment (typically .GlobalEnv)
#' @param dir.orig.data Location of the original BBS and eBird data. This directory should house multiple directories, including the BBS route shapefiles, the eBird database.
#' @param dir.proj Project directory. Assumes current working directory. This is where the directories and output files will be stored.
#' @param subdir.proj The name of a subdirectory to exist within dir.proj. Can be quickly created outside this function using `dubcorms::proj.shorthand()`
#' @export dir_spec

dir_spec <- function(dir.orig.data, dir.proj=NULL, subdir.proj=NULL) {
# browser()
  if(is.null(dir.proj)) dir.proj <- getwd()
  # first, create the proj directory if necessary
  if(!file.exists(dir.proj)) dir.create(dir.proj, showWarnings = FALSE)
  # redefine dir.proj if subdir specified
  dir.proj <- paste0(dir.proj, "/", subdir.proj)
  dir.proj <- str_replace(dir.proj, "//","/")
  if(!file.exists(dir.proj)) dir.create(dir.proj, showWarnings = FALSE)

  if (!endsWith(dir.orig.data, "/")){
    dir.orig.data <- paste0(dir.orig.data, "/")}

  ## Where is your original eBird data stored?
  dir.ebird.in <- paste0(dir.orig.data, "ebird")
  ## Where are the BBS route shapefiles stored?
  cws.routes.dir <- paste0(dir.orig.data, "/bbs/route_shapefiles/cws")
  cws.routes.dir <- str_replace(cws.routes.dir, "//","/")
  usgs.routes.dir <-
    paste0(dir.orig.data, "/bbs/route_shapefiles/usgs")
  usgs.routes.dir <- str_replace(usgs.routes.dir, "//","/")

  if (!any(length(list.files(cws.routes.dir)) > 0))
    stop(
      "No files exist `cws.routes.dir` or `usgs.routes.dir`. Please check directory specification.\n"
    )

  if (!length(list.files(dir.ebird.in) > 0))
    stop("No files exist in `dir.ebird.in`. Please check directory specification.\n")


  # trim trailing and leading forward/back slash from dir.proj
  while(startsWith(dir.proj, "/")){ dir.proj <- substr(dir.proj, 2, nchar(dir.proj))}
  # specify directories within dir.proj
  dir.jags <- "/jags/"
  dir.models <- paste0(dir.jags, "models/")  # save model files
  dir.bbs.out <- "/bbs/"
  dir.ebird.out <- "/ebird/"
  dir.spatial.out <- "/spatial/"
  dir.plots <- "/plots/"
  # add dir.proj to direcotries and dir.create them
  sapply(
    c(
      dir.bbs.out,
      dir.ebird.out,
      dir.spatial.out,
      dir.jags,
      dir.models,
      dir.plots
    ),
    FUN = function(x)
      dir.create(paste0(dir.proj, x), showWarnings = FALSE)
  )

  cat("Project directory output files will go to ", dir.proj)


subset.names <- paste0("dir.",
                         c("jags",
                           "plots",
                           "models",
                           "bbs.out",
                           "ebird.out",
                           "spatial.out"
                         ))

base.names <- c("dir.proj",
                "dir.ebird.in",
                "cws.routes.dir",
                "usgs.routes.dir")

paths <- list()
  for(i in seq_along(subset.names)){
  paths[[i]] <- stringr::str_replace(paste0(dir.proj, "/", eval(parse(text=subset.names[i]))), "//","/")
  names(paths)[[i]] <- subset.names[i]
  }
x=length(paths)
y=length(base.names)
z=x+y
for(i in (x+1):z){
  j = i-x
  paths[[i]] <- stringr::str_replace(eval(parse(text=base.names[j])), "//","/")
  names(paths)[i] <- base.names[j]
}
paths



  return(paths)

  }

