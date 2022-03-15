#' Specify Input/Output Directories For Project
#'
#' @description  Produces a list comprising directories for munged data, JAGS data lists, figures, and model outputs. Should be used with `list2env()` to assign directories to desired environment (typically .GlobalEnv)
#' @param dir.orig.data Location of the original BBS and eBird data. This directory should house multiple directories, including the BBS route shapefiles, the eBird database.
#' @param dir.proj Project directory. Assumes current working directory. This is where the directories and output files will be stored.
#' @param subdir.proj The name of a subdirectory to exist within dir.proj. Can be quickly created outside this function using `dubcorms::set_proj_shorthand()`
#' @importFrom stringr str_replace str_detect
#' @export dir_spec
#
dir_spec <- function(dir.orig.data, dir.proj=NULL, subdir.proj=NULL) {

  if(is.null(dir.proj)) dir.proj <- getwd()


  if(!is.null(subdir.proj) & nchar(subdir.proj)>100){cat("subdir.proj is very long. specifying a new name for project."); subdir.proj="myproject"}



  # first, create the proj directory if necessary
  if(!dir.exists(dir.proj)) dir.create(dir.proj, showWarnings = FALSE)
  # redefine dir.proj if subdir specified
  dir.proj <- paste0(dir.proj, "/", subdir.proj)
  dir.proj <- stringr::str_replace(dir.proj, "//","/")
  if(!dir.exists(dir.proj)) dir.create(dir.proj, showWarnings = FALSE)

  if (!endsWith(dir.orig.data, "/")){
    dir.orig.data <- paste0(dir.orig.data, "/")}

  ## Where is your original eBird data stored?
  dir.ebird.in <- paste0(dir.orig.data, "ebird")
  ## Where are the BBS route shapefiles stored?
  cws.routes.dir <- paste0(dir.orig.data, "/bbs/route_shapefiles/cws")
  cws.routes.dir <- stringr::str_replace(cws.routes.dir, "//","/")
  usgs.routes.dir <-
    paste0(dir.orig.data, "/bbs/route_shapefiles/usgs")
  usgs.routes.dir <- stringr::str_replace(usgs.routes.dir, "//","/")

  if (!any(length(list.files(cws.routes.dir)) > 0))
    stop(
      "No files exist `cws.routes.dir` or `usgs.routes.dir`. Please check directory specification.\n"
    )

  if (!length(list.files(dir.ebird.in) > 0))
    stop("No files exist in `dir.ebird.in`. Please check directory specification.\n")


  # trim trailing and leading forward/back slash from dir.proj
  while(startsWith(dir.proj, "/")){ dir.proj <- substr(dir.proj, 2, nchar(dir.proj))}
  # specify directories within dir.proj
  dir.models <- "/models/"  # save model files
  dir.bbs.out <- "/bbs/"
  dir.ebird.out <- "/ebird/"
  dir.spatial.out <- "/spatial/"
  dir.results <- "/results/"
  dir.plots <- "/plots/"
  # add dir.proj to directories and dir.create them
  x=
  # sapply(
    c(
      'dir.bbs.out',
      'dir.ebird.out',
      'dir.spatial.out',
      'dir.results',
      'dir.models',
      'dir.plots',
      'dir.proj')
  for(i in seq_along(x))dir.create(eval(parse(text=paste0(x))), showWarnings=FALSE)
  cat("Project directory output files will go to ", dir.proj, "/n")

  subset.names <- paste0("dir.",
                         c(
                           "plots",
                           "models",
                           "bbs.out",
                           "ebird.out",
                           "spatial.out",
                           "results"
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

