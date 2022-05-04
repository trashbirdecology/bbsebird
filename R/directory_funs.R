#' Specify Input/Output Directories For Project
#'
#' @description  Produces a list comprising directories for munged data, JAGS data lists, figures, and model outputs. Should be used with `list2env()` to assign directories to desired environment (typically .GlobalEnv)
#' @param dir.orig.data Location of the original BBS and eBird data. This directory should house multiple directories, including the BBS route shapefiles, the eBird database.
#' @param dir.proj Project directory. Assumes current working directory. This is where the directories and output files will be stored.
#' @param subdir.proj The name of a subdirectory to exist within dir.proj. Can be quickly created outside this function using 'dubcorms::set_proj_shorthand'
#' @importFrom stringr str_replace str_detect
#' @export dir_spec

dir_spec <- function(dir.orig.data, dir.proj=NULL, subdir.proj=NULL) {
  if(is.null(dir.proj)) dir.proj <- getwd()

  # first, create the proj directory if necessary
  dir.create(dir.proj, showWarnings = FALSE)
  dir.proj.orig <- dir.proj ## save this to save the files that are common across projects....
  # redefine dir.proj if subdir specified
  if(!is.null(subdir.proj)) dir.proj <- paste0(dir.proj,"/", subdir.proj, "/")
  dir.proj <- gsub(x=dir.proj, pattern = "//","/")
  dir.create(dir.proj, showWarnings = FALSE)

  if (!endsWith(dir.orig.data, "/")){
    dir.orig.data <- paste0(dir.orig.data, "/")}

  ## Where is your original eBird data stored?
  dir.ebird.in <- paste0(dir.orig.data, "ebird")
  ## Where are the BBS route shapefiles stored?
  cws.routes.dir <- paste0(dir.orig.data, "/bbs/route_shapefiles/cws")
  cws.routes.dir <- gsub(x=cws.routes.dir,pattern= "//",replacement = "/")
  usgs.routes.dir <-
    paste0(dir.orig.data, "/bbs/route_shapefiles/usgs")
  usgs.routes.dir <- gsub(x = usgs.routes.dir, pattern = "//",replacement =  "/")

  if (!any(length(list.files(cws.routes.dir)) > 0))
    message(
      "No files exist `cws.routes.dir` or `usgs.routes.dir`. Please check directory specification for dirs$dir.bbs.in.\n"
    )

  if (!length(list.files(dir.ebird.in) > 0))
    stop("No files exist in `dir.ebird.in`. Please check directory specification.\n")


  # trim trailing and leading forward/back slash from dir.proj
  while(startsWith(dir.proj, "/")){ dir.proj <- substr(dir.proj, 2, nchar(dir.proj))}
  # specify directories within dir.proj
  models <- "/models/"  # save model files
  bbs.out <- "/bbs/"
  ebird.out <- "/ebird/"
  results <- "/results/"
  plots <- "/plots/"
  spatial <- "/spatial/"
  # spatial <- paste0(dir.proj.orig, "spatial-data/")
  # add dir.proj to directories and dir.create them
  mylist <- list(
      'bbs.out',
      'ebird.out',
      'results',
      'models',
      'plots',
      'spatial'
      )

  mylist <- lapply(mylist, function(x) paste0(subdir.proj, eval(parse(text=x))))

subset.names <-            c(
                           "plots",
                           "models",
                           "bbs.out",
                           "ebird.out",
                           "spatial",
                           "results"
                         )

base.names <- c("dir.proj",
                "dir.ebird.in",
                "cws.routes.dir",
                "usgs.routes.dir")

paths <- list()
  for(i in seq_along(subset.names)){
    # if(subset.names[i] == "spatial"){
    #   paths[[i]] <-  spatial} else{
        paths[[i]] <- gsub(x=paste0(dir.proj, "/", eval(parse(text=subset.names[i]))), pattern="//",replacement="/")
        # }
     paths[[i]] <- gsub(x=paths[[i]], pattern = "//", replacement = "/")
     names(paths)[[i]] <- subset.names[i]
     dir.create(paths[[i]], recursive=TRUE, showWarnings = FALSE)
     stopifnot(dir.exists(paths[[i]]))
}

x=length(paths)
y=length(base.names)
z=x+y
for(i in (x+1):z){
  j = i-x
  paths[[i]] <- gsub(x=eval(parse(text=base.names[j])),pattern="//",replacement="/")
  names(paths)[i] <- base.names[j]
}

## Ensure directories are created (need to fix bug in earlier part of script...not making dirs)
# if("Linux" %in% Sys.info()[1]){ ind <- TRUE}else{ind <- FALSE}
for(i in seq_along(paths)){
  if(!dir.exists(paths[[i]])) dir.create(paths[[i]], showWarnings = FALSE)
  paths[[i]]   <- gsub(x=paths[[i]],pattern="//",replacement="/") # replace all double forward slashes...
#  if(ind ) paths[[i]]   <- paste0("/", paths[[i]])
}
names(paths)[which(names(paths)=="dir.proj")] <- "project"




return(paths)
}


#' Auto generate a subdirectory name based on project parameters
#'
#' Generates a shorthand name for project based on species, country, states, years, and grid cell size.
#' @param species string of species names using ISO-366-2
#' @param regions string of countries or states
#' @param grid.size size of desired grid cell
#' @param years Vector of years. will take the min and max value
#' @export set_proj_shorthand
set_proj_shorthand <- function(species,
                               countries=c("us", "ca"),
                               states=NULL,
                               grid.size,
                               years) {
  s=which(nchar(species)==nchar(gsub(" ", "", species)))
  if(length(s)>0) species <- species[s]

  stopifnot(all(tolower(states) %in% tolower(bbsAssistant::region_codes$iso_3166_2)))
  stopifnot(all(tolower(countries) %in% tolower(bbsAssistant::region_codes$iso_a2)))
  if(!is.null(states)) {
    regions <- states
  } else{
    regions <- countries
  }

  ## munge the states first.
  regions <- toupper(regions)
  regions <-
    gsub(
      x = regions,
      pattern = " ",
      replacement = "",
      ignore.case = TRUE
    )
  regions <-
    gsub(
      x = regions,
      pattern = "-",
      replacement = "",
      ignore.case = TRUE
    )
  regions <- gsub(x = regions, pattern = ";|,|\\|,", "-")
  regions <- paste(regions, collapse = "-")

  if (length(species) > 1)
    cat(
      "multiple species indexes supplied. please check the project directory naming to ensure it properly represents desired species."
    )

  x <- paste0(
    species[nchar(species) == (max(nchar(species)))][1],
    #take min or max to assign species to dir name
    "_",
    # regions
    regions,
    "_",
    # size of grid cells
    grid.size * 111,
    "km",
    "_",
    # time period
    min(years),
    "-",
    max(years)
    )
  return(tolower(x))
}
