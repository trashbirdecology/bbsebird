#' Specify Arguments and Directories
#'
#' Function to specify arugments and create/point to directories based on those arguments.
#' @importFrom assertthat assert_that
#' @importFrom stringr str_replace
#' @export set_args
set_args <- function(
  # general args
  dir.orig.data,
  dir.proj,
  species,
  species.abbr,
  states=NULL,
  countries=NULL,
  year.range=2008:2019,
  crs.target = 4326,
  get.sunlight = FALSE,
  base.julian.date = (paste0(min(year.range), c("-01-01"))),
  #GRID: arguments for creating spatial sampling grid
  grid.size = 1.00,
  hexagonal = TRUE,
  overwrite.grid = FALSE,
  #BBS: arguments for filtering, downloading, and munging BBS data specifcally
  usgs.layer          = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020",
  cws.layer           = "ALL_ROUTES",
  overwrite.bbs       = FALSE,
  #EBIRD: arguments for filtering and munging the eBird data specifically
  overwrite.ebird     = FALSE,
  max.C.ebird         = 100,
  remove.bbs.obs      = TRUE,
  max.effort.km       = 5,
  max.num.observers   = 10,
  max.effort.mins     = 180,
  complete.checklists.only = TRUE,
  ebird.protocol      = c("Traveling", "Stationary"),
  min.yday            = 91,
  max.yday            = 245,
  mmyyyy              = "nov-2021",
  #JAGS: arguments for customizing the resulting JAGS data list
  overwrite.jdat      = FALSE,
  jagam.args          = list(bs="ds",k=20, family="poisson", sp.prior="log.uniform", diagonalize=TRUE),
  scale.vars          = TRUE
){

  ## check args
  temp=c("complete.checklists.only", "scale.vars", 'overwrite.ebird',"remove.bbs.obs" ,"overwrite.bbs", "hexagonal", "get.sunlight")
  for(i in seq_along(temp))assertthat::assert_that(is.logical(eval(parse(text=temp[i]))), msg = paste("argument ", temp[i],"must be a logical."))
  temp=c("min.yday", "max.yday", "max.effort.km", "max.effort.mins", "max.C.ebird",
         "grid.size", "crs.target","year.range")
  for(i in seq_along(temp)){assertthat::assert_that(class(eval(parse(text = temp[i]))) %in% c("integer", "numeric"),
                                                    msg = paste("argument ", temp[i], "must be a logical."))}
  rm(temp)

  ## munge the states and countries indexes for use in dir/proj dir reation
  if(!exists("states")) states <- NULL
  if(!is.null(states)){regions <- states}else{regions <- countries}

  ## proj.shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
  subdir.proj <-  dubcorms:::proj.shorthand(species.abbr, regions, grid.size, year.range, max.C.ebird)
  if(nchar(subdir.proj)>100){cat("subdir.proj is very long. specifing a new name for project."); subdir.proj="myproject"}
  dirs        <-  dubcorms:::dir_spec(dir.orig.data, dir.proj, subdir.proj) # create and/or specify directories for later use.

  # ensure all directories exist
  suppressWarnings(stopifnot(all(lapply(dirs, dir.exists))))


  # bundle all arguments
  args <-
    c(
      ##directories
      "dirs$dir.proj",
      "dirs$dir.jags",
      "dirs$dir.plots",
      "dirs$dir.models",
      "dirs$dir.bbs.out",
      "dirs$dir.ebird.out",
      "dirs$dir.spatial.out",
      "dirs$dir.ebird.in",
      "dirs$cws.routes.dir",
      "dirs$usgs.routes.dir",
      ##others
      "species",
      "species.abbr",
      "states",
      "countries",
      "year.range",
      "crs.target",
      "get.sunlight",
      "base.julian.date",
      "grid.size",
      "hexagonal",
      "overwrite.grid",
      "usgs.layer",
      "cws.layer",
      "overwrite.bbs",
      "overwrite.ebird",
      "max.C.ebird",
      "remove.bbs.obs",
      "max.effort.km",
      "max.num.observers",
      "max.effort.mins",
      "complete.checklists.only",
      "ebird.protocol",
      "min.yday",
      "max.yday",
      "mmyyyy",
      "overwrite.jdat",
      "jagam.args",
      "scale.vars"
    )

  ## create the list of ebird elements
  list.out <-NULL
  for (z in seq_along(args)) {
    new = eval(parse(text = args[z]))# this is necessary for some reason idk why
    list.out[[args[z]]] <- new
  }
  ## list names
  names=NULL
  for(i in seq_along(args)){
    names[i] <-
      stringr::str_replace(args[i], pattern="dirs\\$", replacement="")
  }

  names(list.out) <- names
  # create an output
  return(list.out)
}
