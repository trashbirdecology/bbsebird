#' @title My big fkn fun
#' @description  tba
#' @param species  tba
#' @param species.abbr  tba
#' @param year.range  tba
#' @param base.julian.date  tba
#' @param crs.target  tba
#' @param get.sunlight  tba
#' @param countries  tba
#' @param states  tba
#' @param dir.proj  tba
#' @param dir.orig.data  tba
#' @param grid.size  tba
#' @param overwrite.grid  tba
#' @param hexagonal  tba
#' @param usgs.layer  tba
#' @param cws.layer  tba
#' @param overwrite.bbs  tba
#' @param max.C.ebird  tba
#' @param remove.bbs.obs  tba
#' @param max.effort.km  tba
#' @param max.num.observers  tba
#' @param max.effort.mins  tba
#' @param complete.checklists.only  tba
#' @param ebird.protocol  tba
#' @param min.yday  tba
#' @param max.yday  tba
#' @param mmyyyy  tba
#' @param jagam.args  tba
#' @param scale.vars  tba
#' @importFrom lubridate hms ymd
#' @importFrom bbsAssistant grab_bbs_data munge_bbs_data
#' @importFrom dplyr select filter group_by ungroup
#' @importFrom stringr str_detect
#' @importFrom mapview mapview
#' @export my_big_fucking_function
my_big_fucking_function <- function(
  # REQUIRED ARGUMENTS
  dir.orig.data, # directory with subdirectories /bbs/ and /ebird/, within which the BBS route shapefiles and eBird data are stored.
  # OPTIONAL ARGUMENTS
  #general arguments
  dir.proj     = NULL, # project directory. If NULL will specify and create a project directory within the current working directory. A single primary directory is made for each species within which new directories comprise combinations of years/spatial extent/etc. are housed.
  species      = c("Double-crested Cormorant", "Nannopterum auritum"),
  species.abbr = c("DCCO", "doccor"), # call all the variations especially those that appear int eh EBIRD dwnloaded files
  countries    = c("US", "CA"), ## string of  countries Call \code{dubcorms::iso.codes} to find relevant codes for Countries and States/Prov/Territories.
  states       = NULL, ## string of  states/provinces/territories. Call \code{dubcorms::iso.codes} to find relevant codes for Countries and States/Prov/Territories.
  year.range   = 2008:2019,
  base.julian.date    = lubridate::ymd(paste0(min(year.range), c("-01-01"))), # used as base date for Julian dates.
  crs.target   = 4326, #target CRS for all created spatial layers
  get.sunlight = FALSE, #TRUE will caculate sunrise/light and moonrise/light times/durations. Only specify if data is needed as it takes a bit of time to run against the eBird data.
  #GRID: arguments for creating a spatial grid
    grid.size  = 1.00, # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)
    hexagonal  = TRUE, # if FALSE will produce square grid cells against CRS.target.
    overwrite.grid = FALSE, # logical FALSE will not overwrite the grid if one already exists in dir.proj
  #BBS: arguments for filtering, downloading, and munging BBS data specifcally
    usgs.layer = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020", # name of the USGS BBS route shapefile to use
    cws.layer  = "ALL_ROUTES",
    overwrite.bbs = FALSE,
  #EBIRD: arguments for filtering and munging the eBird data specifically
    overwrite.ebird = FALSE,
    max.C.ebird = 100, # maximum number of birds of the select species counted in a single ebird checklist
    remove.bbs.obs  = TRUE, # TRUE will attempt to remove BBS observations from the eBird database. This is currently a crude method.
    max.effort.km = 5,
    max.num.observers = 10,
    max.effort.mins = 180,
    complete.checklists.only = TRUE,
    ebird.protocol = c("Traveling", "Stationary"),
    min.yday = 91,
    max.yday = 245,
    mmyyyy = "sep-2021", # the month and year of the eBird data downloads on file
  #JAGS: arguments for customizing the resulting JAGS data list
   jagam.args = list(bs="ds",k=20, family="poisson", sp.prior="log.uniform", diagonalize=TRUE),
   scale.vars = TRUE # whether or not to z-scale select variables.

){

# TEST THE ARGUMENTS  ---------------------------------------------------------
### need to add up front checks (e.g., stop if not integer, or character, or if states are n the preapproved list (whch I still need to create...))
### add a bunch of stopif nots across a loop of the logicals or assertthat
  temp=c("complete.checklists.only", "scale.vars", 'overwrite.ebird',"remove.bbs.obs" ,"overwrite.bbs", "hexagonal", "get.sunlight")
  for(i in seq_along(temp))assertthat::assert_that(is.logical(eval(parse(text=temp[i]))), msg = paste("argument ", temp[i],"must be a logical."))
  temp=c("min.yday", "max.yday", "max.effort.km", "max.effort.mins", "max.C.ebird",
         "grid.size", "crs.target","year.range")
  for(i in seq_along(temp)){assertthat::assert_that(class(eval(parse(text = temp[i]))) %in% c("integer", "numeric"),
                            msg = paste("argument ", temp[i], "must be a logical."))}


  rm(temp)
# MUNGE ARGUMENTS A LITTLE ---------------------------------------------------------
  ## Munge the states and countries indexes for use in dir/proj dir reation
  if(!is.null(states)){regions <- states}else{regions <- countries}
  regions <- toupper(regions)
  regions <- gsub(x=regions, pattern=" ", replacement="", ignore.case=TRUE)
  regions <- gsub(x=regions, pattern="-", replacement="", ignore.case=TRUE)
  regions <- gsub(x=regions,pattern=";|,|\\|,", "-")
  regions <- paste(regions, collapse = "-")

# SPECIFY/CREATE PROJECT DIRECTORIES -----------------------------------------------------
# proj.shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
  subdir.proj <-  dubcorms:::proj.shorthand(species.abbr, regions, grid.size, year.range)
  dirs         <- dir_spec(dir.orig.data, dir.proj, subdir.proj) # create and/or specify directories for later use.
  # ensure all directories exist
  suppressWarnings(stopifnot(all(lapply(dirs, dir.exists))))

# Create Spatial Grid -----------------------------------------------------
grid <- make_spatial_grid(dir.out = dirs[['dir.spatial.out']],
                          overwrite=overwrite.grid,
                          states = gsub(x=toupper(states), pattern="-", replacement=""),
                          countries = countries,
                          hexagonal=hexagonal,
                          crs.target=crs.target
                          )

mapview::mapview(grid) # interactive, openstreetmap

# BBS Data ----------------------------------------------------------------
fns.bbs.in <-  list.files(dirs$dir.bbs.out, pattern = "bbs_obs.rds",recursive = TRUE, full.names = TRUE)
if(length(fns.bbs.in)>0 & !overwrite.bbs){bbs_obs <- readRDS(fns.bbs.in)}else{
  bbs.orig <- grab_bbs_data(overwrite=overwrite.bbs)
  bbs_obs  <- munge_bbs_data(bbs_list = bbs.orig , states = states,
               species=species,
               zero.fill = TRUE,
               observations.output = 'df', # do not change!
               year.range = year.range)
  bbs_obs <- dubcorms:::convert_cols(bbs_obs) # munge column names to mesh with eBird
  bbs_obs <- dubcorms:::match_col_names(bbs_obs) # munge column names to mesh with eBird
  saveRDS(bbs_obs, paste0(dirs$dir.bbs.out, "/bbs_obs.rds"))
}# end bbs data munging

# Overlay BBS and study area/sampling grid
fns.bbs.spat.in <-  list.files(dirs$dir.spatial.out, pattern = "bbs_spatial.rds", recursive = TRUE, full.names = TRUE)
if(length(fns.bbs.spat.in)>0 & !overwrite.bbs){bbs_spatial <- readRDS(fns.bbs.spat.in)}else{
  bbs_spatial <- make_bbs_spatial(bbs_obs,
                          cws.routes.dir = dirs$cws.routes.dir,
                          usgs.routes.dir = dirs$usgs.routes.dir,
                          plot.dir = dirs$dir.plots,
                          grid = grid,
                          overwrite = overwrite.bbs
                          )
  saveRDS(bbs, paste0(dirs$dir.spatial.out, "/bbs_spatial.rds"))
}#end bbs_spatial
plot(bbs["gridcellid"])

# eBIRD DATA --------------------------------------------------------------
fns.ebird.in <- list.files(dirs$dir.ebird.out, full.names=TRUE, recursive = TRUE)

fns.ebird    <- id_ebird_files(dir.ebird.in = dirs$dir.ebird.in,
                               dir.ebird.out =dirs$dir.ebird.out,
                                  mmyyyy = mmyyyy,
                                  species=species.abbr,
                                  states.ind=states
                               )
# Import and munge the desired files..
ebird <- munge_ebird_data(
        fns.ebird = fns.ebird,
        overwrite = overwrite.ebird,
        dir.ebird.out = dirs$dir.ebird.out,
        countries = countries,
        states = states,
        protocol = ebird.protocol,
        species = c(species, species.abbr),
        max.num.observers = max.num.observers,
        complete.only=complete.checklists.only,
        years=year.range
      )

# Create spatial ebird
ebird_spatial <- make_ebird_spatial(df=ebird,
                   crs.target = crs.target,
                   grid=grid,
                   overwrite=overwrite.ebird,
                   dir.out=dirs$dir.spatial.out
                   )

# MUNGE DATA FOR JAGS -----------------------------------------------------
cat("creating a list of objects for use in JAGS...this will take a few to many minutes")
jdat <- make_jags_list(dat=list(ebird_spatial, bbs_spatial, grid),
                       dir.out=dirs$dir.jags,
                       max.C.ebird = max.C.ebird,
                       scale.vars=scale.vars,
                       jagam.args = jagam.args
                       )


# END FUN -----------------------------------------------------------------
}#END FUNCTION
