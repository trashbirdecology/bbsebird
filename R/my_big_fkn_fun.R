my_big_fucking_function <- function(
  #general arguments
  dir.orig.data, # directory with subdirectories /bbs/ and /ebird/, within which the BBS route shapefiles and eBird data are stored. 
  dir.proj=NULL, # project directory. If NULL will specify and create a project directory within the current working directory. A single primary directory is made for each species within which new directories comprise combinations of years/spatial extent/etc. are housed. 
  species = c("DOCCOR,DOCCO,DCCO,DCCOR,Double-crested Cormorant,Double Crested Cormorant"),
  region = NULL,
  states= c("FL", "GA", "Florida", "Georgia"), 
  year.range = 2008:2019, 
  base.date = lubridate::ymd(paste0(min(year.range), c("-01-01"))), # used as base date for Julian dates. 
  crs.target = 4326, #target CRS for all created spatial layers
  get.sunlight = FALSE, #TRUE will caculate sunrise/light and moonrise/light times/durations. Only specify if data is needed as it takes a bit of time to run against the eBird data. 
  #GRID: arguments for creating a spatial grid
    grid.size = 1.00, # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)
    hexagonal = TRUE, 
  #BBS: arguments for filtering, downloading, and munging BBS data specifcally
    usgs.layer = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020", # name of the USGS BBS route shapefile to use
    cws.layer  = "ALL_ROUTES", 
    
  #EBIRD: arguments for filtering and munging the eBird data specifically
    remove.bbs.obs  = TRUE, # TRUE will attempt to remove BBS observations from the eBird database. This is currently a crude method.
    max.effort.km = "5",
    max.effort.mins = "180",
    complete.checklists.only = TRUE,
    ebird.protocol = c("Traveling", "Stationary"),
    min.yday = "91",
    max.yday = "245",
    mmyyyy = "sep-2021" # the month and year of the eBird data downloads on file
){


# PROJECT DIRECTORIES -----------------------------------------------------
# proj.shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
  subdir.proj <- proj.shorthand(species, states, grid.size, year.range)
  dirs <- dir_spec(dir.ebird.in, dir.proj, subdir.proj) # create series of directories for later use.
  list2env(dirs, env=.GlobalEnv)# bind the dir values as named global objects (MUST INCLUDE `env=.GlobalEnv`)
  rm(dirs)
  
  
  
  
  
}

args(make_spatial_grid)
# [1] "convert_cols"       "dir_spec"           "eval_params"        "filter_ebird_data"  "id_ebird_files"     "import_jdat"       
# [7] "junk_it"            "make_array"         "make_bbs_spatial"   "make_ebird_spatial" "make_gam_dat"       "make_inits_list"   
# [13] "make_jags_list"     "make_mat"           "make_spatial_grid"  "munge_bbs"          "munge_date_time"    "proj.shorthand"    
# [19] "scan_files"         "zerofill_ebird"  