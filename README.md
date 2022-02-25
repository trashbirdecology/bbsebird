
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/trashbirdecology/bbsebird/workflows/R-CMD-check/badge.svg)](https://github.com/trashbirdecology/bbsebird/actions)
<!-- badges: end -->

# bbsebird

The purpose of this R package (*likely to undergo a name change…*) is
to:

1.  provide a (currently) faster alternative to the R package `auk` for
    importing and munging the large eBird datasets\*
2.  integrate the BBS and eBird observation datasets for use in JAGS
    (`rjags`) and `mcgv::jagam()` by binding data to a common, spatial
    sampling grid

> \*[@cboettig](https://github.com/cboettig/) and
> [@amstrimas](https://github.com/amstrimas/) are currently developing
> an `auk` alternative, [`birddb`](https://github.com/cboettig/birddb/).
> It is likely that, once stable, this R package will use `birddb` as
> dependency for eBird import and manipulation. For now, however, the
> functions herein provide a much faster alternative to `auk`.

## Installation

Download development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("trashbirdecology/bbsassistant")
devtools::install_github("trashbirdecology/bbsebird")
```

## eBird Data Requirements

Prior to using this package, you must have downloaded eBird data. To
request and download eBird observations, visit [the eBird
website](https://ebird.org/data/download). Credentials are required, and
may take up to a few business days for approval, depending on use case.
For more information on the eBird data see the eBird website, or visit
[the repository for the Cornell Lab of Ornithology’s offical R package
for munging eBird data,
`auk`](https://github.com/CornellLabofOrnithology/auk/).

When your account is approved, you will gain access to the [eBird Basic
Database (EBD)](https://ebird.org/data/download/ebd). This package
requires two components of the EBD to be saved to local file:

1.  the **observations** (i.e. counts)
2.  the **sampling events** (i.e. information about the observation
    process)

# Runthrough

## Step 1: Setup

``` r
# 0:Setup -----------------------------------------------------------------
remotes::install_github("trashbirdecology/bbsebird")
#explicitly load some packages
pkgs <- c("bbsebird",
          "bbsAssistant",
          "reshape2",
          "stringr",
          "dplyr",
          "sf")
# install.packages("mapview")
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs)
```

If using this README, this is the only RMD chunk you shoudl have to
edit. Most important are where the eBird data and BBS shapefiles are
stored (dir.orig.data) and where you wish to save resulting data/models
(dir.proj). The latter need not exist – if it does not exist the package
will create the directory for you.

``` r
# REQUIRED ARGUMENTS
dir.orig.data  = "C:/Users/jburnett/OneDrive - DOI/bbsebird-testing/" # this will be improved to be more intuitive re: what data? 
dir.proj       = "C:/users/jburnett/OneDrive - DOI/bbsebird-testing/House_Sparrow/"
species             = c("House Sparrow") ## eventually need to add alookup table to ensure species.abbr and speices align.
species.abbr        = c("houspa") # see ebird filename for abbreviation
### this needs improvement as well...e.g. a species lookup table to link common-speci-abbrev across BBS and eBird data...
##bbs arguments
usgs.layer          = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020" # name of the USGS BBS route shapefile to use
cws.layer           = "ALL_ROUTES"
##ebird arguments
mmyyyy              = "dec-2021" # the month and year of the eBird data downloads on file

# Strongly suggested but optional args
##general arguments
# dir.proj  = "C:/Users/jburnett/desktop/testing/"


### see bbsAssistant::region_codes
states              = c("us-fl")
countries           = c("US") ## string of  countries Call \code{bbsebird::iso.codes} to find relevant codes for Countries and States/Prov/Territories.
# species             = c("Double-crested Cormorant", "Nannopterum auritum", "phalacrocorax auritum")
# species.abbr        = c("doccor","dcco", "docco")

year.range          = 2008:2019
base.julian.date    = lubridate::ymd(paste0(min(year.range), c("-01-01"))) # used as base date for Julian dates.
crs.target          = 4326 #target CRS for all created spatial layers

##grid arguments
grid.size           = 1.00 # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)

##ebird arguments
min.yday            = 91
max.yday            = 245

##JAGS: arguments for customizing the resulting JAGS data list
jagam.args          = list(bs="ds",k=20, family="poisson", sp.prior="log.uniform", diagonalize=TRUE)

## Munge the states and countries indexes for use in dir/proj dir reation
if(!exists("states")) states <- NULL
if(!is.null(states)){regions <- states}else{regions <- countries}
stopifnot(all(tolower(states) %in% tolower(bbsAssistant::region_codes$iso_3166_2)))
```

This chunk is not required, but is recommended to check that you’ve
correctly specified the arguments above.

``` r
# temp=c("complete.checklists.only", "scale.vars", 'overwrite.ebird',"remove.bbs.obs" ,"overwrite.bbs", "hexagonal", "get.sunlight")
# for(i in seq_along(temp))stopifnot(is.logical(eval(parse(text=temp[i]))))
# temp=c("min.yday", "max.yday", "max.effort.km", "max.effort.mins", "max.C.ebird",
#        "grid.size", "crs.target","year.range")
# for(i in seq_along(temp)){stopifnot(class(eval(parse(text = temp[i]))) %in% c("integer", "numeric"))}
# rm(temp)
```

This chunk will create new environmental variables for project adn data
directries based on teh directories supplied above.

``` r
# set_proj_shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
subdir.proj <-  set_proj_shorthand(species.abbr, regions, grid.size, year.range)
dirs        <-  dir_spec(dir.orig.data = dir.orig.data,  
                         dir.proj = dir.proj,
                         subdir.proj = subdir.proj) # create and/or specify directories for later use.
# ensure all directories exist
suppressWarnings(stopifnot(all(lapply(dirs, dir.exists))))
```

## Step 2: Make Integrated Data

### Create a spatial sampling grid

The following chunk creates a spatial sampling grid of size grid.size
with units defaulting to the units of crs.target.

``` r
tictoc::tic() # start timer to see how long it takes to create the ebird and bbs spatial files
if(is.null(states)){ states.ind <- NULL}else{states.ind<-gsub(x=toupper(states), pattern="-", replacement="")}
study_area <- make_spatial_grid(dir.out = dirs[['dir.spatial.out']],
                          # overwrite=overwrite.grid,
                          states = states.ind,
                          countries = countries,
                          crs.target = crs.target,
                          grid.size = grid.size
                          )
plot(study_area[1])
```

Create the BBS data. This chunk relies heabily on R package . The
resulting data is aligned with the spatial grid (see above).

``` r
## wrapper for creating all bbs data--debating making this an exported function. for now, DNE
fns.bbs.in <-
  list.files(
    dirs$dir.bbs.out,
    pattern = "bbs_obs.rds",
    recursive = TRUE,
    full.names = TRUE
  )
bbs_orig <- grab_bbs_data(bbs_dir = dirs$dir.bbs.out) ## need to add grab_bbs_data into munge_bbs_data and include an option for where to save that data. 
bbs_obs  <- munge_bbs_data(
    bbs_list = bbs_orig,
    states   = states,
    species = species, 
    year.range = year.range)
# bbs_obs <-
  # bbsebird:::match_col_names(bbs_obs) # munge column names to mesh with eBird
saveRDS(bbs_obs, paste0(dirs$dir.bbs.out, "/bbs_obs.rds"))

# Overlay BBS and study area / sampling grid
### note, sometimes when running this in a notebook/rmd i randomly get a .rdf path error. I have no clue what this bug is. Just try running it again. See : https://github.com/rstudio/rstudio/issues/6260
bbs_spatial <- make_bbs_spatial(
  df = bbs_obs,
  cws.routes.dir = dirs$cws.routes.dir,
  usgs.routes.dir = dirs$usgs.routes.dir,
  plot.dir = dirs$dir.plots,
  crs.target = crs.target,
  grid = study_area,
  dir.out = dirs$dir.spatial.out, 
  overwrite=FALSE
)
```

Munge the eBird data (must be saved to file):

``` r
## check the specified ebird directory for files. 
(fns.ebird    <- id_ebird_files(
  dir.ebird.in = dirs$dir.ebird.in,
  dir.ebird.out = dirs$dir.ebird.out,
  mmyyyy = mmyyyy,
  species = species.abbr,
  states.ind = states
))
stopifnot(length(fns.ebird) > 1)

# Import and munge the desired files
ebird <- munge_ebird_data(
  fns.ebird = fns.ebird,
  species = c(species, species.abbr),
  dir.ebird.out = dirs$dir.ebird.out,
  countries = countries,
  states = states,
  years = year.range
)

# Create spatial ebird
ebird_spatial <- make_ebird_spatial(
  df = ebird,
  crs.target = crs.target,
  grid = study_area,
  dir.out = dirs$dir.spatial.out
)
tictoc::toc()#~9 minutes to this point without package install for HOSP in Florida on a machine with 65G ram, 11th Gen Intel(R) Core(TM) i9-11950H @ 2.60GHz   2.61 GHz 64bit
```

## Step 3: Bundle Data for Use in JAGS/Elsewhere

Create a list of lists and indexes for use in JAGS or elsewhere. We
suggest creating a list using `bundle_data` and subsequently grabbing
useful data from there

``` r
tictoc::tic()
bundle <- bundle_data(bbs=bbs_spatial, 
                    ebird=ebird_spatial, 
                    grid=study_area,
                    scale.covs = TRUE)
tictoc::toc()

# names(bundle)
## complete data sets are stored in X.df
## site and grid-level covariates are stored in Xgrid, Xsite
## cell.index and year.index are lookup tables
## G = # grid cells
## T = # time/years
## Mb, Me = # sites for bbs (b) and ebird (e)
## GTb, GTe = grid-time lookup for samples in bbs and ebird
## Z = jagam model output
## nbfs = # of basis functions
## prop = proportion of BBS route in grid cells (dims: G x Mb)
```
