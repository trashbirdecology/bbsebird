
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/trashbirdecology/dubcorms/workflows/R-CMD-check/badge.svg)](https://github.com/trashbirdecology/dubcorms/actions)
<!-- badges: end -->

# dubcorms

The purpose of this R package (*likely to undergo a name change…*) is
to:

1.  provide a (currently) faster alternative to the R package `auk` for
    importing and munging the large eBird datasets (\*see note)
2.  integrate the BBS and eBird observation datasets for use in JAGS
    (`rjags`) and `mcgv::jagam()`

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
devtools::install_github("trashbirdecology/dubcorms")
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
# devtools::install_github("trashbirdecology/dubcorms")
#explicitly load some packages
pkgs <- c("dubcorms",
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
dir.orig.data  = "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/"
# OPTIONAL ARGUMENTS
#general arguments
dir.proj  = "C:/Users/jburnett/documents/github/dubcorms-dev-scene/"
# dir.proj     = NULL # project directory. If NULL will specify and create a project directory within the current working directory. A single primary directory is made for each species within which new directories comprise combinations of years/spatial extent/etc. are housed.
states              = c("us-co", "us-ne", "us-ks","us-wy", "us-SD", 
                        "Us-MT", "US-ND")
species             = c("house sparrow", "passer domesticus")
species.abbr        = c("houspa", "HOSP") # call all the variations especially those that appear int eh EBIRD dwnloaded files
# species             = c("Double-crested Cormorant", "Nannopterum auritum", "phalacrocorax auritum")
# species.abbr        = c("DCCO", "doccor") # call all the variations especially those that appear int eh 
countries           = c("US", "CA") ## string of  countries Call \code{dubcorms::iso.codes} to find relevant codes for Countries and States/Prov/Territories.
year.range          = 2008:2019
base.julian.date    = lubridate::ymd(paste0(min(year.range), c("-01-01"))) # used as base date for Julian dates.
crs.target          = 4326 #target CRS for all created spatial layers
get.sunlight        = FALSE #TRUE will caculate sunrise/light and moonrise/light times/durations. Only specify if data is needed as it takes a bit of time to run against the eBird data.
#GRID: arguments for creating a spatial grid
grid.size           = 1.00 # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)
hexagonal           = TRUE # if FALSE will produce square grid cells against CRS.target.
overwrite.grid      = FALSE # logical FALSE will not overwrite the grid if one already exists in dir.proj
#BBS: arguments for filtering, downloading, and munging BBS data specifcally
usgs.layer          = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020" # name of the USGS BBS route shapefile to use
cws.layer           = "ALL_ROUTES"
overwrite.bbs       = FALSE
#EBIRD: arguments for filtering and munging the eBird data specifically
overwrite.ebird     = FALSE
max.C.ebird         = 100 # maximum number of birds of the select species counted in a single ebird checklist
remove.bbs.obs      = TRUE # TRUE will attempt to remove BBS observations from the eBird database. This is currently a crude method.
max.effort.km       = 5
max.num.observers   = 10
max.effort.mins     = 180
complete.checklists.only = TRUE
ebird.protocol      = c("Traveling", "Stationary")
min.yday            = 91
max.yday            = 245
mmyyyy              = "nov-2021" # the month and year of the eBird data downloads on file
#JAGS: arguments for customizing the resulting JAGS data list
jagam.args          = list(bs="ds",k=20, family="poisson", sp.prior="log.uniform", diagonalize=TRUE)
scale.vars          = TRUE # whether or not to z-scale select variables.
## Munge the states and countries indexes for use in dir/proj dir reation
if(!exists("states")) states <- NULL
if(!is.null(states)){regions <- states}else{regions <- countries}
```

This chunk is not required, but is recommended to check that you’ve
correctly specified the arguments above.

``` r
temp=c("complete.checklists.only", "scale.vars", 'overwrite.ebird',"remove.bbs.obs" ,"overwrite.bbs", "hexagonal", "get.sunlight")
for(i in seq_along(temp))stopifnot(is.logical(eval(parse(text=temp[i]))))
temp=c("min.yday", "max.yday", "max.effort.km", "max.effort.mins", "max.C.ebird",
       "grid.size", "crs.target","year.range")
for(i in seq_along(temp)){stopifnot(class(eval(parse(text = temp[i]))) %in% c("integer", "numeric"))}
rm(temp)
```

This chunk will create new environmental variables for project adn data
directries based on teh directories supplied above.

``` r
# proj.shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
subdir.proj <-  proj.shorthand(species.abbr, regions, grid.size, year.range, max.C.ebird)
if(nchar(subdir.proj)>100){cat("subdir.proj is very long. specifying a new name for project."); subdir.proj="myproject"}
dirs        <-  dir_spec(dir.orig.data, dir.proj, subdir.proj) # create and/or specify directories for later use.
# ensure all directories exist
suppressWarnings(stopifnot(all(lapply(dirs, dir.exists))))
```

## Step 2: Make Integrated Data

### Create a spatial sampling grid

``` r
if(is.null(states)){ states.ind <- NULL}else{states.ind<-gsub(x=toupper(states), pattern="-", replacement="")}
grid <- make_spatial_grid(dir.out = dirs[['dir.spatial.out']],
                          overwrite=overwrite.grid,
                          states = states.ind,
                          countries = countries,
                          crs.target=crs.target,
                          grid.size=grid.size
                          )
plot(grid)
```

Create the BBS data. This chunk relies heabily on R package . The
resulting data is aligned with the spatial grid (see above).

``` r
## wrapper for creating all bbs dafa
# bbs <- make_bbs_data()
fns.bbs.in <-  list.files(dirs$dir.bbs.out, pattern = "bbs_obs.rds",recursive = TRUE, full.names = TRUE)
if(length(fns.bbs.in)>0 & !overwrite.bbs){bbs_obs <- readRDS(fns.bbs.in)}else{
  bbs_orig <- grab_bbs_data(overwrite = overwrite.bbs,
                            bbs_dir = dirs$dir.bbs.out)
  bbs_obs  <- munge_bbs_data(
    bbs_list = bbs_orig,
    states   = states,
    species = species,
    zero.fill = TRUE,
    observations.output = 'df',
    # do not change!
    year.range = year.range
  )
  bbs_obs <-
    dubcorms:::match_col_names(bbs_obs) # munge column names to mesh with eBird
  saveRDS(bbs_obs, paste0(dirs$dir.bbs.out, "/bbs_obs.rds"))
}# end bbs data munging

# Overlay BBS and study area / sampling grid
### note, sometimes when running this in a notebook/rmd i randomly get a .rdf path error. I have no clue what this bug is. Just try running it again. See : https://github.com/rstudio/rstudio/issues/6260
bbs_spatial <- make_bbs_spatial(df = bbs_obs,
  cws.routes.dir = dirs$cws.routes.dir,
  usgs.routes.dir = dirs$usgs.routes.dir,
  plot.dir = dirs$dir.plots,
  crs.target = crs.target,
  grid = grid,
  dir.out = dirs$dir.spatial.out,
  overwrite = overwrite.bbs
)
```

Make eBird data,

``` r
(fns.ebird    <- id_ebird_files(
                        dir.ebird.in = dirs$dir.ebird.in,
                        dir.ebird.out = dirs$dir.ebird.out,
                        mmyyyy = mmyyyy,
                        species = species.abbr,
                        states.ind = states
))
stopifnot(length(fns.ebird)>1)

# Import and munge the desired files..
ebird <- munge_ebird_data(
  fns.ebird = fns.ebird,
  species = c(species, species.abbr),
  overwrite = overwrite.ebird,
  dir.ebird.out = dirs$dir.ebird.out,
  countries = countries,
  states = states,
  protocol = ebird.protocol,
  max.num.observers = max.num.observers,
  complete.only = complete.checklists.only,
  years = year.range
)

# Create spatial ebird
ebird_spatial <- make_ebird_spatial(df=ebird,
                   crs.target = crs.target,
                   grid=grid,
                   overwrite=overwrite.ebird,
                   dir.out=dirs$dir.spatial.out
                   )
```

## Step 3: Bundle Data for Use in JAGS/Elsewhere

``` r
jdat <- make_jags_list(
  dat = list(ebird=ebird_spatial, 
               bbs=bbs_spatial, grid=grid, dirs=dirs),
  dir.out = dirs$dir.jags,
  max.C.ebird = max.C.ebird,
  scale.vars = TRUE,
  jagam.args = list(
    bs = "ds",
    k = 20,
    family = "poisson",
    sp.prior = "log.uniform",
    diagonalize = TRUE)
)
```

``` r
cat(names(jdat), sep="\n")
```
