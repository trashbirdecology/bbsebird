
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
    importing and munging the large eBird datasets\*.
2.  integrate the BBS and eBird observation datasets for use in
    hierarchical modeling efforts by assigning point-referenced data to
    a gridded surface.
3.  provide modeling workflow for analysing spatio-temporal dynamics in
    Nimble or JAGS.

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
# remotes::install_github("trashbirdecology/bbsassistant") # to be safe
remotes::install_github("trashbirdecology/bbsebird")
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

# DATA MUNGING

## Step 1: Setup

``` r
#explicitly load some packages
pkgs <- c("bbsebird")
# install.packages("mapview") # you can use thsi package to get interactive map views..
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
dir.proj <- "C:/Users/jburnett/DOI/Royle, Andy - aaaaa"
dir.orig.data  = "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/" 
species             = c("house sparrow", "houspa") # include abbrev. assoc. wtih ebird files...
##bbs arguments
usgs.layer          = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020" # name of the USGS BBS route shapefile to use
cws.layer           = "ALL_ROUTES" # name of the Canadian (CWS) BBS route shapefile.

##ebird arguments
mmyyyy              = "dec-2021" # the month and year of the eBird data downloads on file
max.birds.checklist = 100 ## maximum number of eBird checklists to keep  per cell/year  (in bbsebird::make_bundle())

year.range          = 2008:2019
crs.target          = 4326 #target CRS for all created spatial layers
##grid arguments
grid.size           = 0.50 # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)
### default grid is in WGS84
##ebird arguments
ydays               = 91:245

# Region(s)
countries           = c("US") ## string of  countries Call /code{bbsebird::iso.codes} to find relevant
countries.iso3      = c("USA")
states    <- c("US-FL")

## Munge the states and countries indexes for use in dir/proj dir reation
if(!exists("states")) states <- NULL
if(!exists("countries")) countries <- NULL
if(!is.null(states)){regions <- states}else{regions <- countries}
stopifnot(all(tolower(states) %in% tolower(bbsAssistant::region_codes$iso_3166_2)))
```

This chunk will create new environmental variables for project and data
directories based on the project directory and data directory specified
above.

``` r
# set_proj_shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
subdir.proj <-
  set_proj_shorthand(
    species = species,
    countries = countries,
    states = states,
    grid.size = grid.size,
    years = year.range
  )

dirs        <-  dir_spec(dir.orig.data = dir.orig.data,  
                         dir.proj = dir.proj,
                         subdir.proj = subdir.proj) # create and/or specify dirs
```

## Step 2: Make Data

### Create a spatial sampling grid

The following chunk creates a spatial sampling grid of size grid.size
with units defaulting to the units of crs.target.

``` r
## entirity of North America takes a couple mins...even longer for <<<0.50 grid size
grid <-
  make_spatial_grid(
    dir.out = dirs$spatial,
    states = states,
    countries = countries,
    grid.size = grid.size,
    overwrite = FALSE
  )
overlay <- grid$grid.overlay
grid    <- grid$grid
par(mfrow=c(1,2))
plot(grid[2])    # just the underlying grid
plot(overlay[2]) # political boundaries
par(mfrow=c(1,1))
```

Create the BBS data. This chunk relies on R package . The resulting data
is aligned with the spatial grid (see above).

``` r
## if the files already exist, don't overwrite unless you've made changes to data specs
if("bbs.rds" %in% list.files(dirs$bbs.out)){
  cat("importing munged bbs data...\n")
  bbs  <- readRDS(list.files(dirs$bbs.out, "bbs.rds", full.names=TRUE))
}else{
  while(!exists("bbs_orig")) bbs_orig <- bbsAssistant::grab_bbs_data(bbs_dir = dirs$bbs.out) 
  bbs  <- bbsAssistant::munge_bbs_data(
    bbs_list = bbs_orig,
    states   = states,
    species = species, 
    year.range = years) |> as.data.table()
  bbs <- munge_col_names(bbs) # munge column names to mesh with eBird
  saveRDS(bbs, paste0(dirs$bbs.out, "/bbs.rds")) # suggest saving data to file for easy access
  rm(bbs_orig)
}
# Overlay BBS and study area / sampling grid
### note, sometimes when running this in a notebook/rmd a random .rdf" path error occurs.
#### I have no clue what this bug is. Just try running it again. See also https://github.com/rstudio/rstudio/issues/6260
bbs <- make_bbs_spatial(
  df = bbs,
  cws.routes.dir = dirs$cws.routes.dir,
  usgs.routes.dir = dirs$usgs.routes.dir,
  crs.target = crs.target,
  grid = grid,
  dir.out = dirs$bbs.out,
  overwrite = FALSE
)
# unique num obs per cell
# plot((bbs |> group_by(gridcellid) |> summarise(n_obs=n_distinct(obsn)))[2]) 
# unique num routes per cell
# plot((bbs |> group_by(gridcellid) |> summarise(n_rts=n_distinct(rteno)))[2])
```

Munge the eBird data (original data must be saved to file):

``` r
# This will tak ea while if not previously created or partitioned
ebird <-
  make_ebird(
    dir.ebird.in = dirs$dir.ebird.in,
    dir.out = dirs$ebird.out,
    mmyyyy = mmyyyy,
    countries = countries,
    species = species,
    states = states, 
    years = year.range, 
    overwrite = FALSE
  )
# Create spatial ebird
ebird <-
  make_ebird_spatial(
    ebird,
    dir.out = dirs$ebird.out,
    grid = grid,
    overwrite = FALSE
  )
## visualizing the ebird_spatial data takes a while, do not recommend...
```

## Step 3: Bundle (“Integrate”) Data for Use in BUGS

Create a list of lists and indexes for use in JAGS or elsewhere. We
suggest creating a list using `make_bundle` and subsequently grabbing
useful data from there.

``` r
message("[note] sometimes when running this chunk in notebook/rmarkdown it crashes. try restarting session or running interactively\n")
dat.full  <- make_bundle(bbs, ebird, grid)
gam.dat.full  <-
  make_gam(
    coords = dat.full$coords,
    scale.coords = TRUE, 
    ll.to.utm = TRUE, 
    # coords = cbind(dat.full$coords[,1], dat.full$coords[,2]),
    method = "cubic2d", #or "mgcv" 
    # nd = length(states)*3, ## num knots - arbitrary choice for now....
    num.nn = 10,
    nruns = 10,
    print.plot = TRUE,
    plot.main = "utm scaled"
  )

dat.full <- c(dat.full, gam.dat.full)

## Add Constants/Data Common to Models 
model.dat.full      <- make_model_data(data = dat.full)
```

`make_bundle` creates site-level covariates in both long (vector) and
wide (matrix) form. Matrix form are housed inside Xsite matrix, whereas
long-form are within bbs.df and ebird.df.

Export Data for Use in Nimble/JAGS

``` r
dir.create(paste0(dirs$project, "/datain"), showWarnings = FALSE)
saveRDS(dat.full, file=paste0(dirs$project, "/datain/bundle.rds"))
saveRDS(model.dat.full, file=paste0(dirs$project, "/datain/model-dat.rds"))
```

Also recommend creating a very small version of the same data for
testing models:

``` r
dat.dev   <- make_bundle(bbs, ebird, grid, dev.mode = TRUE) # full data
gam.dat.dev  <-
  make_gam(
    coords = dat.dev$coords, 
    method = "cubic2d",
    scale.coords = TRUE, 
    ll.to.utm = TRUE,
    nd = 5,
    num.nn = 10,
    nruns = 10,
    max.loop = 4,
    print.plot = TRUE,
    plot.main = "utm scaled dev mode"
  )
```

``` r
dat.dev  <- c(dat.dev, gam.dat.dev)
model.dat.dev       <- make_model_data(data = dat.dev)
saveRDS(dat.dev,  file=paste0(dirs$project, "/datain/bundle-dev.rds"))
saveRDS(model.dat.dev, file=paste0(dirs$project, "/datain/model-dat-dev.rds"))
```

# MODELS

``` r
# suggest clearing up mem
rm(list=setdiff(ls(), c("dirs","dir.proj", "grid.size", "species", "regions","year.range")))
library(bbsebird); library(nimble)
if(Sys.info()[1] == "Linux") setwd("/home/jburnett/integratedmodels/")

# IMportant specifications:
dev.mode = TRUE # want to go into development mode (use smaller data??)
overwrite <- FALSE # want to overwrite existing results if exists?
```

Define the directory for the desiored project (species, region, years,
grid size):

``` r
dir.plots  <- paste0(dirs$project, "/plots/")
dir.samps  <- paste0(dirs$project, "/samps/")
lapply(list(dir.plots, dir.samps), dir.create,  showWarnings = FALSE, recursive = TRUE)
```

## Step A: Import Data

``` r
if(dev.mode){pattern="model-dat-dev.rds"}else{pattern="model-dat.rds"}
(model.dat.fn <-
   list.files(
     dirs$project,
     pattern = pattern,
     recursive = TRUE,
     full.names = TRUE
   ))
model.dat <- readRDS(model.dat.fn) 
```

## Step B: MCMC and Model Specs

``` r
if(dev.mode) parallel = FALSE; ni <- 1200; nt <- 1; nb <- 500; nc <- 1; ncores <- nc
if(!dev.mode) parallel = FALSE; ni <- 25e4; nt <- 100; nb <- 7500; nc <-1; ncores <- nc

block.samp.type <- "AF_slice" # specify block sampler type
# number samples: 
round((ni-nb)/nt, 0)
```

## Step C: Specify Model File, Make Data Lists

optional: test model on 1 iteration

``` r
# source model file and make data lists
model <- "bbs-ls-ebird-gam"
testmod <- ifelse(dev.mode, TRUE, FALSE) # specify TRUE to test the model; suggest using only in dev.mode (compiling takes a long time...)

# source(paste0(system.file(package="bbsebird"), "/inst/models/"), model,".R")
source(paste0(dir.proj, "/models/", model, ".R"))
```

Sourcing the model file creates objects:

``` r
str(data)
str(constants)
str(mod.name)
str(inits)
# code
```

Specify some filenames (messy yes but for now it is what it is)

``` r
  fn <- paste0(mod.name, "_",
               "parallel=", 
               parallel, "_",
               ni,"iters_",
               nc,"chains_",
               nb,"nb_",
               nt, "nt_",
               constants$K, "bfs"
  )
  fn.samps <- paste0(dir.samps, fn, ".rds")
  fn.monhat <- paste0(dir.samps, fn, "_monitorhats.rds")
  fn.hatplot <- paste0(dir.samps, fn, "_hats.pdf")
  fn.gif <- paste0(dir.plots, fn, "_animation.gif")
  fn.trace <- paste0(dir.plots, fn, "_trace.pdf")
  fn.ey    <- paste0(dir.plots, fn, "_Ey.pdf")
  fn.cat    <- paste0(dir.plots, fn, "_cat.pdf")
  fn.ggmcmc    <- paste0(dir.plots, fn, "_ggmcmc.pdf")
```

## Step D: Run Model

``` r
if(!overwrite & file.exists(fn.samps)){
  cat("file", fn.samps, "exists and overwrite=FALSE. Importing samples. \n")
  results <- readRDS(fn.samps)
}else{
  t1 <- Sys.time()
  results <- run_nimble_model(
    code = code,
    data = data,
    constants = constants,
    inits = inits,
    monitors = monitors,
    parallel=parallel,
    ni=ni,
    nb=nb,
    nt=nt,
    nc=1,
    ncores = ncores, 
    dir.out = dir.proj, 
    mod.name = mod.name
  )
  (t2=Sys.time()-t1)
}
```

## Step E: Quick Viz

``` r
# traceplot
{pdf(fn.trace)
  par(mfrow=c(4,4))
  coda::traceplot(results)
  dev.off()
}
{if(is.list(results)) chain <- results[[1]] else chain <- results

# caterpillar plots of ALL monitors...  
pdf(fn.cat)
for(i in seq_along(monitors)){
mcmcplots::caterplot(chain, monitors[i], greek = TRUE)
}
dev.off()
}


browseURL(fn.trace)
browseURL(fn.cat)
```
