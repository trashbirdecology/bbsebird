
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

## Step 1: Load Packages

``` r
remotes::install_github("trashbirdecology/bbsAssistant",
                        force = FALSE,
                        upgrade = "never") ## "never" for dev purposes... to avoid interactivity
remotes::install_github("trashbirdecology/bbsebird",
                        force = FALSE,
                        upgrade = "never") ## "never" for dev purposes... to avoid interactivity
pkgs <- c("bbsebird")
lapply(pkgs, require, character.only = TRUE, quietly = TRUE)
rm(pkgs)
```

If using this README, this is the only RMD chunk you shoudl have to
edit. Most important are where the eBird data and BBS shapefiles are
stored (dir.orig.data) and where you wish to save resulting data/models
(dir.proj). The latter need not exist – if it does not exist the package
will create the directory for you.

``` r
crs.target          = 4326 #target CRS for all created spatial layers
grid.size           = 0.5 # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)
species             = c("wood thrush", "woothr")
mmyyyy              = "feb-2022" # the month and year of the eBird data downloads on file
years               = 2008:2019
countries           = c("US") ## string of  countries Call /code{bbsebird::iso.codes} to find relevant
states              = c("us-wv")
ydays               = 91:245
max.birds.checklist = 55 ## maximum number of birds within a single eBird checklist (removes any over
max.checklists      = 10 
```

This chunk will create new environmental variables for project and data
directories based on the project directory and data directory specified
above.

``` r
# set_proj_shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
if (!is.na(pmatch("C:/Users/aroyle", getwd()))){
  setwd("C:/users/aroyle/OneDrive - DOI/AAAA-IntegratedModels")}else{
if (!is.na(pmatch("C:/Users/jburnett", getwd()))){ # use na because if FALSE does not return as logical..
  if(Sys.info()[1] == "Linux") {dir.proj <- "/home/jburnett/integratedmodels/"}
  if(Sys.info()[4] == "IGSACEESLTAROYB") #JLB's SAS comp
    dir.proj <- "C:/Users/jburnett/DOI/Royle, Andy - aaaaa/"
  if(Sys.info()[4] == "IGSACEESWSWLIN8") # Bill Link's Comp
    dir.proj <- "C:/Users/jburnett/DOI/Royle, Andy - AAAA-IntegratedModels/"
}}
dir.orig.data   <-  paste0(dir.proj,"/dataorig")  # ebird data + bbs routes
## subdir.proj is optional but recommended when creating data/models for different species, regions, and/or grid cell sizes...
subdir.proj <-
  set_proj_shorthand(
    species = species,
    countries = countries,
    states = states,
    grid.size = grid.size,
    years = years
  )
## if ebird and/or bbs route files don't exist in dir.orig.data this will throw a warning. 
dirs        <-  dir_spec(dir.orig.data = dir.orig.data,  
                         dir.proj = dir.proj,
                         subdir.proj = subdir.proj) 
## returns a list of directories
names(dirs)
```

## Step 2: Make Data

### Create a spatial sampling grid

The following chunk creates a spatial sampling grid of size grid.size
with units defaulting to the units of crs.target.

``` r
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
plot(grid[2])    # just the underlying grid
plot(overlay[2]) # cells clipped to political boundaries
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
plot((bbs |> group_by(gridcellid) |> summarise(n_obs=n_distinct(obsn)))[2])
# unique num routes per cell
plot((bbs |> group_by(gridcellid) |> summarise(n_rts=n_distinct(rteno)))[2])
# max count per cell
plot((bbs |> group_by(gridcellid) |> summarise(c_max=max(c, na.rm=TRUE)))[2])
```

Munge the eBird data (original data must be saved to file):

``` r
ebird <-
  make_ebird(
    dir.ebird.in = dirs$ebird.in,
    dir.out = dirs$ebird.out,
    mmyyyy = mmyyyy,
    countries = countries,
    species = species,
    max.birds.checklist = max.birds.checklist, 
    states = states, 
    years = years, 
    overwrite = FALSE
  )

ebird <-
  make_ebird_spatial(
    ebird,
    dir.out = dirs$ebird.out,
    grid = grid,
    overwrite = FALSE
  )
## visualizing the ebird_spatial data takes a while, do not recommend!
```

## Step 3: Bundle (“Integrate”) Data for Use in BUGS

Create a list of lists and indexes for use in JAGS or elsewhere. We
suggest creating a list using `make_bundle` and subsequently grabbing
useful data from there.

``` r
dat.full  <- make_bundle(bbs, ebird, grid, max.ebird = max.birds.checklist)
### make_bunlde provides site-level covariates as both vectors and matrices
### e.g., dat.full$bbs.df$obsfirstyearbbs == dat.full$Xb$obsfirstyearbbs
gam.dat.full  <-
  make_gam(
    coords = dat.full$coords, 
    # coords = cbind(dat.full$coords[,1], dat.full$coords[,2]),
    method = "cubic2d")
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

You can also create a very small version of the same data for testing
models, but do not use the ‘dev’ data for inference!

``` r
dat.dev   <- make_bundle(bbs, ebird, grid, dev.mode = TRUE, max.ebird = max.birds.checklist) # full data
gam.dat.dev  <-
  make_gam(
    coords = dat.dev$coords, 
    method = "cubic2d",
    scale.coords = TRUE, 
    ll.to.utm = TRUE,
    nd = 2,
    # num.nn = 10,
    nruns = 10,
    max.loop = 4,
    print.plot = TRUE,
    plot.main = "utm scaled dev mode"
  )
# stopifnot(dat.dev$G < dat.full$G)
dat.dev  <- c(dat.dev, gam.dat.dev)
model.dat.dev       <- make_model_data(data = dat.dev)
saveRDS(dat.dev,  file=paste0(dirs$project, "/datain/bundle-dev.rds"))
saveRDS(model.dat.dev, file=paste0(dirs$project, "/datain/model-dat-dev.rds"))
```

``` r
dat.dev  <- c(dat.dev, gam.dat.dev)
model.dat.dev       <- make_model_data(data = dat.dev)
saveRDS(dat.dev,  file=paste0(dirs$project, "/datain/bundle-dev.rds"))
saveRDS(model.dat.dev, file=paste0(dirs$project, "/datain/model-dat-dev.rds"))
```

# Step 4: Nimble Models

``` r
# overwrite = TRUE ## TRUE will re-run any existing models...
dev.mode = FALSE # if TRUE, this will use reduced dataset AND run fewer iterations for quick testing...
if(dev.mode) model.dat <- model.dat.dev else model.dat <- model.dat.full
## let's just keep useful stuff
rm(list=setdiff(ls(), c("dev.mode", "model.dat", "dir.proj", "overwrite")))
library(bbsebird, quietly = TRUE) # load nimble
library(nimble,  quietly = TRUE) # load nimble
```

Set MCMC specs:

``` r
if(dev.mode)  parallel = FALSE; ni <- 100;  nt <- 1;  nb <- 500;  nc <- 1; ncores <- nc
if(!dev.mode) parallel = TRUE;  ni <- 25e4; nt <- 50; nb <- 3000; nc <-1;  ncores <- nc
cat(round((ni-nb)/nt, 0), "iterations will remain after thinning and burn-in\n")
```

If blocking is required, specify here:

``` r
block.samp.type <- "noblock" # or "AF_slice" or "RW_block"
```

This model file (currently private…) creates the Nimble code, data,
constants, initial values, etc…

``` r
source(paste0(dir.proj, "models/bbs-ls-ebird-gam.R"))
str(data)
str(constants)
str(inits)
sort(monitors)
```

Create and specify export directories and filepaths for saving MCMC
samples and plots:

``` r
fns <- set.filenames(mod.name = mod.name, dir.proj = dir.proj, ni = ni, nb=nb, nc = nc, nt = nt, nbfs = constants$K)
names(fns) # use them...or not ~shrug~
```

``` r
if(!overwrite & file.exists(fn.samps)){
  cat("file", fn.samps, "exists and overwrite is FALSE. Importing existing samples. \n")
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
    nc=nc,
    ncores = ncores, 
    dir.out = dir.proj,
    block.samp.type = block.samp.type, 
    mod.name = mod.name
  )
  (t2=Sys.time()-t1)
}
```
