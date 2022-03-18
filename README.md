
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
dir.orig.data  = "C:/Users/jburnett/OneDrive - DOI/bbsebird-testing/" # this will be improved to be more intuitive re: what data? 
dir.proj       = "C:/users/jburnett/OneDrive - DOI/bbsebird-testing/House_Sparrow/"
species             = c("House Sparrow") ## eventually need to add alookup table to ensure species.abbr and speices align.
species.abbr        = c("houspa") # see ebird filename for abbreviation
### this needs improvement as well...e.g. a species lookup table to link common-speci-abbrev across BBS and eBird data...
##bbs arguments
usgs.layer          = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020" # name of the USGS BBS route shapefile to use
cws.layer           = "ALL_ROUTES" # name of the Canadian (CWS) BBS route shapefile.
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

## Munge the states and countries indexes for use in dir/proj dir reation
if(!exists("states")) states <- NULL
if(!is.null(states)){regions <- states}else{regions <- countries}
stopifnot(all(tolower(states) %in% tolower(bbsAssistant::region_codes$iso_3166_2)))
```

This chunk will create new environmental variables for project and data
directories based on the project directory and data directory specified
above.

``` r
# set_proj_shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
subdir.proj <-  set_proj_shorthand(species.abbr, regions, grid.size, year.range)
dirs        <-  dir_spec(dir.orig.data = dir.orig.data,  
                         dir.proj = dir.proj,
                         subdir.proj = subdir.proj) # create and/or specify 
```

## Step 2: Make Integrated Data

### Create a spatial sampling grid

The following chunk creates a spatial sampling grid of size grid.size
with units defaulting to the units of crs.target.

``` r
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

Create the BBS data. This chunk relieson R package . The resulting data
is aligned with the spatial grid (see above).

``` r
## if the files already exist, don't overwrite unless you've made changes to data specs
if("bbs_obs.rds" %in% list.files(dirs$dir.bbs.out)){bbs_obs <- readRDS(list.files(dirs$dir.bbs.out, "bbs_obs.rds", full.names=TRUE))}else{
bbs_orig <- bbsAssistant::grab_bbs_data(bbs_dir = dirs$dir.bbs.out) 
bbs_obs  <- bbsAssistant::munge_bbs_data(
    bbs_list = bbs_orig,
    states   = states,
    species = species, 
    year.range = year.range)
bbs_obs <- bbsebird:::match_col_names(bbs_obs) # munge column names to mesh with eBird
saveRDS(bbs_obs, paste0(dirs$dir.bbs.out, "/bbs_obs.rds")) # suggest saving data to file for easy access
}
# Overlay BBS and study area / sampling grid
### note, sometimes when running this in a notebook/rmd a random .rdf" path error occurs.
#### I have no clue what this bug is. Just try running it again. See also https://github.com/rstudio/rstudio/issues/6260
if("bbs_spatial.rds" %in% list.files(dirs$dir.bbs.out)){bbs_spatial <- readRDS(list.files(dirs$dir.bbs.out, "bbs_spatial.rds", full.names=TRUE))}else{
bbs_spatial <- make_bbs_spatial(
  df = bbs_obs,
  cws.routes.dir = dirs$cws.routes.dir,
  usgs.routes.dir = dirs$usgs.routes.dir,
  plot.dir = dirs$dir.plots,
  crs.target = crs.target,
  grid = study_area,
  dir.out = dirs$dir.spatial.out, 
  overwrite=TRUE
)
saveRDS(bbs_spatial, paste0(dirs$dir.bbs.out, "/bbs_spatial.rds"))
}
## check out the bbs spatial data to ensure things look ok
# plot(bbs_spatial['area']) # cell area
```

Munge the eBird data (must be saved to file):

``` r
## check the specified ebird original data directory for files. 
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
  # overwrite = FALSE, ## this function checks for exisiting, munged files iin dir.ebird.out..
  years = year.range
)

# Create spatial ebird
ebird_spatial <- make_ebird_spatial(
  df = ebird,
  crs.target = crs.target,
  grid = study_area,
  overwrite = FALSE, # this fun checks for existing spatial ebird file in dir.spatial.out
  dir.out = dirs$dir.spatial.out 
)
## visualizing the ebird_spatial data takes a while, do not recommend.
```

## Step 3: Bundle Data for Use in BUGS

Create a list of lists and indexes for use in JAGS or elsewhere. We
suggest creating a list using `make_bundle` and subsequently grabbing
useful data from there.

`make_bundle` creates site-level covariates in both long (vector) and
wide (matrix) form. Matrix form are housed inside Xsite matrix, whereas
long-form are within bbs.df and ebird.df.

``` r
message("[note] sometimes when running this chunk in notebook/rmarkdown it crashes. try restarting session or running interactively\n")

### make a teeny little bundle for model dev/debugging
bundle.dev <- make_bundle(
  bbs = bbs_spatial,
  ebird = ebird_spatial,
  grid = study_area,
  dev.mode = TRUE
)
## recommend saving to file in case you have crashes due to memory or modeling
saveRDS(bundle.dev, paste0(dirs$dir.proj,"/dev-bundle.rds"))

### make full sized bundle
bundle <- make_bundle(
  # data
  bbs = bbs_spatial,
  ebird = ebird_spatial,
  grid = study_area,
  # optional args
  dev.mode = FALSE
)
saveRDS(bundle, paste0(dirs$dir.proj,"/bundle.rds"))

# ### or read in from file...
# (bundle.fns <- list.files(paste0(dirs$dir.proj), pattern="bundle.rds", full.names = TRUE))
# bundle <- readRDS(bundle.fns[1])
```

# BAYESIAN HIERARCHICAL MODELING

Here, we provide one approach to creating, compiling, and . Additional
functionality for compiling models, MCMC samplers, and for sampling from
posterior distributions can be found in the R packages nimble and jagsUI
or rjags.

## Step 1: Specify Model

To facilitate modeling using either JAGS or Nimble, we recommend first
creating a text file of a standard BUGS model. However, to be compatible
within Nimble, the user should not provide any empty indexes (e.g.,
myMatrix\[,\] should be myMatrix\[1:N, 1:M\]).

<!-- We provide some model files in the package: -->
<!-- ```{r view.model} -->
<!-- ``` -->

Specify model as a BUGS model in R:

``` r
mod.fn <- paste0(dirs$dir.models, "/myBUGSmodel.txt")
sink(mod.fn)
cat(model.base, sep = "")
sink()
stopifnot(file.exists(mod.fn))
```

## Step 2: bugs.data

The output of `make_bundle()` will likely create more constants and data
than the user requires. Extract what is necessary for your model here.
You can unwrap this from a function, but the function makes it quicker
to switch between the dev and full bundled datasets.

``` r
make.bugs.data <- function(myBundleDat){
## see myBundleDat$bbs.df and myBundleDat$ebird.df for all data in a flat data object
bugs.data          = list(
  ## Center the years on some reference year
  ref.year  = round(median(1:myBundleDat$T)),
  nobsb     = nrow(myBundleDat$bbs.df),             # num obs. for bbs data
  nobse     = nrow(myBundleDat$ebird.df),           # num obs. for ebird data
  Cb        = myBundleDat$bbs.df$c,                 # bbs count data
  Ce        = myBundleDat$ebird.df$c,               # ebird count data
  Mb        = myBundleDat$Mb,                       # num sites bbs data
  Me        = myBundleDat$Me,                       # num sites ebird data
  G         = myBundleDat$G,                        # num grid cells total
  T         = myBundleDat$T,                        # num years total
  yearb     = myBundleDat$bbs.df$year.ind,          # vec of year index bbs data
  siteb     = myBundleDat$bbs.df$site.ind,          # vec of site index bbs data
  cellb     = myBundleDat$bbs.df$cell.ind,          # vec of grid cell index bbs data
  yeare     = myBundleDat$ebird.df$year.ind,        # vec of year index ebird data
  sitee     = myBundleDat$ebird.df$site.ind,        # vec of site index ebird data
  celle     = myBundleDat$ebird.df$cell.ind,        # vec of grid cell index ebird data
  nobsrb    = length(unique(myBundleDat$bbs.df$obs.ind)), # num unique observers bbs
  obsrb     = myBundleDat$bbs.df$obs.ind,           # observer identifier bbs data
  fyr       = myBundleDat$bbs.df$obsfirstyearbbs,   # first-year index bbs
  asst      = myBundleDat$bbs.df$assistant,         # assistant index bbs
  mins      = myBundleDat$ebird.df$duration_minutes,   # num mins surveyed ebird
  dist      = myBundleDat$ebird.df$effort_distance_km, # dist (km) traveled ebird
  effarea   = myBundleDat$ebird.df$effort_area_ha, # dist (km) traveled ebird
  party     = myBundleDat$ebird.df$number_observers,   # num observers ebird
  starttime = myBundleDat$ebird.df$time_observations_started_hsm, # time start ebird
  nknots    = myBundleDat$nbfs,                      # numb knots/basis functions
  gy        = as.matrix(myBundleDat$gy),             # index for all grid-year combos
  ngy       = myBundleDat$ngy,                       # length of grid-year combo mat (gy)
  prop      = as.matrix(myBundleDat$prop),           # proportion of route in grid (< G x T >)
  hab       = sqrt(abs(myBundleDat$Xgrid$area)),     # made-up hab variable 
  Z         = myBundleDat$Z.mat                      # Z-matrix (<G x nknots>)
)
return(bugs.data)
}
```

## Step 3: MCMC and Model Specs

First, specify parameters to monitor (based on your model):

``` r
##  refer to model for inits or params
# browseURL(mod.fn) # check out the model if you wish
params <- c("alpha1", #asst
            "alpha2", #fyrobs
            "beta1",   #hab
            "alpha11",  #party
            "alpha22",  #dist
            "alpha33",  #mins
            "alpha44",  #effarea
            "alpha55",  #starttime
            "yeareff", 
            "obseff",
            "Ntot" , 
            "lambda" 
)
```

Next, create a function for later specifying intiial values. Creating
this as function allows us to dynamically create intiail values for
whatever number of chains we wish to run and, importantly, creates
unique intial values for independent MCMC chains in parallel:

``` r
##  refer to model for inits or params
# browseURL(mod.fn) # check out the model if you wish
set.inits <- function(n = 1, dat = bugs.data) {
  inits.out <- list()
  for (i in 1:n) {
    inits.list <- list(
      alpha1        = rnorm(1, 0, 0.001), # bbs asst
      alpha2        = rnorm(1, 0, 0.001), # bbs fyr
      beta1         = rnorm(1, 0, 0.001), # hab
      alpha11       = rnorm(1, 0, 0.001), # num in party ebird
      alpha22       = rnorm(1, 0, 0.001), # dist travel ebird
      alpha33       = rnorm(1, 0, 0.001), # duration min ebird
      alpha44       = rnorm(1, 0, 0.001), # eff area  ebird
      alpha55       = rnorm(1, 0, 0.001), # starttime
      mu.b          = rnorm(1, 0, 0.001), # log expected N on lambda.route
      obseff        = rep(rnorm(dat$nobsb, 0, 0.001)),
      trend         = rep(rnorm(dat$G,     0, 0.001)),
      yeareff       = matrix(rep(rnorm(dat$G,     0, 0.001)),
                             nrow = dat$G,
                             ncol = dat$T)
      # .RNG.name = "base::Mersenne-Twister",
      # .RNG.seed = runif(1, 1, 2000)
    )
    inits.out[[i]] <- inits.list
    if (n == 1)
      return(inits.list)
    else{
      next()
    }
  }
  return(inits.out)
}
```

You can set the MCMC specs now or later. The function
`bbsebird::set_mcmc_specs()` allows user to create mcmc specification as
a list. Use `dev.mode=TRUE` to create mcmc specifications useful for
quickly testing/diagnosing models. This function can also be called
directly inside the `run_in_jags()` or `run_in_nimble()` funcitons.

``` r
# mcmc.specs <- set_mcmc_specs(dev.mode=TRUE)
```

## Step 4: Run Model(s)

### JAGS

``` r
bugs.dat  <- make.bugs.data(bundle.dev)
myMCMC.specs <- set_mcmc_specs(dev.mode=TRUE)
tictoc::tic("jags runtime")
results.jags <- run_in_jags(bugs.data = bugs.dat,
  parallel = TRUE,
  model=mod.fn,
  inits = set.inits(dat = bugs.dat, n=myMCMC.specs$nc), 
  mcmc.specs = myMCMC.specs,
  savedir = dirs$dir.results,
  monitor = params, 
  mod.name = "testrun"
)
tictoc::toc()
# plot(results.jags)
```

### Nimble

``` r
# unlike in JAGS, there cannot be stray INITS. 
# compiling the model takes a while in nimble...
# bugs.dat  <- make.bugs.data(bundle.dev)
# myMCMC.specs <- set_mcmc_specs(dev.mode=TRUE)
bugs.dat  <- make.bugs.data(bundle)
myMCMC.specs <- set_mcmc_specs(nc=3, ncores = 13)
# inits <- set.inits(n=1, dat=bugs.dat)
results.nimb <- run_in_nimble(
  myModel = mod.fn,
  myData = bugs.dat,
  myInits = set.inits(n=1, dat=bugs.dat),
  # myInits = inits,
  parallel = TRUE,
  mcmc.specs = myMCMC.specs,  
  monitor = params, 
  mod.name = "test-nimb")
saveRDS(
  results.nimb,
  paste0(
    dirs$dir.proj,"/jagsout/",
    Sys.Date(),
    "run-nimble",
    myMCMC.specs$ni,
    "ni_",
    myMCMC.specs$na,
    "na_",
    myMCMC.specs$nt,
    "nt_",
    myMCMC.specs$nb,
    "nb",
    ".RDS"
  )
)
# results.nimb[[1]]
# 
library(coda)
coda.samples <- as.mcmc(samples)
```

### STAN

``` r
# jags_to_stan # make fun to convert the JAGS to stan (prob already exists)
# run_in_stan
```
