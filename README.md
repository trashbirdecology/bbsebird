
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/trashbirdecology/dubcorms/workflows/R-CMD-check/badge.svg)](https://github.com/trashbirdecology/dubcorms/actions)
<!-- badges: end -->

# dubcorms

The purpose of this R package (*likely to undergo a name change…*) is
three-fold:

1.  provide a (currently) faster alternative to the R package `auk` for
    importing and munging the large eBird datasets (\*see note)
2.  integrate the BBS and eBird observation datasets for use in JAGS
    (`rjags`) and `mcgv::jagam()`
3.  run fully Bayesian integrated population models using said data

\*Note: GH users [@cboettig](https://github.com/cboettig/) and
[@amstrimas](https://github.com/amstrimas/) are currently developing an
`auk` alternative, [`birddb`](https://github.com/cboettig/birddb/). It
is likely that, once stable, this R package will use `birddb` for eBird
import/manipulation.

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

## Quick Start

As mentioned above, this package is in early development stages and
functions are subject to change.

``` r
# devtools::install_github("trashbirdecology/dubcorms")
library(dubcorms)
```

Current exported functions:

``` r
lsf.str("package:dubcorms")[2:22]
#>  [1] "eval_params"             "id_ebird_files"         
#>  [3] "import_jdat"             "junk_it"                
#>  [5] "make_array"              "make_bbs_spatial"       
#>  [7] "make_ebird_spatial"      "make_gam_dat"           
#>  [9] "make_inits_list"         "make_jags_list"         
#> [11] "make_mat"                "make_spatial_grid"      
#> [13] "munge_date_time"         "munge_ebird_data"       
#> [15] "my_big_fucking_function" "scan_files"             
#> [17] "set_mcmc_specs"          "zerofill_ebird"         
#> [19] NA                        NA                       
#> [21] NA
```

# 0: Setup

``` r
# 0:Setup -----------------------------------------------------------------
devtools::install_github("trashbirdecology/dubcorms",
                         force = FALSE, 
                         dependencies = TRUE)
#> Skipping install of 'dubcorms' from a github remote, the SHA1 (db20603b) has not changed since last install.
#>   Use `force = TRUE` to force installation
#explicitly load some packages
pkgs <- c("dubcorms",
          "bbsAssistant",
          # "mapview",
          "reshape2",
          "stringr",
          "dplyr",
          "sf")
# install.packages("mapview")
invisible(lapply(pkgs, library, character.only = TRUE))
#> TERMS OF USE: North American Breeding Bird Survey Data: Users of these BBS data are obligated
#>             to formally recognize their use of the program's data in publications, presentations and other
#>             outlets. Additionally, all work using these data should acknowledge the thousands of U.S. and
#>             Canadian participants who annually perform  and coordinate the survey. It is in the best interest
#>             for the continued success of the BBS that authors submit a reprint or pdf of their work featuring
#>             BBS data to the National BBS staff for inclusion in the program's bibliography. If a publication
#>             is based solely on the analysis of BBS data, we recommend that you involve National BBS staff with
#>             the writing and/or review of the manuscript. DATA LIABILITY DISCLAIMER: North American Breeding
#>             Bird Survey Data.This database, identified as the North American Breeding Bird Survey Dataset,
#>             has been approved for release and publication by the U.S. Geological Survey (USGS) and the
#>             Canadian Wildlife Service of Environment Canada (EC). Although this database has been subjected
#>             to rigorous review and is substantially complete, the USGS and EC  reserve the right to revise
#>             the data pursuant to further analysis and review. Furthermore, it is released on the condition
#>             that the USGS, the U.S. Government, the EC, and the Canadian Government may not be held liable
#>             for any damages resulting from its authorized or unauthorized use.
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1
rm(pkgs)
```

``` r
# REQUIRED ARGUMENTS
dir.orig.data  = "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/"
# OPTIONAL ARGUMENTS
#general arguments
dir.proj  = "C:/Users/jburnett/documents/github/dubcorms-dev-scene/"
# dir.proj     = NULL # project directory. If NULL will specify and create a project directory within the current working directory. A single primary directory is made for each species within which new directories comprise combinations of years/spatial extent/etc. are housed.
states              = c("us-co", "us-ne", "us-wy", "us-SD")
species             = c("house sparrow", "passer domesticus")
species.abbr        = c("houspa", "HOSP") # call all the variations especially those that appear int eh EBIRD dwnloaded files
# species             = c("Double-crested Cormorant", "Nannopterum auritum", "phalacrocorax auritum")
# species.abbr        = c("DCCO", "doccor") # call all the variations especially those that appear int eh 
countries           = c("US", "CA") ## string of  countries Call \code{dubcorms::iso.codes} to find relevant codes for Countries and States/Prov/Territories.
# states  = c("us-mi",
#             "us-oh",
#             "us-wi",
#             "us-il",
#             "us-in")
# states              = c(paste0("us-",c("nc", "mi","sc", "fl", "ga", "al","md","de","va","wv","tn","pa","ny","ms","ky","oh","in","il", "wi")), "ca-on")
# states       = c("usfl", "us-ga" ) ## string of  states/provinces/territories. Call \code{dubcorms::iso.codes} to find relevant codes for Countries and States/Prov/Territories.
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
```

``` r
temp=c("complete.checklists.only", "scale.vars", 'overwrite.ebird',"remove.bbs.obs" ,"overwrite.bbs", "hexagonal", "get.sunlight")
for(i in seq_along(temp))assertthat::assert_that(is.logical(eval(parse(text=temp[i]))), msg = paste("argument ", temp[i],"must be a logical."))
temp=c("min.yday", "max.yday", "max.effort.km", "max.effort.mins", "max.C.ebird",
       "grid.size", "crs.target","year.range")
for(i in seq_along(temp)){assertthat::assert_that(class(eval(parse(text = temp[i]))) %in% c("integer", "numeric"),
                                                  msg = paste("argument ", temp[i], "must be a logical."))}
rm(temp)
## Munge the states and countries indexes for use in dir/proj dir reation
if(!exists("states")) states <- NULL
if(!is.null(states)){regions <- states}else{regions <- countries}
```

``` r
# proj.shorthand: this will make all directories within a new dir in dir.proj. this is useful for iterating over species/time/space and saving all resulting information in those directories.
subdir.proj <-  dubcorms:::proj.shorthand(species.abbr, regions, grid.size, year.range, max.C.ebird)
#> Warning in dubcorms:::proj.shorthand(species.abbr, regions, grid.size,
#> year.range, : multiple species indexes supplied. please check the project
#> directory naming to ensure it properly represents desired species.
if(nchar(subdir.proj)>100){cat("subdir.proj is very long. specifing a new name for project."); subdir.proj="myproject"}
dirs        <-  dir_spec(dir.orig.data, dir.proj, subdir.proj) # create and/or specify directories for later use.
#> Project directory output files will go to  C:/Users/jburnett/documents/github/dubcorms-dev-scene/houspa_USCO-USNE-USWY-USSD_111km_2008-2019_100maxCebird /n
# ensure all directories exist
suppressWarnings(stopifnot(all(lapply(dirs, dir.exists))))
```

# 1: Make Data

``` r
if(is.null(states)){ states.ind <- NULL}else{states.ind<-gsub(x=toupper(states), pattern="-", replacement="")}
grid <- make_spatial_grid(dir.out = dirs[['dir.spatial.out']],
                          overwrite=overwrite.grid,
                          states = states.ind,
                          countries = countries,
                          hexagonal=hexagonal,
                          crs.target=crs.target,
                          grid.size=grid.size
                          )
# plot(grid)
# mapview::mapview(grid)
```

Make BBS data.

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

# Overlay BBS and study area/sampling grid
  bbs_spatial <- make_bbs_spatial(bbs_obs,
                          cws.routes.dir = dirs$cws.routes.dir,
                          usgs.routes.dir = dirs$usgs.routes.dir,
                          plot.dir = dirs$dir.plots,
                          crs.target = crs.target,
                          grid = grid,
                          dir.out = dirs$dir.spatial.out,
                          overwrite = overwrite.bbs
                          )
#> File  C:/Users/jburnett/documents/github/dubcorms-dev-scene/houspa_USCO-USNE-USWY-USSD_111km_2008-2019_100maxCebird/spatial/bbs_spatial.rds  exists and overwrite.ebird = FALSE. Importing spatial bbs data.
```

Make eBird data,

``` r
# fns.ebird.in <- list.files(dirs$dir.ebird.out,
#                             full.names = TRUE,
#                             recursive = TRUE)
(fns.ebird    <- id_ebird_files(
                        dir.ebird.in = dirs$dir.ebird.in,
                        dir.ebird.out = dirs$dir.ebird.out,
                        mmyyyy = mmyyyy,
                        species = species.abbr,
                        states.ind = states
))
#> [1] "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird/ebd_sampling_relNov-2021.txt.gz"                        
#> [2] "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird/ebd_ca_houspa_relnov-2021.txt"                          
#> [3] "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/ebird/ebd_us_houspa_relnov-2021.txt"                          
#> [4] "C:/Users/jburnett/documents/github/dubcorms-dev-scene/houspa_USCO-USNE-USWY-USSD_111km_2008-2019_100maxCebird/ebird/ebird_filtered.rds"
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
#> File  C:/Users/jburnett/documents/github/dubcorms-dev-scene/houspa_USCO-USNE-USWY-USSD_111km_2008-2019_100maxCebird/ebird/ebird_filtered.rds exists. Importing. If you need to re-create the ebird data, specify overwrite=FALSE in my_big_fkn_fun().

# Create spatial ebird
ebird_spatial <- make_ebird_spatial(df=ebird,
                   crs.target = crs.target,
                   grid=grid,
                   overwrite=overwrite.ebird,
                   dir.out=dirs$dir.spatial.out
                   )
#> File  C:/Users/jburnett/documents/github/dubcorms-dev-scene/houspa_USCO-USNE-USWY-USSD_111km_2008-2019_100maxCebird/spatial/ebird_spatial.rds  exists and overwrite.ebird = FALSE. Importing spatial ebird data.
```

# 2: Make JAGS Data List

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
#> File  c:/users/jburnett/documents/github/dubcorms-dev-scene/houspa_usco-usne-uswy-ussd_111km_2008-2019_100maxcebird/jags//jdat.rds  exists and `overwrite`== FALSE. Importing from file.
```

# 3: Specify Model(s)

## BBS Only

``` r
mod <- "model{
####################################################
####################################################
# Likelihoods
####################################################
for(t in 1:nyears.b){
  for(s in 1:nsites.b){
    C.b[s,t] ~ dpois(lambda[s])
  } # end bbs data model s
} # end bbs data model t
  
for(s in 1:nsites.b){
  lambda[s]  = inprod(nu[], prop.b[s,])
} # end s (lambda route)

for(g in 1:ngrids.b){
  log(nu[g]) = alpha_g + area.b[g]*beta_g
} # end g (nu)

####################################################
####################################################
# Priors
####################################################
beta_g    ~ dnorm(0,1)
alpha_g   ~ dnorm(0,1)
####################################################
####################################################
# Derived
####################################################
for(t in 1:nyears.b){
  N.b[t] <- sum(C.b[,t])
}
####################################################
####################################################
}"

## export model
# browseURL(mod.fn)
mod.name <- paste0(dirs$dir.models,"/bbs-pois-null") ## not sure why but when i knit the chunks outside this one it doesn't keep the params, so having trouble putting it up there.
mod.fn <- paste0(mod.name, ".txt") # we want to name it now so we can call in jags functions
sink(mod.fn)
cat(mod)
#> model{
#> ####################################################
#> ####################################################
#> # Likelihoods
#> ####################################################
#> for(t in 1:nyears.b){
#>   for(s in 1:nsites.b){
#>     C.b[s,t] ~ dpois(lambda[s])
#>   } # end bbs data model s
#> } # end bbs data model t
#>   
#> for(s in 1:nsites.b){
#>   lambda[s]  = inprod(nu[], prop.b[s,])
#> } # end s (lambda route)
#> 
#> for(g in 1:ngrids.b){
#>   log(nu[g]) = alpha_g + area.b[g]*beta_g
#> } # end g (nu)
#> 
#> ####################################################
#> ####################################################
#> # Priors
#> ####################################################
#> beta_g    ~ dnorm(0,1)
#> alpha_g   ~ dnorm(0,1)
#> ####################################################
#> ####################################################
#> # Derived
#> ####################################################
#> for(t in 1:nyears.b){
#>   N.b[t] <- sum(C.b[,t])
#> }
#> ####################################################
#> ####################################################
#> }
sink()
```

``` r
mod <- "model{
####################################################
####################################################
# Likelihoods
####################################################
for(t in 1:nyears.b){
  for(s in 1:nsites.b){
    C.b[s,t] ~ dpois(lambda[s]*pB[s,t])
  } # end data model s
} # end data model t

  
for(t in 1:nYearsB){
  for(s in 1:nsites.b){
    logit(pB[s,t]) <- alpha_pb + 
                        beta_pf * pf[s,t] + 
                        beta_pw * pw[s,t]
  } # end data model s
} # end data model t


for(s in 1:nsites.b){
  lambda[s]  = inprod(nu[], prop[s,])
} # end s (lambda route)


for(g in 1:nGridsB){
  log(nu[g]) = beta_g*area[g] + alpha_g 
} # end g (nu)

####################################################
####################################################
# Priors
####################################################
## Priors on BBS site(route)-level detection 
alpha_pb   ~ dnorm(0,0.1) # intercept on the BBS detection model
beta_pf   ~ dnorm(0,1) # observer's first year (on BBS or Route)
beta_pw   ~ dnorm(0,1) # wind
## Priors on BBS grid-level covariates
alpha_g    ~ dnorm(0,1) # intercept on the grid covariates model
beta_g    ~ dnorm(0,1) # grid-cell area (scaled)

####################################################
####################################################
# Derived
####################################################
for(t in 1:nYearsB){
  N.b[t] <- sum(C.b[,t])
}
####################################################
####################################################
}"
## export model
# browseURL(mod.fn)
mod.name <- paste0(dirs$dir.models,"/bbs-pois-with-p-covs") ## not sure why but when i knit the chunks outside this one it doesn't keep the params, so having trouble putting it up there.
mod.fn <- paste0(mod.name, ".txt") # we want to name it now so we can call in jags functions
sink(mod.fn)
cat(mod)
#> model{
#> ####################################################
#> ####################################################
#> # Likelihoods
#> ####################################################
#> for(t in 1:nyears.b){
#>   for(s in 1:nsites.b){
#>     C.b[s,t] ~ dpois(lambda[s]*pB[s,t])
#>   } # end data model s
#> } # end data model t
#> 
#>   
#> for(t in 1:nYearsB){
#>   for(s in 1:nsites.b){
#>     logit(pB[s,t]) <- alpha_pb + 
#>                         beta_pf * pf[s,t] + 
#>                         beta_pw * pw[s,t]
#>   } # end data model s
#> } # end data model t
#> 
#> 
#> for(s in 1:nsites.b){
#>   lambda[s]  = inprod(nu[], prop[s,])
#> } # end s (lambda route)
#> 
#> 
#> for(g in 1:nGridsB){
#>   log(nu[g]) = beta_g*area[g] + alpha_g 
#> } # end g (nu)
#> 
#> ####################################################
#> ####################################################
#> # Priors
#> ####################################################
#> ## Priors on BBS site(route)-level detection 
#> alpha_pb   ~ dnorm(0,0.1) # intercept on the BBS detection model
#> beta_pf   ~ dnorm(0,1) # observer's first year (on BBS or Route)
#> beta_pw   ~ dnorm(0,1) # wind
#> ## Priors on BBS grid-level covariates
#> alpha_g    ~ dnorm(0,1) # intercept on the grid covariates model
#> beta_g    ~ dnorm(0,1) # grid-cell area (scaled)
#> 
#> ####################################################
#> ####################################################
#> # Derived
#> ####################################################
#> for(t in 1:nYearsB){
#>   N.b[t] <- sum(C.b[,t])
#> }
#> ####################################################
#> ####################################################
#> }
sink()
```

## BBS + eBird

``` r
mod <- "model{
####################################################
### Attempt to incorporate Ce (ebird count processes) in 
####################################################
# Likelihoods
####################################################
for(t in 1:nyears.b){
  for(s in 1:nsites.b){
    C.b[s,t] ~ dpois(lambda[s]*pB[s,t])
  } # end data model s
} # end data model t

  
for(t in 1:nyears.b){
  for(s in 1:nsites.b){
    logit(pB[s,t]) <- alpha_pb + 
                        beta_pf * pf[s,t] + 
                        beta_pw * pw[s,t]
  } # end data model s
} # end data model t


for(s in 1:nSitesB){
  lambda[s]  = inprod(nu[], prop[s,])
} # end s (lambda route)


for(g in 1:nGridsB){
  log(nu[g]) = beta_g*area[g] + alpha_g 
} # end g (nu)

####################################################
####################################################
# Priors
####################################################
## Priors on BBS site(route)-level detection 
alpha_pb   ~ dnorm(0,0.1) # intercept on the BBS detection model
beta_pf   ~ dnorm(0,1) # observer's first year (on BBS or Route)
beta_pw   ~ dnorm(0,1) # wind
## Priors on BBS grid-level covariates
alpha_g    ~ dnorm(0,1) # intercept on the grid covariates model
beta_g    ~ dnorm(0,1) # grid-cell area (scaled)

####################################################
####################################################
# Derived
####################################################
for(t in 1:nyears.b){
  N.b[t] <- sum(C.b[,t])
}
####################################################
####################################################
}"
## export model
# browseURL(mod.fn)
mod.name <- paste0(dirs$dir.models,"/bbs-ebird-pois-with-p-covs") ## not sure why but when i knit the chunks outside this one it doesn't keep the params, so having trouble putting it up there.
mod.fn <- paste0(mod.name, ".txt") # we want to name it now so we can call in jags functions
sink(mod.fn)
cat(mod)
#> model{
#> ####################################################
#> ### Attempt to incorporate Ce (ebird count processes) in 
#> ####################################################
#> # Likelihoods
#> ####################################################
#> for(t in 1:nyears.b){
#>   for(s in 1:nsites.b){
#>     C.b[s,t] ~ dpois(lambda[s]*pB[s,t])
#>   } # end data model s
#> } # end data model t
#> 
#>   
#> for(t in 1:nyears.b){
#>   for(s in 1:nsites.b){
#>     logit(pB[s,t]) <- alpha_pb + 
#>                         beta_pf * pf[s,t] + 
#>                         beta_pw * pw[s,t]
#>   } # end data model s
#> } # end data model t
#> 
#> 
#> for(s in 1:nSitesB){
#>   lambda[s]  = inprod(nu[], prop[s,])
#> } # end s (lambda route)
#> 
#> 
#> for(g in 1:nGridsB){
#>   log(nu[g]) = beta_g*area[g] + alpha_g 
#> } # end g (nu)
#> 
#> ####################################################
#> ####################################################
#> # Priors
#> ####################################################
#> ## Priors on BBS site(route)-level detection 
#> alpha_pb   ~ dnorm(0,0.1) # intercept on the BBS detection model
#> beta_pf   ~ dnorm(0,1) # observer's first year (on BBS or Route)
#> beta_pw   ~ dnorm(0,1) # wind
#> ## Priors on BBS grid-level covariates
#> alpha_g    ~ dnorm(0,1) # intercept on the grid covariates model
#> beta_g    ~ dnorm(0,1) # grid-cell area (scaled)
#> 
#> ####################################################
#> ####################################################
#> # Derived
#> ####################################################
#> for(t in 1:nyears.b){
#>   N.b[t] <- sum(C.b[,t])
#> }
#> ####################################################
#> ####################################################
#> }
sink()
```

# 4: Grab Only Necessary Data

``` r
jags.data <- list(
  ## COUNTS 
  C.b         = jdat$bbs$C,
  C.e         = jdat$ebird$C,
  ## INDEXES 
  ### bbs
  indexing.e    = jdat$bbs$idsYears,
  nyears.b    = jdat$bbs$nYears,
  nsites.b    = jdat$bbs$nSites,
  ngrids.b    = jdat$bbs$nGrids,
  gridid.b    = jdat$bbs$idsGrids, 
  # siteid.b    = jdat$bbs$idsSites,
  siteid.b    = as.numeric(jdat$bbs$idsSites),
  siteind.b   = jdat$bbs$idsSitesInd,
  ### ebird
  yearid.e    = jdat$ebird$idsYears,
  nyears.e    = jdat$ebird$nYears,
  ngrids.e    = jdat$ebird$nGrids,
  nsites.e    = jdat$ebird$nSites,
  gridid.e    = jdat$ebird$idsGrids, 
  # siteid.e    = jdat$ebird$idsSites,
  siteid.e    = as.numeric(jdat$ebird$idsSites),
  siteind.e   = jdat$ebird$idsSitesInd,
  ## DETECTION COVARIATES
  ### bbs
  p.b.a      = jdat$bbs$Xp$assistant, 
  p.b.c      = jdat$bbs$Xp$car, 
  p.b.fy     = jdat$bbs$Xp$fyrbbs, 
  p.b.n      = jdat$bbs$Xp$noise, 
  p.b.w      = jdat$bbs$Xp$wind, 
  ### ebird
  p.e.nobs   = jdat$ebird$Xp$nobs,
  p.e.nmins  = jdat$ebird$Xp$nmins,
  p.e.doy    = jdat$ebird$Xp$doy,
  ## GRID-LEVEL COVARIATES
  ### bbs-grids (only where bbs data exists..)
  area.b      = jdat$bbs$Xg$area, 
  coords.b    = jdat$bbs$Xg$XY,
  prop.b      = jdat$bbs$Xg$prop, 
  ### ebird-grids (only where ebird data exists)
  area.e      = jdat$ebird$Xg$area, 
  coords.e    = jdat$ebird$Xg$XY,
  ## GAM DATA
  X.gam       = jdat$gam$X, 
  Y.gam       = jdat$gam$Y, 
  N.gam       = jdat$gam$N
)
sort(names(jags.data))
#>  [1] "area.b"     "area.e"     "C.b"        "C.e"        "coords.b"  
#>  [6] "coords.e"   "gridid.b"   "gridid.e"   "indexing.e" "N.gam"     
#> [11] "ngrids.b"   "ngrids.e"   "nsites.b"   "nsites.e"   "nyears.b"  
#> [16] "nyears.e"   "p.b.a"      "p.b.c"      "p.b.fy"     "p.b.n"     
#> [21] "p.b.w"      "p.e.doy"    "p.e.nmins"  "p.e.nobs"   "prop.b"    
#> [26] "siteid.b"   "siteid.e"   "siteind.b"  "siteind.e"  "X.gam"     
#> [31] "Y.gam"      "yearid.e"
```

# 6: Inits and MCMC Specifications

## Inits + MCMC

``` r
mcmc <- set_mcmc_specs()
```

``` r
params.monitor <- c("lambda", "nu",  "N.b", "pB")
myinits <- list(alpha_pb  = rnorm(1,0,1), 
               alpha_g = rnorm(1, 0, 1), 
               beta_g = rnorm(1, 0, 1),
               beta_pb = rnorm(1,0,1), 
               beta_pw = rnorm(1,0,1), 
               beta_pf = rnorm(1,0,1))
inits <- make_inits_list(myinits, mcmc$nc)
```

# 7: Build Model(s)

``` r
dir.models <- dirs$dir.models
# models in jags model dir
mod.fns   <- list.files(dir.models, full.names = FALSE, pattern=".txt|.jags") # names of models
mod.names <- sub(".txt|.jags", "", x=mod.fns) # remove the .rds files
mod.fns   <- paste0(dir.models, "/", mod.fns) # tack on the directory to filename after grabbing the mod.names
```

``` r
overwrite.model.outputs = TRUE
# Choose the model to run
for(i in 1:1){
# for(i in seq_along(mod.fns)) {
  mod.fn   <- mod.fns[i]
  mod.name <- mod.names[i]
  fn.out <- paste0(dir.models, "/", mod.name, ".rds")
  mods.out <- if(i==1){fn.out}else{c(mods.out, fn.out)}
  
  if (file.exists(fn.out) &
        !overwrite.model.outputs)
    next(fn.out, " exists. skipping loop", i)
  # names(jdat)
  tictoc::tic()
  out <- jagsUI::jags(
    data  = jags.data,
    model.file = mod.fn,
    inits = inits,
    parameters.to.save = params.monitor,
    n.chains = mcmc$nc,
    n.thin = mcmc$nt,
    n.iter = mcmc$ni,
    n.burnin = mcmc$nb
  )
  x = tictoc::toc()
  mod.time <- paste0(round(x$toc - x$tic, 2), " seconds")
  out$tictoc.allchains <- mod.time
  # save model outputs
  saveRDS(out, file = fn.out)
}
```
