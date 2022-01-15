
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
#>  [1] "dir_spec"           "eval_params"        "filter_ebird_data" 
#>  [4] "get_data_structure" "id_ebird_files"     "import_jdat"       
#>  [7] "junk_it"            "make_array"         "make_bbs_spatial"  
#> [10] "make_ebird_spatial" "make_gam_dat"       "make_jags_list"    
#> [13] "make_mat"           "make_spatial_grid"  "munge_bbs"         
#> [16] "munge_date_time"    "proj.shorthand"     "scan_files"        
#> [19] "zerofill_ebird"     NA                   NA
```
