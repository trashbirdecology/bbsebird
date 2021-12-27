
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dubcorms

<!-- badges: start -->
<!-- badges: end -->

In development package for integrating eBird and BBS data.

## Installation

You can install the development version of dubcorms from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("trashbirdecology/dubcorms")
#> Using github PAT from envvar GITHUB_PAT
#> Skipping install of 'dubcorms' from a github remote, the SHA1 (3bf1e08a) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

``` r
require('here')
#> Loading required package: here
#> here() starts at C:/Users/jburnett/Documents/GitHub/dubcorms
require('kableExtra')
#> Loading required package: kableExtra
library(kableExtra)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter()     masks stats::filter()
#> x dplyr::group_rows() masks kableExtra::group_rows()
#> x dplyr::lag()        masks stats::lag()
library(jagsUI)
jdat<-readRDS(file=paste0(here::here("inst/jdat-jar.rds")))

meta <- read.csv("inst/data-meta-table.csv")
names(meta)[1] <-"dat"
```

# About Data Object `jdat`

The data object **jdat** is currently a list comprising the BBS and
eBird data for use in JAGS (SUBJECT TO CHANGE)

Important:: the BBS data in this list DO NOT include grid cells where no
BBS data exists. I.e., 191 is the # grid cells with BBS data, but total
number of grid cells in our study area is actually .

## Naming conventions:1. “C.xxx”:BBS counts

1.  “p.xxx”:detection / observation process covariates
2.  “n.xxx”:scalars indicating number of xxx
3.  “id.xxx”:unique identifiers for xxx
4.  others:others named to strive for recognizability (e.g., doy = day
    of year, julian = julian date with some origin)

## jdat contents

Here are the contents of jdat (for now, for bbs…)

``` r
# kableExtra::kable(names(jdat))
temp=meta %>% arrange(dat, name)
kable(temp) %>% 
  kableExtra::kable_minimal()
```

<table class=" lightable-minimal" style="font-family: &quot;Trebuchet MS&quot;, verdana, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
dat
</th>
<th style="text-align:left;">
name
</th>
<th style="text-align:left;">
class
</th>
<th style="text-align:left;">
description
</th>
<th style="text-align:left;">
nrow
</th>
<th style="text-align:left;">
ncol
</th>
<th style="text-align:left;">
nslice
</th>
<th style="text-align:left;">
length
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-6
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
doy.bbs
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
day of calendar year of BBS survey (origin=Jan 1 YYYY)
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
G.bbs
</td>
<td style="text-align:left;">
number
</td>
<td style="text-align:left;">
# unique grid cell ids with BBS data
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
id.gridcellswbbs
</td>
<td style="text-align:left;">
vector
</td>
<td style="text-align:left;">
sorted identifiers for grid cells with bbs data
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
G.bbs
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
n.routesperyear
</td>
<td style="text-align:left;">
vector
</td>
<td style="text-align:left;">
number bbs routes with data per year
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
T.bbs
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
p.cars
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
average # cars detected on route (Scaled)
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
p.noise
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
average noise indicated detected on route (Scaled)
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
p.obsfyearbbs
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
logical-indicator person’s first year on BBS (any routes); 0=FALSE
1=TRUE
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
p.obsfyearrteno
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
logical-indicator person’s first year on this particularly BBS route;
0=FALSE 1=TRUE
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
p.wind
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
average wind indicated on route (Scaled)
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
number
</td>
<td style="text-align:left;">
# unique BBS routes with data
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
T.bbs
</td>
<td style="text-align:left;">
number
</td>
<td style="text-align:left;">
# unique years with BBS data
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
w.area
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
area (m^2) for each grid cell
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
G
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs
</td>
<td style="text-align:left;">
w.prop
</td>
<td style="text-align:left;">
matrix
</td>
<td style="text-align:left;">
% of bbs route (row) within grid cell (col)
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
G.bbs
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs & ebird
</td>
<td style="text-align:left;">
id.gridcellswdata
</td>
<td style="text-align:left;">
vector
</td>
<td style="text-align:left;">
sorted identifiers for grid cells with any data
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
max(length(G.bbs), length(G.ebird))
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs & ebird
</td>
<td style="text-align:left;">
id.routessampled
</td>
<td style="text-align:left;">
vector
</td>
<td style="text-align:left;">
list of bbs routes with data (sorted)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
R
</td>
</tr>
<tr>
<td style="text-align:left;">
bbs & ebird
</td>
<td style="text-align:left;">
T
</td>
<td style="text-align:left;">
number
</td>
<td style="text-align:left;">
# unique years of all data (ebird and bbs)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
study area
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:left;">
number
</td>
<td style="text-align:left;">
# unique grid cells in study area (with or wihtout data)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
study area
</td>
<td style="text-align:left;">
id.gridcells
</td>
<td style="text-align:left;">
vector
</td>
<td style="text-align:left;">
sorted identifier for all grid cells
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
G
</td>
</tr>
</tbody>
</table>
