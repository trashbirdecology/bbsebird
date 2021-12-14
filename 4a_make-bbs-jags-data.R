if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
source("0_setup.r")
devtools::load_all()

# Create or Load in Post-spatial Munging BBS and eBird Data ----------------------------
fns <- c("bbs.rds", "ebird.rds", "grid.rds")

if(all(fns %in% list.files(dir.jags))){
  cat("importing the munged bbs, ebird and grid sf objects.")
  fns=list.files(dir.jags, full.names = TRUE)
  bbs <- readRDS(fns[str_detect(fns, "bbs")])
  grid <- readRDS(fns[str_detect(fns, "grid")])
  ebird <- readRDS(fns[str_detect(fns, "ebird")])
}else{
  cat("Building the BBS and eBird data and spatial objects. Sit back and relax......\nor be worried for errors halfway through")
  source("3_make-bird-data.R")
}


# JAGS Data-------------------------------------------------------------
## for my sanity just going to list things out to populate them later
jags.bbs <- list()

## Grid/study area ---------------------------------------------------------
XY <- cbind(grid$cell.lon.centroid, grid$cell.lat.centroid)
XY.scaled <- scale(XY)
nGridCells <- nrow(XY)


## BBS data ---------------------------------------------------------
##coerce the bbs data to a data frame.
bbs.df <- bbs %>% st_drop_geometry() %>%
  as.data.frame() %>%
  na.omit(rteno, year) %>%
  distinct(id, rteno, year, .keep_all=TRUE) %>%
  ### scale covariates
  mutate(
    wind.z = (avgwind - mean(avgwind, na.rm=TRUE))/sd(avgwind, na.rm=TRUE),
    noise.z = (noisemean - mean(noisemean, na.rm=TRUE))/sd(noisemean, na.rm=TRUE),
    car.z = (carmean - mean(carmean, na.rm=TRUE))/sd(carmean, na.rm=TRUE),
  )

## Create arrays for counts [ rteno ~ year ~ id(grid cell) ]
bbs.temp <- split(bbs.df %>% select(id, year, C, rteno), f = bbs.df$id)
### create list of data frames comprising count data
C <-lapply(1:length(bbs.temp),
                    function(x) (pivot_wider(bbs.temp[[x]],
                                             values_fill = NA,
                                             id_cols=rteno,
                                             names_from = year,
                                             values_from = "C"))

)
## coerce to array
# C <- C %>% as.array()
# ## make the first column of each dataframe the rownames
C <- lapply(1:length(C), function(x) (
  tibble::column_to_rownames(C[[x]], var = names(C[[x]][,1]))))

### add list element names
names(C) <- names(bbs.temp)
rm(bbs.temp)



# ## mean observed counts within each grid cell
# C.mean <- lapply(1:length(C), function(x)(round(colMeans(C[[x]], na.rm=TRUE))))
# N_i <- bind_rows(C.mean) %>% as.data.frame() # keep split into two parts, otherwise weird shit happens
# rownames(N_i) <- names(C)
#
# ## Create initial values for N for BBS
# N_i[is.na(N_i)] <- 0 ### shoudl I be doing this when there is no data?!
# for (i in 1:nrow(N_i)) {
#   for (t in 1:ncol(N_i)) {
#     if (N_i[i, t] > 0)
#       N_i[i, t] <- round(N_i[i, t] + (N_i[i, t] * 0.8)) ## why 0.8?
#   }
# }


# "Effort" ------------------------------------------------------------------
## Effort for BBS, here, will be the proportion of the route that falls within a given gridcell
bbs.temp <- bbs.df %>% distinct(id, year, rteno, proprouteincell) %>%
  mutate(proprouteincell=as.numeric(proprouteincell)) # drop the units

## Create arrays for counts [ rteno ~ year ~ id(grid cell) ]
bbs.temp <- split(bbs.temp , f = covars.det$id)

### create list of data frames comprising detefction covariates
effort <-lapply(1:length(bbs.temp),
           function(x) (pivot_wider(bbs.temp[[x]],
                                    values_fill = NA,
                                    id_cols=rteno,
                                    names_from = year,
                                    values_from = "proprouteincell"))

)


# ## make the first column of each dataframe the rownames
# effort <- lapply(1:length(effort), function(x) (
#   tibble::column_to_rownames(effort[[x]], var = names(effort[[x]][,1]))))

### add list element names
names(effort) <- names(bbs.temp)
rm(bbs.temp)




# # JAGAM -------------------------------------------------------------------
# # Generate the appropriate prior distributions for a poisson gam
# jagam.data <-
#   data.frame(
#     N = rep(N = N_i[, 1], length(XY[, 1])),
#     x = XY.scaled[,1],
#     y = XY.scaled[,2]
#   )
#
# jagam.mod <- mgcv::jagam(
#   N ~ s(
#     x,
#     y,
#     k = 20,
#     # number of basis functions (controls upper level of 'smoothness')
#     bs = 'ds',
#     # duchon spline
#     m = c(1, 0.5)
#   ),
#   # duchon spline w/ 1st deriv penalty (penalizes wiggliness)
#   sp.prior = "log.uniform",
#   # use gamma (default) or log.uniform priors on the lambda penalty term
#   diagonalize = TRUE,
#   # Should smooths be re-parameterized to have i.i.d. Gaussian priors (where possible)?
#   data = jagam.data,
#   family = "poisson",
#   file = system.file(dir.jags, "jagam-bbs.jags")
# )
#
# ## Add GAM components to model
# jags.bbs$XY = XY.scaled # Transformed XY coords
# jdat$nknots = ncol(jdat$Z)     # Number of knots
# jdat$nYears = 1   # Number of years
# jdat$nYearsB = 1  # Number of years with BBS data
#
#
#
