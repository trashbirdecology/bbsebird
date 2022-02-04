#' Make 2-D array for Indexing Purposes
#'
#' @param X a data frame with variables site.id, site.ind, grid.id, grid.ind, year.id, year.ind, and "c"
#' @keywords internal
#' @importFrom dplyr select
make_indexing_df <- function(X) {
## Purpose of this functin is to create data frames for indexing rows and columns inside the sampling matrices in which we expect data.
names(X) <- tolower(names(X))

# Grab the combination of site and year ids where we expect data
Y <- X %>%
  dplyr::select(year.ind, year.id, site.ind, site.id, grid.ind, grid.id, c) %>%
  na.omit(c)


samples <- Y %>% dplyr::select(year.ind, site.ind, grid.ind)

for(h in 1:nrow(samples)){stopifnot(C[samples$site.ind[h], samples$year.ind[h]] == Y$c[h])}


# Create Indices for Output Object
sampled.sites.ind <- sort(unique(Y$site.ind))
n.sampled.sites   <- length(sampled.sites.ind)

sampled.grids.ind <- sort(unique(Y$grid.ind))
n.sampled.grids   <- length(sampled.grids.id)

sampled.years.ind <- sort(unique(Y$year.ind))
n.sampled.years   <- length(sampled.years.ind)

# output list
index.out <- list(
  grids.id   = sampled.grids.id,
  grids.ind  = sampled.grids.ind,
  ngrids     = n.sampled.grids,
  sites.id   = sampled.sites.id,
  sites.ind  = sampled.sites.ind,
  nsites     = n.sampled.sites,
  year.id    = sampled.years.id,
  year.ind   = sampled.years.ind,
  nyears     = n.sampled.years,
  samples    = samples
)

# return object
return(index.out)
}
