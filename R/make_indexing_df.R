#' Make 2-D array (Matrix for JAGS)
#'
#' @param X a data frame with dimensions site by year
#' @param Y a data frame with dimensions grid by site
#' @keywords internal
make_indexing_df <- function(X, Y) {
  # X=C;Y=Xg.area
## use the C matrix for site by year indexing
## use Xg.area where cell >0 for grid by site indexing
sites.ind   <- data.frame(site.id=rownames(X), site.ind = 1:nrow(X))
year.ind    <- data.frame(year.id=as.integer(colnames(X)), year.ind=1:ncol(X))
# colnames(X) <- year.ind$year.ind
rownames(X) <- sites.ind$site.ind

indexing <- X %>%
  mutate(site.ind = sites.ind$site.ind) %>%
  tidyr::pivot_longer(cols=-c(site.ind),
                      names_to="year",
                      values_to="temp") %>%
  na.omit(temp) %>%
  dplyr::select(-temp)
## append grid cell ids
temp.grid <- Y %>%
  mutate(site.ind = sites.ind$site.ind) %>%
  tidyr::pivot_longer(cols=-c(site.ind),
                      names_to="gridcellid",
                      values_to="temp") %>%
  dplyr::distinct(site.ind, gridcellid)
# %>%
#   filter(temp>0) %>% # not sure why i wanted to remove this.
if("checklist_id"%in%rownames(X))cat("creating the indexing for ebird...may take a minute...\n")
indexing <- dplyr::left_join(indexing, temp.grid) %>%
  filter(!is.na(gridcellid)) %>%
  dplyr::arrange(site.ind, year, gridcellid) %>%
  mutate(gridcellid = as.integer(gridcellid)) %>%
  mutate(year = as.integer(year))

nSites <- length(unique(indexing$site.ind))
nGrids <- length(unique(indexing$gridcellid))
nYears <- length(unique(indexing$year))

# output list
index.out <- list(
  # indexing   = indexing,
  nsites     = nSites,
  ngrids     = nGrids,
  nyears     = nYears,
  sites.id   = sites.ind$site.id,
  sites.ind  = sites.ind$site.ind,
  year.id    = year.ind$year.id,
  year.ind   = year.ind$year.ind
)

# return object
return(index.out)
}
