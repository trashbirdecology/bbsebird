#' Make 2-D array for Indexing Purposes
#'
#' @param X a data frame with variables site.id, site.ind, grid.id, grid.ind, year.id, year.ind, and "c"
#' @keywords internal
#' @importFrom dplyr select distinct
do_indexing <- function(X) {
## Purpose of this functin is to create data frames for indexing rows and columns inside the sampling matrices in which we expect data.
names(X) <- tolower(names(X))

# Grab the combination of site and year ids where we expect data
Y <- X %>%
  dplyr::select(year.ind, year.id, site.ind, site.id, grid.ind, grid.id, c) %>%
  na.omit(c)

sg.lookup <- Y %>% dplyr::distinct(site.ind, site.id, grid.ind, grid.id)
sy.lookup <- Y %>% dplyr::distinct(year.ind, year.id, site.ind, site.id)

samples <- Y %>% dplyr::distinct(year.ind, site.ind, grid.ind)

for(h in 1:nrow(samples)){stopifnot(C[samples$site.ind[h], samples$year.ind[h]] == Y$c[h])}

# LOCATION OF CELLS WITH DATA in BBS/EBIRD MATRICES
## indexing data frame for site-by-year matirces
sy <- samples %>% distinct(site.ind, year.ind) %>%
  arrange(site.ind, year.ind) %>%
  dplyr::select(site.ind, year.ind)
nsy <- nrow(sy)
## indexing data frame for site-by-grid matrices
sg <- jdat$bbs$indexing$samples %>%
          distinct(site.ind, grid.ind) %>%
          arrange(site.ind, grid.ind) %>%
          dplyr::select(site.ind, grid.ind)
nsg <- nrow(sg)

## Unique sites, grids, years
y.ind  <- sort(unique(samples$year.ind))
nyears <- length(y.ind)
s.ind  <- sort(unique(samples$site.ind))
nsites <- length(s.ind)
g.ind  <- sort(unique(samples$grid.ind))
ngrids <- length(g.ind)


## if data is BBS, need to collect information on proportion of routes in cells
if(any(tolower(names(X)) %in% c("proprouteincell", "rteno", "routenum"))){
  prop <- X %>% dplyr::distinct(proprouteincell,
                              site.ind, grid.ind)
  prop$proprouteincell[is.na(prop$proprouteincell)] <- 0 # yes, do this twice. too lazy to figure out why though.
  prop.sg <- reshape2::acast(data = prop,
                  formula = site.ind~grid.ind,
                  value.var = "proprouteincell")
  ## remove the row where site ind == NA if its there
  prop.sg <- prop.sg[!rownames(prop.sg) %in% c(NA, "NA", "NULL", NULL),]
  prop.sg[is.na(prop.sg)] <- 0 # yes, do this twice. too lazy to figure out why though.
}


# Specify potential output object names
objs.index <- c(
  "s", "g", "y",
  "sg", "nsg", "sy", "nsy",
  "nsites", "ngrids", "nyears",
  "sy.lookup", "sg.lookup",
  "prop.sg"
)


objs.in <- objs.index[objs.index %in% ls()] %>% as.vector()
index.out <- vector(mode='list', length=length(objs.in))
names(index.out) <- objs.in
for (z in seq_along(objs.in)) {
  new = eval(parse(text = objs.in[z]))# this is necessary for some reason idk why
  index.out[[objs.in[z]]] <- new
}
#remove all objects to be sure they arent put into other lists
suppressWarnings(rm(list=c(objs)))

# return object
return(index.out)
}
