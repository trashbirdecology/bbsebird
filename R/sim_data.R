#' Simulate BBS Data for Use in JAGS
#'
#' Creates a list of BBS data for use in JAGS. Data is simulated using Poisson or zero-inflated Poisson distributions. Output includes both N and C.
#' @param propna proportion of all routes by year combinations to have no data (NA). this is a random process so will not always equal propna (uses rbinom to generate NAs)
#' @param propfyer proportion of routes by year combinations that comprise an observer's first year
#' @param nroutes number of BBS routes sampled across all years
#' @param nyear number of years
#' @param nchecklistmaxperyear maximum number of ebird checklists to randomly impute into grid cells per year
#' @param ngrid number of grid cells
#' @param zip TRUE will generate N using a zero-inflated poisson distribution, FALSE non-zip
#' @param maxNb max number in the true state for BBS
#' @param maxNe max number in the true state for eBird
#' @param maxgridperroute max number of grids that a single route can span. use whatever makes most sense. if probMinMultG is very low, then use 1
#' @param probMinMultG probability of a route falling into multiple grid cells. probMinMultG should decrease as grid cell size increases (i.e., less chance of a rotue falling into multiple grid cells as spatial coverage of a cell increases)
#' @param probChecklistInGrid probability of a gridcell in a given year having data(checklistid). Setting this below 1.0 allows for some cells to be without checklists each year
#' @param show.plot if TRUE will print a density plot of N and C to device
#' @export sim_bbs
sim_data <-
  function(nyear = 20,
           nroutes = 100,
           nchecklistsmaxperyear = 100,
           ngrid = 10,
           zip = TRUE,
           maxNb = 33,
           maxNe = 25,
           probMinMultG = 0.75,
           probChecklistInGrid = 0.85,
           probfyear = 0.15,
           propna = 0.25,
           maxgridperroute = 5,
           show.plot = TRUE) {
    stopifnot(maxgridperroute < ngrid)

    T    <- nyear
    G    <- ngrid
    M    <- nroutes
    N    <- nchecklistsmaxperyear

# GRID --------------------------------------------------------------------
# Simulate grid-level covariates
area <- rnorm(G, mean = 100, sd = 5)
hab1 <- rnorm(G, mean = 0, sd = 1)
hab2 <- scale(runif(G, 0, 100), center = TRUE)
# Simulate latitude/longitude
lat <- runif(n = G,-40, 40)
lon <- runif(n = G, 60, 80)
cellids <- 1:G
grid <-
  data.frame(
    lat = lat,
    lon = lon,
    gridcellid = cellids,
    hab1 = hab1,
    hab2 = hab2,
    area = area
  )

grid.list.out <- list(grid=grid)

# BBS DATA ----------------------------------------------------------------
    # Simulate routes falling in multiple grids
    ## create a site by grid matrix for prop. route in a grid
    prop <- matrix(NA, nrow = M, ncol = G)
    ind  <- vector(length = M)
    for (j in 1:M) {
      ## create an index for whether or not a given route falls into multiple grids
      ind[j] <- rbinom(n = 1, size = 1, prob = probMinMultG)

      ## if route falls into on grid cell, randomly assign a 1.00 (100%) into a grid cell
      if (ind[j] == 0) {
        prop[j, sample(1:G, size = 1, replace = FALSE)] <- 1.00
        next()
      }

      ## for each route that spans multiple grids, we need to randomly grab X consecutive (to mimic spatial neighbors) grid cells to assign to
      g.ind = sample(2:maxgridperroute, size = 1) # numb grids to assign values to
      g.ids = sort(sample(1:G, size = g.ind, replace = FALSE))

      ### create a vectr of proportions for each grid cell id (g.ids)
      props = runif(length(g.ids), 0, 1)
      props = (props + abs(min(props))) / sum(props + abs(min(props)))
      ### assign these proportions to row j prop[j, ]
      prop[j, g.ids] <- props
      rm(props)
    }
    prop[is.na(prop)] <- 0 # force NAs to zero.


    # Simulate true state (N)
    N <- matrix(nrow = M, ncol = T)
    for (j in 1:M) {
      if (zip)
        N[j,] <-
          VGAM::rzipois(n = nyear, lambda = rpois(n = 1, round(runif(1, 0, maxNb))))
      if (!zip)
        N[j,] <-
          rpois(n = nyear, lambda = rpois(n = 1, round(runif(1, 0, maxNb))))
    }

    N[N == "NaN"] <- NA

    # Simulate count data  (observations)
    C <- matrix(nrow = M, ncol = T)
    for (j in 1:M) {
      if (zip)
        C[j,] <- VGAM::rzipois(n = nyear, lambda = N[j,])
      if (!zip)
        C[j,] <- rpois(n = nyear, lambda = N[j,])
    }
    C[C == "NaN"] <- NA

    # Add colnames and rownames to C and N and prop
    colnames(N) <- 1:ncol(N)
    rownames(N) <- 1:nrow(N)
    colnames(C) <- 1:ncol(C)
    rownames(C) <- 1:nrow(C)
    colnames(prop) <- 1:ncol(prop)
    rownames(prop) <- 1:nrow(prop)


    ## Impute missing values based on argument propyrsmissingperrte
    C[matrix(rbinom(prod(dim(C)), size = 1, prob = propna) == 1, nrow = dim(C)[1])] <-
      NA

    if (show.plot) {
      if (zip)
        sub = "zero-inflated Poisson)"
      else{
        sub = "Poisson)"
      }
      plot(density(N, na.rm = TRUE), main = paste0("Simulated BBS Data \n(", sub))
      lines(density(C, na.rm = TRUE), col = "red")
      legend("topright",
             c("N", "C"),
             col = c("black", "red"),
             lty = 1)
    }

    # Simulate detectability process
    pc <- matrix(nrow = M, ncol = T) ##cars
    pn <- matrix(nrow = M, ncol = T) ##noise
    pw <- matrix(nrow = M, ncol = T) ##wind level
    pf <- matrix(nrow = M, ncol = T) ##first year on bbs or route
    for (j in 1:M) {
      pc[j, ] <-
        VGAM::rzipois(n = T, lambda = rpois(n = 1, round(runif(1, 0, 5))))
      pn[j, ] <- round(runif(n = T, min = 0, max = 3))
      pw[j, ] <- round(runif(n = T, min = 0, max = 3))
      pf[j, ] <- purrr::rbernoulli(n = T, p = probfyear)
      pf[pf == TRUE] <- 1
      pf[pf == FALSE] <- 0
    }
    ## add row and colnames to them
    pcovs <- list(
      cars = pc,
      noise = pn,
      wind = pw,
      fyear = pf
    )
    for (i in seq_along(pcovs)) {
      colnames(pcovs[[i]]) <- 1:ncol(pcovs[[i]])
      rownames(pcovs[[i]]) <- 1:nrow(pcovs[[i]])
    }
    ## create long dfs for pcovs
    pcovs.long <- list()
    pcovs.long.df <- data.frame()
    for (i in seq_along(pcovs)) {
      pcovs.long[[i]] <-
        reshape2::melt(pcovs[[i]], value.name = names(pcovs)[i])
      names(pcovs.long[[i]])[1:2] <- c("site", "year")
      if (i == 1)
        pcovs.long.df <- pcovs.long[[i]]
      pcovs.long.df <- merge(pcovs.long[[i]], pcovs.long.df)
    }


    # Save BBS data in long format
    C.long <- reshape2::melt(C, value.name = "C")
    N.long <- reshape2::melt(N, value.name = "N")
    names(C.long)[1:2] <- names(N.long)[1:2] <- c("site", "year")

    N.long <- merge(N.long, grid)
    C.long <- merge(C.long, grid)

    # Combine all data
    df.long <-
      merge(pcovs.long.df, merge(grid, merge(N.long, C.long)))


    # Create initial values for gam based on BBS counts
    Nib <- df.long %>%
      group_by(gridcellid, year) %>%
      filter(N==max(N, na.rm =
                           TRUE)) %>%
      distinct(gridcellid, year, N) %>%
      ungroup() %>%
      arrange(gridcellid, year) %>%
      pivot_wider(id_cols = "gridcellid",
                  names_from = "year",
                  values_from = "N") %>%
      column_to_rownames(var="gridcellid")
    names(Nib) <- as.integer(names(Nib))


    # Output list
    bbs.list.out <- list(
      N = N,
      C = C,
      Nib = Nib,
      pc = pc,
      pn = pn,
      pw = pw,
      pf = pf,
      prop = prop,
      # hab1 = hab1,
      # hab2 = hab2,
      # area = area,
      lat = lat,
      lon = lon,
      nyear = T,
      ngrid = G,
      nroutes = M,
      df.long = df.long
    )



# EBIRD -------------------------------------------------------------------
    probfun <-
      function() {
        prob = sample(
          c(0, 1),
          size = 1,
          prob = c(1 - probChecklistInGrid, probChecklistInGrid)
        )
        return(prob)
        }

grid  <- sort(rep(1:G, T))
years <- rep(1:T, G)
d  <- data.frame(gridcellid= grid, year= years, prob=NA)
### probability that the cell has checklists that year
for(i in 1:nrow(d)){
  d$prob[i] = probfun()
  d$ncls[i] = round(runif(1, min=N/2, max=N))
}
### impute checklists
d <- d %>% filter(prob==1) %>% ## keep only rows year/grids where checklists will occur
      select(-prob)
ebird <- data.frame(gridcellid=NA, year=NA, N=NA, C=NA)
for(i in 1:nrow(d)){
  ncl=d$ncls[i]
  id=d$gridcellid[i]
  year=d$year[i]

tempdf <- data.frame(gridcellid = rep(id, ncl), year=rep(year, ncl))
# create N within each grid cell and year
if (zip)
  tempdf$N <-
  VGAM::rzipois(n = nrow(tempdf), lambda = rpois(n = 1, round(runif(1, 0, maxNe))))
if (!zip)
  tempdf$N <-
  rpois(n = nrow(tempdf), lambda = rpois(n = 1, round(runif(1, 0, maxNe))))
# Simulate count data  (observations)
  if (zip)
    tempdf$C <- VGAM::rzipois(n = nrow(tempdf), lambda = tempdf$N)
  if (!zip)
    tempdf$C <- rpois(n = nrow(tempdf), lambda = tempdf$N)
tempdf$C[tempdf$C == "NaN"] <- NA

ebird <- bind_rows(tempdf, ebird)

}
ebird$checklistid <- 1:nrow(ebird)
ebird <- ebird %>% arrange(gridcellid, year, checklistid) %>%
  filter(!is.na(checklistid))

# ## simulate num observers
# ebird$nobs <-

ebird.list.out <- list(
  df.long = ebird,
  N = ebird$N,
  C = ebird$C,
  nyear = T,
  ngrid = G,
  nroutes = M
)


if (show.plot) {
  if (zip)
    sub = "zero-inflated Poisson)"
  else{
    sub = "Poisson)"
  }
  plot(density(ebird$N, na.rm = TRUE), main = paste0("Simulated eBird Data \n(", sub))
  lines(density(ebird$C, na.rm = TRUE), col = "red")
  legend("topright",
         c("N", "C"),
         col = c("black", "red"),
         lty = 1)
}

# OUTPUT ALL DATA ---------------------------------------------------------

return(list(bbs=bbs.list.out, ebird=ebird.list.out, grid=grid.list.out))

  } # END FUNCTION
