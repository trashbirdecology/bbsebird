#' Simulate BBS Data for Use in JAGS
#'
#' Creates a list of BBS data for use in JAGS. Data is simulated using Poisson or zero-inflated Poisson distributions. Output includes both N and C.
#' @param propna proportion of all routes by year combinations to have no data (NA). this is a random process so will not always equal propna (uses rbinom to generate NAs)
#' @param propfyer proportion of routes by year combinations that comprise an observer's first year
#' @param nsite number of routes
#' @param nyear number of years
#' @param ngrid number of grid cells
#' @param zip TRUE will generate N using a zero-inflated poisson distribution, FALSE non-zip
#' @param maxN max number in the true state
#' @param maxgridperroute max number of grids that a single route can span. use whatever makes most sense. if probMinMultG is very low, then use 1
#' @param probMinMultG probability of a route falling into multiple grid cells. probMinMultG should decrease as grid cell size increases (i.e., less chance of a rotue falling into multiple grid cells as spatial coverage of a cell increases)
#' @param show.plot if TRUE will print a density plot of N and C to device
#' @export sim_bbs
#'
sim_bbs <-
  function(nyear = 20,
           nsite = 100,
           ngrid = 10,
           zip = TRUE,
           maxN = 33,
           probMinMultG = 0.75,
           probfyear = 0.15,
           propna = 0.25,
           maxgridperroute = 5,
           show.plot = TRUE) {
    stopifnot(maxgridperroute < ngrid)

    T    <- nyear
    G    <- ngrid
    M    <- nsite

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
        N[j, ] <-
          VGAM::rzipois(n = nyear, lambda = rpois(n = 1, round(runif(1, 0, maxN))))
      if (!zip)
        N[j, ] <-
          rpois(n = nyear, lambda = rpois(n = 1, round(runif(1, 0, maxN))))
    }

    N[N == "NaN"] <- NA

    # Simulate count data  (observations)
    C <- matrix(nrow = M, ncol = T)
    for (j in 1:M) {
      if (zip)
        C[j, ] <- VGAM::rzipois(n = nyear, lambda = N[j, ])
      if (!zip)
        C[j, ] <- rpois(n = nyear, lambda = N[j, ])
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
      pc[j,] <-
        VGAM::rzipois(n = T, lambda = rpois(n = 1, round(runif(1, 0, 5))))
      pn[j,] <- round(runif(n = T, min = 0, max = 3))
      pw[j,] <- round(runif(n = T, min = 0, max = 3))
      pf[j,] <- purrr::rbernoulli(n = T, p = probfyear)
      pf[pf == TRUE] <- 1
      pf[pf == FALSE] <- 0
    }
    ## add row and colnames to them
    pcovs <- list(cars=pc, noise=pn, wind=pw, fyear=pf)
    for(i in seq_along(temp)){
      colnames(pcovs[[i]]) <- 1:ncol(pcovs[[i]])
      rownames(pcovs[[i]]) <- 1:nrow(pcovs[[i]])
    }
    ## create long dfs for pcovs
    pcovs.long <- list()
    pcovs.long.df <- data.frame()
    for(i in seq_along(pcovs)){
        pcovs.long[[i]] <- reshape2::melt(pcovs[[i]], value.name = names(pcovs)[i])
        names(pcovs.long[[i]])[1:2] <- c("site","year")
        if(i==1) pcovs.long.df <- pcovs.long[[i]]
        pcovs.long.df <- merge(pcovs.long[[i]], pcovs.long.df)
    }


    # Simulate grid-level covariates
    hab1 <- rnorm(G, mean = 0, sd = 1)
    hab2 <- scale(runif(G, 0, 100), center = TRUE)
    # Simulate latitude/longitude
    lat <- runif(n = G, -40, 40)
    lon <- runif(n = G, 60, 80)
    cellids <- 1:G
    grid <- data.frame(lat=lat, lon=lon, gridcellid=cellids, hab1=hab1, hab2=hab2)

    # Save BBS data in long format
    C.long <- reshape2::melt(C, value.name = "C")
    N.long <- reshape2::melt(N, value.name = "N")
    names(C.long)[1:2] <- names(N.long)[1:2] <- c("site", "year")

    N.long <- merge(N.long, grid)
    C.long <- merge(C.long, grid)

    bbs.long <- merge(pcovs.long.df, merge(grid, merge(N.long, C.long)))

    list.out <- list(
      N = N,
      C = C,
      pc = pc,
      pn = pn,
      pw = pw,
      pf = pf,
      hab1 = hab1,
      hab2 = hab2,
      prop = prop,
      nyear = T,
      ngrid = G,
      nsite = M,
      lat = lat,
      lon = lon,
      bbs.long=bbs.long
    )
    return(list.out)

  } # END FUNCTION
