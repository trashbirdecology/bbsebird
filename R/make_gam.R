#' Function for Generating Basis Functions
#' @param method one of c("mgcv", "cubic2d", "strebel")
#' @param nd  number of points to use in cover design. See fields::cover.design.
#' @param num.nn max number of nearest neighbors. See fields::cover.design.
#' @param nruns The number of random starts to be optimized.See fields::cover.design.
#' @param max.loop Maximum number of passes throguh cover.design algorithm. See fields::cover.design.
#' @param coords matrix or data frame with easting and northing coordinates, respectively. First column should be easting (e.g., X, long) and second northing.
#' @param print.plot logical if TRUE will print spatial representation of site and knot locations to local device.
#' @param plot.main optional Character vector for plot title.
#' @param ll.to.utm logical if TRUE will convert lat long coordinates to UTMs prior to creating distance-based basis functions.
#' @param K max number of basis functions to produce in mgcv::jagam()
#' @param scale.coords logical if TRUE will scale and center the XY coordinates using bbsebird::standardize.
#' @importFrom fields cover.design rdist
#' @export make_gam
make_gam <- function(coords,
                     method = "cubic2d",
                     nd = 20,
                     K        = NULL,
                     nruns = 10,
                     num.nn = 20,
                     ll.to.utm = TRUE,
                     max.loop = 40,
                     print.plot = TRUE,
                     plot.main  = NULL,
                     scale.coords = TRUE
                     ) {
  # ARGS --------------------------------------------------------------------
  coords <- as.matrix(coords)
  method <- tolower(method)
  stopifnot(method %in% c("cubic2d", "jagam", "mgcv", "cubicalt"))
  stopifnot(ncol(coords) >= 2)
  stopifnot(is.numeric(coords[, 1]) | is.integer(coords[, 1]))
  stopifnot(is.numeric(coords[, 2]) | is.integer(coords[, 2]))

  # SCALE XY ----------------------------------------------------------------
  XY <- matrix(NA, nrow = nrow(coords), ncol = 2)
  if (scale.coords) {
    cat("   [note] coords.scaled == TRUE; z-scaling coordinates.\n")
    XY[, 1] <- standardize(coords[, 1])
    XY[, 2] <- standardize(coords[, 2])
  } else{
    XY <- coords[, 1:2]
  }


# LL to UTM? --------------------------------------------------------------
  if(ll.to.utm){
    XY <- longlat_to_UTM(XY[,1], XY[,2])

  }


  # JAGAM -------------------------------------------------------------------------
  # if not specified, K is defined as:
  ### this is kind of arbitrary. based on some Wood, Simon paper about min 20
  ### and anything > like 100 will crash most systems.
  ### also need to consider compute time for use in Bayesian param estiamtion
  if (method %in% c("mgcv", "jagam")) {
    cat("  [note] creating 2D duchon splines using `mgcv::jagam()`\n")
    if (!is.null(K) && K > nrow(XY)) {
      message(
        "[important] you defined K as a value higher than the unique number of available grid cells. Resetting K automatically. See notes following. \n"
      )
      K <- NULL
    }
    if (is.null(K)) {
      ###logic for K selection borrowed from AHM2 book , which cites Giminez et al. 2009 and Rupert et al. 2003
      K <- max(20, min(round(nrow(XY) / 4), 150))
    }

    cat(
      "  [note] K is currently set to ",
      K,
      ". If you have memory errors, try defining K in arguments as a lower value\n"
    )
    jagam.fn <- paste0(dir.outputs, "/gam-UNEDITED.txt")
    jagam.mod <- mgcv::jagam(
      c ~ s(
        # note the c doesn't matter, it's just for show
        X,
        Y,
        bs = "ds",
        k = K,
        m = c(1, 0.5)
      ),
      file = jagam.fn,
      sp.prior = "log.uniform",
      data = bf.in,
      diagonalize = TRUE,
      # parallell = TRUE,
      # modules = "glm"
      family = "poisson"
    )
    jagam.mod$fn <- jagam.fn
    ## specify some output scalars and data
    Z.mat <-
      jagam.mod$jags.data$X               # dims <ncells  by nknots>
    nbfs  <-
      dim(jagam.mod$jags.data$X)[2]        # number of basis functions/knots

  }#end JAGAM


  # CUBIC --------------------------------------------------
  if (method %in% c("strebel")) {
    ### follow the methods of Strebel et al. (which follows methods of Royle and Kery AHM)
    cat("  [note] creating 2D cubic splines following Strebel et al. \n")
    # XY <- bf.in[c("X", "Y")] ### the "scaled down" coordinates
    # Define the omega and Z.k matrices for the random effects
    omega.all <-
      fields::rdist(XY, XY) ^ 3 # 2D cubic splines on "reduced coords
    svd.omega.all <- svd(omega.all)
    sqrt.omega.all <-
      t(svd.omega.all$v %*% (t(svd.omega.all$u) * sqrt(svd.omega.all$d)))
    ##
    Z.k   <- (fields::rdist(XY.orig, XY)) ^ 3
    Z.mat <- t(solve(sqrt.omega.all, t(Z.k)))
    nbfs  <- dim(Z.mat)[2]
  }

  if (method %in% "cubic2d") {
    cat("   [note] using method cubic2d\n")
    # XY <- bf.in[c("X", "Y")] ### the "scaled down" coordinates
    knots <-
      fields::cover.design(
        cbind(XY$x, XY$y),
        nd = nd,
        nruns = nruns,
        num.nn = num.nn,
        max.loop = max.loop
      )$design
    omega.all <- fields::rdist(knots, knots) ^ 3
    svd.omega.all <- svd(omega.all)
    sqrt.omega.all <- t(svd.omega.all$v %*% (t(svd.omega.all$u) *
                                               sqrt(svd.omega.all$d)))
    Z.k <- (fields::rdist(cbind(XY$x, XY$y), knots)) ^ 3
    Z.mat <- t(solve(sqrt.omega.all, t(Z.k)))
    nbfs <- dim(Z.mat)[2]

  } # end cubic2d

  # plot --------------------------------------------------------------------
  if (print.plot) {
    if (method %in% c("mgcv", "jagam")) {
      "plotting currently not supported for mgcv gam"
    } else{
      try({
        plot(cbind(XY$x, XY$y), pch = 20, main = plot.main, xlab="X coord", ylab="Y coord")
        points(knots,
               pch = 20,
               col = "red",
               cex = 2)
      })
    }


  }


  # RETURN OBJECT -----------------------------------------------------------
  if(ll.to.utm) {out <- list(K         = nbfs,
              Z.mat     = Z.mat,
              XY        = data.frame(x=XY$x, y=XY$y),
              XY.utm.df = XY
              )
  }else{
  out <- list(K         = nbfs,
              Z.mat     = Z.mat,
              XY        = XY)
  }
  ## add knots if not gam
  if (!method %in% c("mgcv", "jagam"))
    out$knotlocs <- knots

  ## remove empty objects
  todel <- NULL
  for (i in seq_along(out)) {
    if (length(out[[i]]) == 1 &&
        out[[i]][1] == "NULL") {
      todel <- c(todel, names(out)[i])
    }
  } # end i loop
  if (!is.null(todel)) out <- pluck_multiple(out, remove = todel)

  return(out)
} # end fFUn
