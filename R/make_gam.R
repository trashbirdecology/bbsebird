#' Function for Generating Basis Functions
#' @param method one of c("mgcv", "cubic2d", "strebel")
#' @param nd  ...
#' @param nruns ...
#' @param nruns ...
#' @param coords matrix or data frame with easting and northing coordinates, respectively. First column should be easting (e.g., X, long) and second northing.
#' @param num.nn ...
#' @param print.plot logical if TRUE will print the spatial representation of gam to local device
#' @param max.loop ...
#' @param ll.to.utm logical if TRUE will convert lat long coordinates to UTMs prior to creating distance-based basis functions.
#' @param K max number of basis functions to produce in mgcv::jagam()
#' @param scale.coords logical if TRUE will scale the XY coordinates
#' @param XY matrix or data frame comprising XY coordinates, where first column is X coordinate and second is Y.
#' @importFrom fields cover.design rdist
#' @export make_gam
make_gam <- function(coords,
                     method = "cubic2d",
                     nd = 20,
                     nruns = 10,
                     num.nn = 20,
                     ll.to.utm = TRUE,
                     print.plot=TRUE,
                     max.loop = 40,
                     plot.main=NULL,
                     K        = NULL,
                     scale.coords = FALSE) {

# ARGS --------------------------------------------------------------------
  coords <- as.matrix(coords)
  method <- tolower(method)
  stopifnot(method %in% c("cubic2d", "jagam", "mgcv", "cubicalt"))
  stopifnot(ncol(coords)>=2)
  stopifnot(  is.numeric(coords[,1]) | is.integer(coords[,1]))
  stopifnot(  is.numeric(coords[,2]) | is.integer(coords[,2]))

# SCALE XY ----------------------------------------------------------------
XY <- matrix(NA, nrow=nrow(coords), ncol=2)
if(scale.coords){
  cat("   [note] coords.scaled == TRUE; z-scaling coordinates.\n")
  XY[,1] <- standardize(coords[,1])
  XY[,2] <- standardize(coords[,2])
} else{
  XY <- coords[,1:2]
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
        XY,
        nd = nd,
        nruns = nruns,
        num.nn = num.nn,
        max.loop = max.loop
      )$design
    omega.all <- fields::rdist(knots, knots) ^ 3
    svd.omega.all <- svd(omega.all)
    sqrt.omega.all <- t(svd.omega.all$v %*% (t(svd.omega.all$u) *
                                               sqrt(svd.omega.all$d)))
    Z.k <- (fields::rdist(XY, knots)) ^ 3
    Z.mat <- t(solve(sqrt.omega.all, t(Z.k)))
    nbfs <- dim(Z.mat)[2]

  } # end cubic2d

# plot --------------------------------------------------------------------
if(print.plot){
  if(method %in% c("mgcv", "jagam")){"plotting currently not supported for mgcv gam"}else{
    plot(XY,pch=20, main=plot.main)
    points(knots, pch=20,col="red",cex=2)
  }


}


# RETURN OBJECT -----------------------------------------------------------
out <- list(
  K         = nbfs,
  Z.mat     = Z.mat,
  XY        = XY)
## add knots if not gam
if(!method %in% c("mgcv", "jagam")) out$knotlocs <- knots

## remove empty objects
todel <- NULL
for(i in seq_along(out)){
 if(length(out[[i]]) == 1 & out[[i]][1]=="NULL"){todel<-c(todel, names(out)[i])}
}
if(!is.null(todel)) out <- pluck_multiple(out, remove = todel)

return(out)
} # end fFUn
