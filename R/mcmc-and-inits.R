#' Set Specifications for MCMC
#'
#'
#' @param na number of adaptation iterations
#' @param nb number of burn-in iterations per chain
#' @param nc number of chains
#' @param in.parallel logical if TRUE will set number chains ('nc') value to 1
#' @param ncores number of cores to run model in parallel
#' @param ni number of iterations per chain
#' @param dev.mode logical if TRUE will set mcmc specs to very low values to allow for a quick test of model
#' @importFrom  parallel detectCores
#' @param nt thinning rate (every nth iteration removed)
#' @export set_mcmc_specs
set_mcmc_specs <- function(
                      na=1000,
                      nb=5000,
                      nc=3,
                      ni=20000,
                      nt=10,
                      in.parallel=FALSE,
                      ncores=parallel::detectCores()-1,
                      dev.mode = FALSE

){


  if(dev.mode){
    na <- 5; ni <- 10; nb <- 5; nt <-1; nc <- 2 # quick run ## somehow took like 2 hours
  }

  if(in.parallel){nc <- 1}

  return(list(na=na, nb=nb,
              nc=nc, ni=ni,
              nt=nt, ncores=ncores
              ))

}



#' Create a List of Repeated Initial Values for each Chain
#'
#' @param inits a list of initial values which will be repeated according to arg `nc`
#' @param nc number of chains (number of times the inits will be repeated)
#' @export make_inits_list
make_inits_list <- function(inits, nc = 3) {
  inits.out <- list()
  for (i in 1:nc) {
    inits.out[[i]] <- inits
  }
  stopifnot(length(inits.out) == nc)
  return(inits.out)

}
