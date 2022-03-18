#' Set Specifications for MCMC
#'
#'
#' @param na number of adaptation iterations
#' @param nb number of burn-in iterations per chain
#' @param nc number of chains
#' @param in.parallel logical if TRUE will set number chains ('nc') value to 1
#' @param ncores number of cores to run model in parallel
#' @param ni number of iterations per chain
#' @param dev.mode integer 1:3. 1=large-ish run; 2=moderate run; 3=quick, short run.
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
                      dev.mode = 2

){


  if(dev.mode==1){
    na <- 5; ni <- 100; nb <- 5; nt <-10; nc <- 2 # quick run ## somehow took like 2 hours
  }

  if(dev.mode==2){
    na <- 5*10; ni <- 100*10; nb <- 5*10; nt <-10; nc <- 2 # quick run ## somehow took like 2 hours
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
