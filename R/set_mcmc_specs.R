#' Set Specifications for MCMC
#'
#'
#' @param na number of adaptation iterations
#' @param nb number of burn-in iterations per chain
#' @param nc number of chains
#' @param ncores number of cores to run model in parallel
#' @param ni number of iterations per chain
#' @importFrom  parallel detectCores
#' @param nt thinning rate (every nth iteration removed)
#' @export set_mcmc_specs
set_mcmc_specs <- function(na=1000,
                      nb=5000,
                      nc=3,
                      ni=20000,
                      nt=10,
                      ncores=parallel::detectCores()-1
){

  return(list(na=na, nb=nb,
              nc=nc, ni=ni,
              nt=nt, ncores=ncores
              ))

}
