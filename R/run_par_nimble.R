#' Run Nimble Model in Parallel
#' Function will run a Nimble model for 'nc' indepdendent chains
#' @param code nimble model code
#' @param data data as a list
#' @param constants constants as list
#' @param inits list of initial values
#' @param seed used to set the seed for nimble within each processor
#' @param ncores maximum number of cores to employ. Actual number used is the minimum of nc and ncores
#' @param mod.name optional Used to save model output to file. Defaults to 'mynimbleModel'
#' @param monitors optional Character vector of parameters to monitor.
#' @param ni number iterations to run
#' @param nb number of burn-in iterations to discard
#' @param nt thinning rate (every Nth iteration will be saved)
#' @param nc number of chains to run (in parallel)
#' @param aI optional If using parameter block sampler, specify the adaptive interval
#' @param ntries optional If using parameter block sampler, specify the maximum number of tries
#' @param block.name optional one of c("alpha+b", "all"). If "alpha+b" will block each alpha and b across all T. If "all" will block all alpha and b for each Ts.
#' @param block.samp.type optional one of c("AF_slice", "RW_block").
#' @param mod.name model name
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster parLapply
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom nimble compileNimble buildMCMC runMCMC nimbleModel configureMCMC runMCMC buildMCMC
#' @export run_par_nimble

run_par_nimble <- function(code,
                         data,
                         constants = NULL,
                         inits,
                         monitors = NULL,
                         ncores = NULL,
                         ni = 10000,
                         nb = NULL,
                         nt = 1,
                         nc = 1,
                         aI = 100,
                         ntries = 5,
                         block.name = NULL,
                         block.samp.type = "AF_slice",
                         mod.name = "mymodel",
                         seed = 123
                         ){
## arg eval
if(is.null(nb)) nb <- round(ni/nt*.15, 0)
stopifnot(ni/nt - nb > 100)
if(is.null(ncores)) ncores <- min(nc, parallel::detectCores()-1)

## make empty objects
minefficiency <- list(NULL)
list.out      <- list(NULL)



# FUN: in.parallel() ------------------------------------------------------
in.parallel <- function(code,
                        data,
                        constants = NULL,
                        inits=inits,
                        monitors = NULL,
                        nt=1,
                        ni=1e4,
                        nb=0,
                        aI=100,
                        ntries=10,
                        block.name=NULL,
                        block.samp.type=NULL,
                        seed=123
){
  library(nimble)


  ## compile model code
  Rmodel   <- nimbleModel(
    code = code,
    data = data,
    constants = constants,
    inits = inits,
    check = TRUE
  )
  Rmodel.comp <- compileNimble(Rmodel)
  ## configure MCMC alg
  Rmodel.conf <- configureMCMC(Rmodel,
                               monitors = monitors,
                               thin = nt,
                               nburnin = nb)
  ## add block on b across all T
  if (block.name == "alpha+b") {
    Rmodel.conf$removeSampler(c('alpha', 'b'))

    for (b in 1:constants$K) {
      Rmodel.conf$addSampler(
        target = paste0("b[", b, ",1:", constants$T, "]"),
        type = block.samp.type,
        control = list(
          # adaptive = TRUE,
          tries = ntries,
          adaptInterval = aI
        )
      )
    }

    ## add block on alpha across all T
    Rmodel.conf$addSampler(
      target = paste("alpha[1:", constants$T, "]"),
      type = block.samp.type,
      control = list(adaptive = TRUE),
      adaptInterval = aI
    )
  } # end alpha + b
  if(block.name == "all"){
    Rmodel.conf$removeSampler(c("alpha","b"))
    Rmodel.conf$addSampler(
      c("alpha", "b"),
      type = block.samp.type,
      control =
        list(adaptInterval = aI, ntries=ntries),
      silent = TRUE)
  }

  # Build and compile
  Rmcmc  <- buildMCMC(Rmodel.conf)
  Cmcmc  <- compileNimble(Rmcmc, project = Rmodel, resetFunctions = TRUE)

  # Run
  results <- runMCMC(Cmcmc,
                     niter = ni,
                     setSeed = seed
                     )

  return(results)

}  # END


# Init cluster ------------------------------------------------------------
this_cluster <- parallel::makeCluster(ncores)
results <-
  parallel::parLapply(
    cl = this_cluster,
    X = 1:ncores,
    fun = in.parallel,
    code=code,
    data=data,
    constants = constants,
    inits=inits,
    monitors = monitors,
    nt=nt,
    nb=nb,
    aI=aI,
    ni=ni,
    ntries=ntries,
    block.name=block.name,
    block.samp.type=block.samp.type
  )

try({
  # cat("Attempting to stop cluster\n")
  doParallel::stopImplicitCluster()
  parallel::stopCluster(this_cluster)
})



return(results)


} # END FUN
