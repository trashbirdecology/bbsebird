# Create a function with all the code needed to run in or out of parallel
### adapted from https://r-nimble.org/nimbleExamples/parallelizing_NIMBLE.html
### retrieved last 2022-03-10
#' Run MCMC in or Out of Parallel in nimble
#' @param seed for internal use
#' @param data data and constants as list
#' @param inits list of initial values
#' @noRd
#' @param model filepath or nimble model object
#' @importFrom nimble compileNimble buildMCMC readBUGSmodel runMCMC
#' @export run_MCMC_allcode
run_MCMC_allcode <-
  function(seed = 1,
           data,
           model
           ) {
    library(nimble)
    myModel <-
      nimble::readBUGSmodel(model,
                            data,
                            inits)
    CmyModel <- nimble::compileNimble(myModel, showCompilerOutput = verbose)
    myMCMC <- nimble::buildMCMC(CmyModel)
    CmyMCMC <- nimble::compileNimble(myMCMC, showCompilerOutput = verbose)

    results <-
      nimble::runMCMC(
        CmyMCMC,
        niter   = mcmc.specs$ni,
        setSeed = seed,
        progressBar = parallel,
        ## won't show if in PARALLEL
        thin = mcmc.specs$nt,
        nburnin = mcmc.specs$nb
      )

    return(results)
  }

