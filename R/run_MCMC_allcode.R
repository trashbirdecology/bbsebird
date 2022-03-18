# Create a function with all the code needed to run in or out of parallel
### adapted from https://r-nimble.org/nimbleExamples/parallelizing_NIMBLE.html
### retrieved last 2022-03-10
#' Run MCMC in or Out of Parallel in nimble
#' @param seed for internal use
#' @param data data and constants as list
#' @param inits list of initial values
#' @param ... Additional arguments
#' @param verbose logical if TRUE and code is not running in parallel will print all output available from Nimble commands.
#' @param mcmc.specs MCMC specifications as a list; created using \code{set_mcmc_specs}
#' @param model filepath or nimble model object
#' @importFrom nimble compileNimble buildMCMC readBUGSmodel runMCMC
#' @export run_MCMC_allcode
run_MCMC_allcode <-
  function(
           data,
           model,
           inits,
           mcmc.specs,
           verbose=FALSE,
           seed = sample(1:1E6, 1),
           monitors=NULL,
           ...
           ) {
    require(nimble)
    myModel <-
      nimble::readBUGSmodel(model,
                            data,
                            inits)

    CmyModel <- nimble::compileNimble(myModel, showCompilerOutput = verbose)
    myMCMC   <- nimble::buildMCMC(CmyModel)
    CmyMCMC  <- nimble::compileNimble(myMCMC, showCompilerOutput = verbose)

    results <-
      nimble::runMCMC(
        CmyMCMC,
        niter   = mcmc.specs$ni,
        setSeed = seed,
        progressBar = TRUE,
        ## won't show if in PARALLEL
        thin = mcmc.specs$nt,
        nburnin = mcmc.specs$nb
      )

    return(results)
  }

