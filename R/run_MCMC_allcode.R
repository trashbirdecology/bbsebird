# Create a function with all the code needed to run in or out of parallel
### adapted from https://r-nimble.org/nimbleExamples/parallelizing_NIMBLE.html
### retrieved last 2022-03-10
#' Run MCMC in or Out of Parallel in nimble
#' @param seed for internal use
#' @param data data as a list
#' @param constants constants as list
#' @param inits list of initial values
#' @param monitors list of parameters to monitor
#' @param ... Additional arguments
#' @param verbose logical if TRUE and code is not running in parallel will print all output available from Nimble commands.
#' @param mcmc.specs MCMC specifications as a list; created using \code{set_mcmc_specs}
#' @param model filepath or nimbleModel object
#' @importFrom nimble compileNimble buildMCMC readBUGSmodel runMCMC
#' @export run_MCMC_allcode
run_MCMC_allcode <-
  function(
           data,
           constants = NULL,
           model,
           inits,
           mcmc.specs,
           verbose=FALSE,
           seed = sample(1:1E6, 1),
           monitors=NULL,
           ...
           ) {
    require(nimble)

    if(is.character(model) && file.exists(model)){

    model <-
      nimble::readBUGSmodel(model=model,
                            data=data,
                            constants=constants,
                            inits=inits)
    }




    Cmodel <- nimble::compileNimble(model, showCompilerOutput = verbose)

    # model$plotGraph() # add functionality to plot..
    ### inspect log probabilities
    # model$logProb_alpha
    ## trying to figure out how to add the monitors to compiled model.
    # if(!is.null(monitors)){
    #   toeval <- paste0("Cmodel$addMonitors(",
    #                    paste0("'", paste(monitors, collapse ="', '"), "'")
    #                    ,")")
    #   eval(parse(text=toeval))
    #   Cmodel$addMonitors(c("alpha", "beta", "theta"))
    #
    #
      # }

    myMCMC   <- nimble::buildMCMC(Cmodel)
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

