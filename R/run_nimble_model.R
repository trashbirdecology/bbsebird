#' Run Nimble Model in Parallel
#' Function will run a Nimble model for 'nc' indepdendent chains
#' @param code nimble model code
#' @param data data as a list
#' @param constants constants as list
#' @param inits list of initial values
#' @param parallel logical if TRUE will run chains in parallel (using foreach).
#' @param ncores maximum number of cores to employ. Actual number used is the minimum of nc and ncores
#' @param monitors optional Character vector of parameters to monitor.
#' @param ni number iterations to run
#' @param nb number of burn-in iterations to discard (I think it's PRE-THINNING burnin discard...)
#' @param nt thinning rate (every Nth iteration will be saved)
#' @param nc number of chains to run (in parallel)
#' @param ntries optional If using parameter block sampler, specify the maximum number of tries
#' @param block.name optional one of c("alpha+b", "all"). If "alpha+b" will block each alpha and b across all T. If "all" will block all alpha and b for each Ts.
#' @param block.samp.type optional one of c("AF_slice", "RW_block").
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom nimble compileNimble buildMCMC runMCMC nimbleModel configureMCMC runMCMC buildMCMC
#' @export run_nimble_model

run_nimble_model <- function(code,
                           data,
                           constants = NULL,
                           inits,
                           monitors = NULL,
                           ncores = NULL,
                           ni = 10000,
                           nb = NULL,
                           nt = 1,
                           nc = 1,
                           aI = 200,
                           ntries = 5,
                           block.name      = "alpha+b",
                           block.samp.type = "RW_block",
                           parallel = TRUE
                           ) {

  require(foreach)
  ## arg eval
  if (is.null(nb))
    nb <- round(ni / nt * .10, 0)
  stopifnot((ni - nb)/nt > 50)
  if (is.null(ncores))
    ncores <- min(nc, parallel::detectCores() - 1)

  block.name <- tolower(block.name)

  # RUN IN PARALLEL ---------------------------------------------------------
  if (ncores > 1 && parallel) {
    print(
      "ncores is greater than 1. Invoking multiple workers. Nimble messages/updates are suppressed in parallel mode.\n"
    )

    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    out <- foreach::foreach(
      i = 1:nc,
      .combine = list,
      .packages = c("nimble"),
      .multicombine = TRUE
    ) %dopar% {
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
            control = list(# adaptive = TRUE,
              tries = ntries,
              adaptInterval = aI)
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
      if (block.name == "all") {
        Rmodel.conf$removeSampler(c("alpha", "b"))
        Rmodel.conf$addSampler(
          c("alpha", "b"),
          type = block.samp.type,
          control =
            list(ntries = ntries),
          silent = TRUE
        )
      }

      # Build and compile
      Rmcmc  <- buildMCMC(Rmodel.conf)
      Cmcmc  <-
        compileNimble(Rmcmc, project = Rmodel, resetFunctions = TRUE)

      # Run
      results <- runMCMC(
        Cmcmc,
        niter = ni,
        samples = TRUE,
        samplesAsCodaMCMC = TRUE
      )

      # return temp obj
      return(results)
    }
    parallel::stopCluster(cl)
    names(out) <- paste0("chain_", seq_len(ncores))
} # end parallel processing
# browser()
  # NO PARALLEL PROCESSING --------------------------------------------------
  if (ncores == 1 | !parallel) {
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
          control = list(# adaptive = TRUE,
            tries = ntries,
            adaptInterval = aI)
        )
      }

      ## add block on alpha across all T
      Rmodel.conf$addSampler(
        target = paste("alpha[1:", constants$T, "]"),
        type = block.samp.type,
        control = list(adaptive = TRUE),
        adaptInterval = aI
      )
    } # end alpha + b add samplers
    if (block.name == "all") {
      Rmodel.conf$removeSampler(c("alpha", "b"))
      Rmodel.conf$addSampler(
        c("alpha", "b"),
        type = block.samp.type,
        control =
          list(ntries = ntries),
        silent = TRUE
      )
    } # end block all samplers

    # Build and compile
    Rmcmc  <- buildMCMC(Rmodel.conf)
    Cmcmc  <-
      compileNimble(Rmcmc, project = Rmodel, resetFunctions = TRUE)

    # Run
    out <- runMCMC(
      Cmcmc,
      niter = ni,
      samples = TRUE,
      samplesAsCodaMCMC = TRUE
    )
  }  # END NO PARALLEL PROCESSING

return(out)

} #END FUNCTION
