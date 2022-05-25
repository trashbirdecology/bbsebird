#' Run Nimble Model in Parallel
#' Function will run a Nimble model for 'nc' indepdendent chains
#' @param code nimble model code
#' @param data data as a list
#' @param constants constants as list
#' @param inits list of initial values
#' @param parallel logical if TRUE will run chains in parallel (using foreach).
#' @param ncores maximum number of cores to employ. Actual number used is the minimum of nc and ncores
#' @param monitors optional Character vector of parameters to monitor.
#' @param monitors2 optional Character vector of parameters to monitor.
#' @param ni number iterations to run
#' @param seed seed number
#' @param calculate logical if TRUE will calculate the model logprob. Used as argument 'calculate' in function nimble::nimbleModel()
#' @param nb number of burn-in iterations to discard (I think it's PRE-THINNING burnin discard...)
#' @param nt thinning rate (every Nth iteration will be saved)
#' @param nc number of chains to run (in parallel)
#' @param aI adapt interval, used in nimble::addSamplers
#' @param ntries optional If using parameter block sampler, specify the maximum number of tries. Defaults to ~50% of the number of GAM basis functions.
#' @param block.name optional one of c("alpha+b", "all"). If "alpha+b" will block each alpha and b across all T. If "all" will block all alpha and b for each Ts.
#' @param block.samp.type optional one of c("AF_slice", "RW_block").
#' @param dir.out path where samps and runtimes wll be saved
#' @param fn.times filename of runtimes output. Optional. Defaults to runtimes.csv
#' @param save.output Logical. If TRUE will attempt to save MCMC results output as .rds to file.
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom nimble compileNimble buildMCMC runMCMC nimbleModel configureMCMC runMCMC buildMCMC
#'
#' @export run_nimble_model

run_nimble_model <- function(code,
                             data,
                             constants = NULL,
                             inits,
                             monitors = NULL,
                             monitors2 = NULL,
                             ncores = NULL,
                             ni = 10000,
                             nb = 100,
                             nt = 1,
                             nc = 1,
                             aI = 200,
                             seed = NULL,
                             ntries = NULL,
                             calculate = FALSE,
                             block.name      = "alpha+b",
                             block.samp.type = "AF_slice",
                             parallel = TRUE,
                             mod.name = NULL,
                             dir.out = NULL,
                             fn.times = "runtimes.csv",
                             save.output = TRUE) {
  fn.ind <- substr(fn.times, nchar(fn.times) - 3, nchar(fn.times))
  if (!fn.ind %in% c(".csv")) {
    warning("arg fn.times should have file extension .csv")
    stop()
  }

  ## arg eval
  if (is.null(seed))
    seed <- sample(1:1e3, 1)

  if (!(tolower(block.samp.type) %in% c("af_slice", "rw_block")))
    block.name <- "none"
  if (!is.null(block.name))
    block.name <- tolower(block.name)
  if (is.null(nb))
    nb <- round(ni / nt * .25, 0)
  stopifnot((ni - nb) / nt > 50)
  if (is.null(ncores))
    ncores <- min(nc, parallel::detectCores() - 1)


  if (ncores > parallel::detectCores()) {
    ncores <- parallel::detectCores() - 1
    cat(
      "ncores requested is greater than available cores. Requesting only",
      ncores,
      "workers at this time."
    )
  }

  if (is.null(ntries)) {
    if (constants$K > 0) {
      ntries <-
        round(constants$K / 2, 0)
    } else{
      if (dim(constants$Z.mat)[2] > 0) {
        ntries <- round(dim(constants$Z.mat)[2] / 2, 0)
      }
    }
    if (is.null(ntries))
      ntries <- 10
  }


  # START RUNTIMES ----------------------------------------------------------
  t.build <-
    t.compile <-
    t.confmcmc <- t.buildcompwblock <- t.run <- t.tot <- NULL
  t.tot <- Sys.time() ## start tracking runtime

  # RUN IN PARALLEL ---------------------------------------------------------
  if (ncores > 1 && parallel) {
    print(
      "ncores is greater than 1. Invoking multiple workers. Nimble messages/updates are suppressed in parallel mode. Only total run time is captured in runtime savefile.\n"
    )
    ## need to add an output file for cluster logs
    # https://stackoverflow.com/questions/24327137/error-in-unserializesocklistn-error-reading-from-connection-on-unix
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
        calculate = calculate
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
        inits = inits,
        nchains = nc,
        thin = nt,
        nburnin = nb,
        samples = TRUE,
        setSeed = seed,
        samplesAsCodaMCMC = TRUE
      )

      # return temp obj
      return(results)
    }
    parallel::stopCluster(cl)
    names(out) <- paste0("chain_", seq_len(ncores))
  } # END PARALLEL PROCESSING
  # browser()
  # NO PARALLEL PROCESSING --------------------------------------------------
  if (ncores == 1 | !parallel) {
    t.build <- Sys.time()
    Rmodel   <- nimbleModel(
      code = code,
      data = data,
      constants = constants,
      inits = inits
    )
    t.build <- Sys.time() - t.build

    t.compile <- Sys.time()
    Rmodel.comp <- compileNimble(Rmodel)
    t.compile <- Sys.time() - t.compile

    ## configure MCMC algorithm
    t.confmcmc <- Sys.time()
    Rmodel.conf <- configureMCMC(
      Rmodel,
      monitors = monitors,
      thin = nt,
      nburnin = nb,
      calculate = calculate
    )
    t.confmcmc <- Sys.time() - t.confmcmc

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
    t.buildcompwblock <- Sys.time()
    Rmcmc  <- buildMCMC(Rmodel.conf)
    Cmcmc  <-
      compileNimble(Rmcmc, project = Rmodel, resetFunctions = TRUE)
    t.buildcompwblock <-
      round(as.numeric(Sys.time() - t.buildcompwblock), 2)

    # Run
    t.run <- Sys.time()
    out <- runMCMC(
      Cmcmc,
      niter = ni,
      thin = nt,
      nburnin = nb,
      samples = TRUE,
      samplesAsCodaMCMC = TRUE
    )
    t.run <- round(as.numeric(Sys.time() - t.run), 2)
  }  # END NO PARALLEL PROCESSING


  # END RUNTIMES -----------------------------------------------------------
  t.tot <- Sys.time() - t.tot
  temp <- attributes(t.tot)$units
  if (temp == "secs")
    t.tot <- round(t.tot / 60, 0)  # convert from hr to mins
  if (temp == "hours")
    t.tot <- round(t.tot * 60)    # convert from hr to mins
  if (temp == "days")
    t.tot <- round(t.tot * 60 * 24) # convert from days to mins
  ### write the runtimes to file
  times <- data.frame(
    totalmins = as.numeric(t.tot),
    name = mod.name,
    nbfs = constants$K,
    build = t.build,
    comp  = t.compile,
    configmcmc = t.confmcmc,
    buildcompwblock = t.buildcompwblock,
    runmod   = t.run,
    parallel = parallel,
    niters = ni,
    nchains = nc,
    nburnin = nb,
    nthin = nt,
    dir = dir.out,
    OS    = paste(Sys.info()[1:2], collapse = "_")
  )
  if (ifelse(file.exists(fn.times), FALSE, TRUE)) {
    write.csv(times, fn.times, row.names = FALSE)
  } else{
    timesin  <- read.csv(fn.times)
    line     <- times[1, ]
    try(timesout <- rbind(timesin, line))
    try(write.csv(timesout, fn.times, row.names = FALSE))
  }

  if (save.output) {
    try(saveRDS(out,  paste0(dir.out, "/samps/", fn, ".rds")))
  }

  return(out)


} #END FUNCTION
