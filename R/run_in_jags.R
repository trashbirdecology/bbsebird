# FUNCTION TO BE MOVED IN BBSEBIRD
#' Run BUGS Model in JAGS
#' @param bugs.data list List of elements comprising constants and data for use in JAGS
#' @param inits list List of initial values.
#' @param parallel logical If TRUE will run in parallel
#' @param model Filepath to file containing JAGS model
#' @param mcmc.specs list List of specs for use in MCMC specification (e.g., n.chains, n.adapt, n.cores). If any necessary values are missing, will use set_mcmc_specs to grab a default value.
#' @param seed optional If specified will set a seed for random number generation.
#' @param savedir If not specified, will save the samples resulting from jagsUI::jags() to current working directory
#' @param verbose logical Argument used in jagsUI::jags(). If TRUE will generate more messages/information during sampling phase. Note: If parallel=TRUE, messages are suppressed given behavior of parallel compute.
#' @param mod.name optional Used to save model output to file. Defaults to 'myJAGSModel'
#' @param overwrite logical If TRUE and .RDS file already exists, will prompt user with a menu to confirm model re-run. Specifying overwrite=TRUE will avoid that prompt.
#' @param monitor optional Character vector of parameters to monitor.
#' @param traceplots logical If TRUE will save traceplots to file
#' @importFrom parallel detectCores
#' @importFrom doParallel stopImplicitCluster
#' @importFrom jagsUI jags
#' @export run_in_jags

run_in_jags <- function(bugs.data,
                        model,
                        inits,
                        monitor,
                        savedir     = "/",
                        seed        = sample(1:111111, size = 1),
                        parallel    = TRUE,
                        verbose     = TRUE,
                        mod.name    = "myJAGSModel",
                        overwrite   = FALSE,
                        traceplots  = TRUE,
                        mcmc.specs  = set_mcmc_specs()) {
  # deal with unbinded vars

  # arg checks --------------------------------------------------------------
  stopifnot(is.logical(parallel))
  stopifnot(is.logical(verbose))
  stopifnot(is.list(bugs.data))
  stopifnot(is.list(inits))
  stopifnot(is.list(mcmc.specs))
  ### check mcmc.specs and fill in any missing values.


  mcmc.missing <-
    setdiff(c("na", "nb", "nc", "ni", "nt", "ncores"), names(mcmc.specs))

  if (length(mcmc.missing) > 0) {
    message(
      paste0(mcmc.missing, collapse = ", "),
      " missing from arg `mcmc.specs`. Using defaults from set_mcmc_spec() for missing values."
    )
    default.vals  <- set_mcmc_specs()
    for (i in seq_along(mcmc.missing)) {
    j = length(mcmc.specs) + 1
      new.name = mcmc.missing[i]

      mcmc.specs[[j]] <- default.vals[[new.name]]
      names(mcmc.specs)[j] <-new.name
    }
    rm(default.vals, new.name, i, j)
  }

  stopifnot(is.integer(seed) || is.numeric(seed))
  set.seed(seed)

  stopifnot(is.character(params))


# Out filepath + file search ----------------------------------------------
  if(!endsWith(savedir, "/")) savedir <- paste0(savedir, "/")
  dir.create(savedir, showWarnings = FALSE)
  results.out.fn <- paste0(savedir, mod.name, "-jags-",
                           mcmc.specs$ni, "ni_",
                           mcmc.specs$na, "na_",
                           mcmc.specs$nt, "nt_",
                           mcmc.specs$nb, "nb",
                           ".RDS")
  if(file.exists(results.out.fn) & !overwrite){
    choice <- menu(title=paste0("a jags file at the following location already exists. \nAre you sure you want to re-run JAGS?"),
                    choices = c("Yes, definitely", "No. Don't run!", "What?!"))
      message("Great choice. Importing existing results now...\n")
    if(choice != 1){results <- readRDS(results.out.fn)
    return(results)
    }
  }



  # Run JAGS ---------------------------------------------------------------
  ## if not parallel:
  message("began jags model at: ", timestamp())
  time1 <- Sys.time()
  if(!parallel){
  results <- jagsUI::jags(
    data = bugs.data,
    model.file = model,
    inits      = inits,
    parameters.to.save = params,
    n.chains   = mcmc.specs$nc,
    # n.adapt    = mcmc.specs$na,
    n.iter     = mcmc.specs$ni,
    n.burnin   = mcmc.specs$nb,
    n.thin     = mcmc.specs$nt)}
  ## if parallel
  if(parallel){
  results <- jagsUI::jags(
    data = bugs.data,
    model.file = model,
    inits      = inits,
    parameters.to.save = params,
    n.chains   = mcmc.specs$nc,
    # n.adapt    = mcmc.specs$na,
    n.iter     = mcmc.specs$ni,
    n.burnin   = mcmc.specs$nb,
    n.thin     = mcmc.specs$nt,
    parallel   = TRUE,
    n.cores    = mcmc.specs$ncores,
    seed       = NULL
  )
  try({
    # cat("Attempting to stop cluster\n")
    doParallel::stopImplicitCluster()
    # parallel::stopCluster()
  })

  } # end run jagsUI::jags()

  message("`finish` jags model at: ", timestamp())
  (time2 <- Sys.time()-time1)
  if(exists("results")) {
    results$runtime <- time2
    print(results$runtime)
  }


# SAVE FILES --------------------------------------------------------------
  # SAVE AND EXPORT RESULTS
  trySave(x = results, name = "jags-samps", savedir, mcmc.specs, traceplots =FALSE)

  # SAVE AND EXPORT PLOTS
  if(traceplots) trySave(x = results, traceplots=TRUE, name = "jags-trace", savedir, mcmc.specs)



# RETURN ------------------------------------------------------------------
  return(results)

}
