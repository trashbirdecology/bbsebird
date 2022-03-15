# FUNCTION TO BE MOVED IN BBSEBIRD
#' Run BUGS Model in nimble
#' @param myData list List of elements comprising data (and constants if not provided separately) for use in nimble
#' @param myInits list List of initial values.
#' @param parallel logical If TRUE will run in parallel
#' @param constants optional Can be provided separately from myData. If both are provided, function will combine prior to inclusion in readNimbleModel
#' @param myModel nimbleCode object or filepath to a BUGS model.
#' @param mcmc.specs list List of specs for use in MCMC specification (e.g., n.chains, n.adapt, n.cores). If any necessary values are missing, will use set_mcmc_specs to grab a default value.
#' @param seed optional If specified will set a seed for random number generation.
#' @param savedir If not specified, will save the samples resulting from nimbleUI::nimble() to current working directory
#' @param verbose logical Argument used in nimble::compileNimble(showCompilerOutput). If TRUE will generate more messages/information during sampling phase. Note: If parallel=TRUE, messages are suppressed given behavior of parallel compute.
#' @importFrom parallel detectCores
#' @param mod.name optional Used to save model output to file. Defaults to 'mynimbleModel'
#' @param monitor optional Character vector of parameters to monitor.
#' @importFrom  parallel makeCluster parLapply stopCluster
#' @importFrom  doParallel registerDoParallel
#' @importFrom foreach %dopar%
#' @export run_in_nimble

run_in_nimble <- function(myData,
                          myModel,
                          myInits,
                          constants   = NULL,
                          monitor,
                          savedir     = "/",
                          seed        = sample(1:111111, size = 1),
                          parallel    = TRUE,
                          verbose     = TRUE,
                          mod.name    = "mynimbleModel",
                          mcmc.specs  = set_mcmc_specs(dev.mode = TRUE)) {
  # Step -1. arg checks --------------------------------------------------------------
  stopifnot(is.logical(parallel))
  stopifnot(is.logical(verbose))
  stopifnot(is.list(myData))
  stopifnot(is.list(myInits))
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
      names(mcmc.specs)[j] <- new.name
    }
    rm(default.vals, new.name, i, j)
  }


  stopifnot(is.integer(seed) || is.numeric(seed))
  set.seed(seed)

  stopifnot(is.character(params))

  if (!is.null(constants))
    myData <- c(myData, constants)

  stopifnot(typeof(myModel) == "language" |  file.exists(myModel))

  if (parallel) {
    this_cluster <- this_cluster <- parallel::makeCluster(mcmc.specs$ncores)
  } else{
    this_cluster <- this_cluster <- parallel::makeCluster(1)
  }

  # RUN MODEL ---------------------------------------------------------------
  verbose.ind<-verbose
  specs <- mcmc.specs
  chain_output <-
    parallel::parLapply(
      cl = this_cluster,
      X = 1:mcmc.specs$ncores,
      fun = run_MCMC_allcode,
      data = myData,
      model = myModel,
      mcmc.specs = specs,
      inits = myInits,
      verbose=verbose.ind # to avoid recusive args
    )
  ## close connection

    try({
      cat("Attempting to stop cluster\n")
      stopImplicitCluster()        # package: `doParallel`
      stopCluster(this_cluster) # package: `parallel`
    })

  # Save Results to File ----------------------------------------------------------
  if (!endsWith(savedir, "/"))
    savedir <- paste0(savedir, "/")
  dir.create(savedir, showWarnings = FALSE)
  results.out.fn <- paste0(
    savedir,
    mod.name,
    "-nimble-",
    mcmc.specs$ni,
    "ni_",
    mcmc.specs$na,
    "na_",
    mcmc.specs$nt,
    "nt_",
    mcmc.specs$nb,
    "nb",
    ".RDS"
  )
  ### need to add a tryCatch


  # RETURN OBJ --------------------------------------------------------------
  try({
    cat("attempting to save nimble output to file: ", results.out.fn)
    saveRDS(chain_output, file = results.out.fn)
  })

  return(chain_output)

}
