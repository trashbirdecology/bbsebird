#' @title Run JAGS Model
#'
#'
#' @param jdat Data input to JAGS
#' @param mcmc.specs Specifications for MCMC chains. Should be created using bbsebird::set_mcmc_species. If  not provided will default to default values of bbsebird::set_mcmc_specs
#' @param inits Initial values for a single chain. If initial values are not provided for all chains, this function will repeat the intiial values provided .
#' @param mod.fn Local file path for JAGS model file.
#' @param mod.name Model name. Used to name output files.
#' @param model.out.dir Directory path where to save output files (e.g., mod.name.RDS)
#' @param plot.dir Directory path where to save output figures.
#' @param params.monitor Vector List of parameters to monitor.
#' @param plot logical If TRUE will produce default traceplots of model output
#' @param use.dclone logical If TRUE will use dclone::jags.parfit to fit model in JAGS. See package dclone for more details.
#' @param view.plot logical IF TRUE will open the plot saved to file
#' @param overwrite logical If FALSE and mod.name.RDS already exists in model.out.dir, will prompt user to confirm whether they wish to import existing model outputs or overwrite.
#' @importFrom tictoc tic toc
#' @importFrom jagsUI jags traceplot
#' @importFrom parallel makePSOCKcluster
#' @export run_model

run_model <- function(
  jdat,
  mod.fn,
  mod.name         = "mymodel",
  model.out.dir    = NULL,
  inits            = NULL,
  mcmc.specs       = set_mcmc_specs(),
  use.dclone       = TRUE,
  params.monitor   = NULL,
  make.plot        = FALSE,
  plot.dir         = NULL,
  view.plot        = FALSE,
  overwrite        = FALSE
  # saveoutput       = FALSE
){
# Arg check
## jags model exists?
stopifnot(file.exists(mod.fn))
## all mcmc specs specified?
stopifnot(all(c("na", "nb", "nc", "ni", "nt", "ncores") %in% names(mcmc.specs)))
## dclone
if(use.dclone){
  if(!nzchar(system.file(package = "dclone"))){
  # if(!"dclone" %in% .packages(all.available = TRUE)){ # this doesn't function as expected.
    choice <- menu(title = "Package 'dclone' is required when 'use.dclone'==TRUE.\n
                   Would you like to install dclone?\n",
                   choices = c("yes, install package","no, do not use dclone."))
   if(choice==1){install.packages("dclone")}
   if(choice==2){message("dclone not installed but 'use.dclone' == TRUE. To avoid this message, please use 'use.dclone=FALSE' or install dclone package.");
      use.dclone=FALSE}
    } # end check dclone edxists.
} # end use.dclone arg check
## directories
if(!is.null(plot.dir))      dir.create(plot.dir, showWarnings = FALSE)
if(!is.null(model.out.dir)) dir.create(model.out.dir, showWarnings = FALSE)

# check if model file(s) already exist.
modoutfn <- paste0(model.out.dir,mod.name ,".rds")
if (file.exists(modoutfn) & !overwrite) {
    choice.runmod <-
      menu(
        title = paste0(
          "file ",
          fn,
          " exists. Do you wish to overwrite (this may take hours..)?\n"
        ),
        choices = c(
          "Yes, re-run model (not in parallel!) and overwrite existing file",
          "No, definitely not!"
        )
      )
  } else{
    choice.runmod = 1
  }

if(choice.runmod==2){out <- message("importing the previously-saved model from file: ", modfn)
return(out)
}


# INITS -------------------------------------------------------------------
# repeat inits
if(!is.null(inits)) inits <- make_inits_list(inits, nc = mcmc.specs$nc)

# RUN MODEL ---------------------------------------------------------------
tictoc::tic()

if(!use.dclone){
out <- jagsUI::jags(
  data  = jdat,
  model.file = mod.fn,
  inits = inits,
  parameters.to.save = params.monitor,
  n.chains = mcmc.specs$nc,
  n.thin = mcmc.specs$nt,
  n.iter = mcmc.specs$ni,
  n.burnin = mcmc.specs$nb
  )
  x = tictoc::toc()
  cat("runtime: ", (mod.time <- paste0(round(x$toc - x$tic, 2), " seconds")))
  out$tictoc.allchains <- mod.time # do not add this line to jags.parfit out object -- complicates plotting functions

} #end regular jags fit
if(use.dclone){
  cl <- parallel::makePSOCKcluster(3)
  parfit <- dclone::jags.parfit(
    cl = cl,
    data = jdat,
    model = mod.fn,
    # can this be a file or must it be char?
    inits = inits,
    params = params.monitor,
    n.chains = mcmc.specs$nc,
    n.iter = mcmc.specs$ni,
    n.ipdate = mcmc.specs$nb,
    n.adapt = mcmc.specs$na,
    thin = mcmc.specs$nt
  )

  x = tictoc::toc()
  cat("runtime: ", (mod.time <- paste0(round(x$toc - x$tic, 2), " seconds")))
} #end parfit

#
# save model output
model.saved <- FALSE
tryCatch( {
  message("attempting to save model to file: ", modoutfn); saveRDS(out, file=modoutfn)
  }, error = function(e) {model.saved <<- TRUE})
print(ifelse(model.saved, cat("model saved to ", modoutfn),
             cat("unable to save model to file.")

             ))

# PLOTS -------------------------------------------------------------------
# save plots only after model was saved in case fails.
## need to add tryCatch here to ensure function completes.
if(make.plot){
tp.fn <-
    paste0(
      plot.dir,
      mod.name,
      "_trace_",
      mcmc.specs$ni,
      "iters_",
      Sys.Date(),
      ".pdf"
    )
pdf(tp.fn)
if(use.dclone){plot(out$samples)}else{plot(out)}
dev.off()
}
if(make.plot & view.plot & file.exists(tp.fn)) browseURL(tp.fn)


# RETURN ------------------------------------------------------------------

return(out)

} # END FUNCTION

