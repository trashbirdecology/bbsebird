#' @title Try to Save Results or plot(results) to File
#' @param x object to save to file as .RDS or as .PDF
#' @param name object name (e.g., plots-nimble or jags_samples)
#' @param savedir directory within which object will attempt to save
#' @param mcmc.specs if provided, the MCMC specs will be used to save output files
#' @param traceplots logical If TRUE will save plots to PDF
#' @export trySave
#' @importFrom coda as.mcmc
#' @importFrom mcmcplots mcmcplot


trySave <- function(x,
                    name,
                    savedir = "/",
                    mcmc.specs = NULL,
                    traceplots = FALSE,
                    monitors="all") {
  if (!endsWith(savedir, "/")) {
    savedir <- paste0(savedir, "/")
  }
  dir.create(savedir, showWarnings = FALSE)

  if (!is.null(mcmc.specs)) {
    middleparts <- paste0(
      mcmc.specs$ni,
      "ni_",
      mcmc.specs$na,
      "na_",
      mcmc.specs$nt,
      "nt_",
      mcmc.specs$nb,
      "nb"
    )
  } else{
    middleparts = NULL
  }

  out.fn <- paste0(savedir,
                   name,
                   "_",
                   middleparts)

  if(traceplots){
    ## save non plot objects
    tryCatch({
        pdf(paste0(out.fn,"_monitor-summary.pdf"))
        # MCMCvis::MCMCplot(results.nimb)
        MCMCvis::MCMCplot(results.nimb, params=monitors)
        dev.off()
        }, error = function(e) {
        cat("failed to save traceplots to file\n")
      }
    )
  }

if(!traceplots){
  tryCatch(
    saveRDS(x, file = paste0(out.fn, ".rds")),
    error = function(e) {
      cat("failed to save output to file. Be sure to save the output manually!\n")
    }
  )
  }


message("   [note] check ", out.fn, " for saved outputs.\n\n")



} # end function
