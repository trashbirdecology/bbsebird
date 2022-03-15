#' @title Try to Save Object as RDS
#' @param x object to save to file as .RDS
#' @param name object name (e.g., plots-nimble or jags_samples)
#' @param savedir directory within which object will attempt to save
#' @param mcmc.specs if provided, the MCMC specs will be used to save output files#'
#' @export trySaveResults

trySave <- function(x,
                    name,
                    savedir = "/",
                    mcmc.specs = NULL) {
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

  # if(tolower(x)=="plots"){
  #   ## save non plot objects
  #   tryCatch(
  #     {
  #     sink(paste0(out.fn, ".pdf"))
  #          plot(results.jags)
  #     sink()
  #     }
  #     error = function(e) {
  #       cat("failed to save traceplots to file\n")
  #     }
  #   )
  #
  #
  # }

  ## save non plot objects
# if(tolower(x)!="plots"){
  tryCatch(
    saveRDS(x, file = paste0(out.fn, ".rds")),
    error = function(e) {
      cat("failed to save output to file. Be sure to save the output manually!\n")
    }
  )
  # }






} # end function
