nimble_run_wbugs <- function(data,
                             modfn,
                             inits       = NULL,
                             traceplots  = TRUE,
                             constants   = NULL,
                             monitors    = NULL,
                             savedir     = "outputs",
                             seed        = sample(1:111111, size = 1),
                             verbose     = TRUE,
                             mod.name    = "mynimbleModel"){


  # Compile model from bugs
  model <-
    nimble::readBUGSmodel(model = modfn, data = myData, inits = myInits)
  cmodel <- nimble::compileNimble(myModel, showCompilerOutput = verbose)
  

  # Plot graph
  fn.p <- paste0(savedir, "/", mod.name, "-graph",".pdf")
  message("printing model graph to ", fn.p)
  pdf(fn.p)
  model$plotGraph()
  dev.off()
  # browseURL(fn.p)
  
  # build MCMC functions, skipping customization of the configuration.
  if(is.null(monitors)) monitors <- model$getNodeNames(stochOnly = TRUE,
                                                       includeData = FALSE)
  mcmc <- buildMCMC(model,monitors = monitors)
  
  # compile the MCMC function via generated C++
  cmcmc <- compileNimble(mcmc)
  
  
  modelConf <- configureMCMC(myModel, print = TRUE)
  Cpump <- compileNimble(myModel)
  
  
    
  
    
  
    
  
  
  
  
  
  
  
  
}