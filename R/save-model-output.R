#' Ensure JAGS Model Outputs are Saved 
#' Sometimes when working in OneDrive/Dropbox or on company networks, you may ahve problems saving to file. Running this instead of directly saving will ensure model output (.rds) is saved to an alternate location. This may be handy for running models in parallel/loops.
#' The function returns the output filename. Will save to a directory within the user's Desktop (windows, mac) if alt.save.dir is unspecified
#' 
#' 
#' @export trySaveModel
trySaveModel <-
  function(obj,
           obj.name="mymodel", 
           save.dir, 
           alt.save.dir = paste0(file.path(Sys.getenv("USERPROFILE"), "Desktop"), "/JAGS.outputs/"),
           alt.save.subdir = NULL
           
  ) {
    error.index=FALSE
      fn.out <- paste0(save.dir, obj.name, ".rds")
      tryCatch(saveRDS(object = obj, file = fn.out),
               error = function(e) error.index=TRUE)
                      
if(error.index==TRUE){ alt.save.subdir <- paste0(alt.save.dir, subdir.proj)
      dir.create(alt.save.dir, alt.save.subdir)
      fn.out <- paste0(alt.save.dir, "/", modname, ".rds")
      saveRDS(out, fn.out)
}

    message("saving to ", fn.out, "/n")
    return(fn.out)
  }
