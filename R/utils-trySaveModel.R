#' #' Ensure JAGS Model Outputs are Saved
#' #' Sometimes when working in OneDrive/Dropbox or on company networks, you may have problems saving to file. Running this instead of directly saving will ensure model output (.rds) is saved to an alternate location. This may be handy for running models in parallel/loops.
#' #' The function returns the output filename. Will save to a directory within the user's Desktop (windows, mac) if alt.save.dir is unspecified
#' #' @param obj an R object to save to file as .rds. Under the this context, a JAGS or other model output object.
#' #' @param obj.name desired name of the object file name
#' #' @param save.dir directory location of where to attempt to save the output .rds file
#' #' @param alt.save.dir if object save fails to save to `save.dir`, will save on user's desktop (future package edits will save to package files)
#' #' @export trySaveModel
#' trySaveModel <-
#'   function(obj,
#'            obj.name="mymodel",
#'            save.dir,
#'            alt.save.dir = paste0(file.path(Sys.getenv("USERPROFILE"), "Desktop"), "/JAGS.outputs/")
#'
#'   ) {
#'     error.index=FALSE
#'       fn.out <- paste0(save.dir, obj.name, ".rds")
#'       tryCatch(saveRDS(object = obj, file = fn.out),
#'                error = function(e) error.index=TRUE)
#'
#' if(error.index==TRUE){
#'       dir.create(alt.save.dir)
#'       fn.out <- paste0(alt.save.dir, "/", modname, ".rds")
#'       saveRDS(out, fn.out)
#' }
#'
#'     message("saving to ", fn.out, "/n")
#'     return(fn.out)
#'   }
