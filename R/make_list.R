#' #' Make a List With Named
#' #' Take multiple objects from the global environment and place into a single list.
#' #' @param input
#' #' @keywords internal
#' #' @noRd
#'
#' make_list <- function(input){
#'
#' new.list <- list()
#' for(i in seq_along(input)){
#'
#'   # if(exists(input[i], envir = env)){
#'         # keep <- c(keep, i)
#'         # new.list[[i]] <- get(input[i], envir=env)
#'         # new.list[[i]] <- eval(parse(text=paste(input[i])), envir=env)
#'         # names <- c(names, input[i])
#' x <- new.list[!sapply(new.list, is.null)]
#'   }
#'
#'   else{}
#' }
#' # drop empty lists
#' print("sdlkjsdkfj")
#' names(x) <- input[keep]
#'
#' return(x)
#'
#' }
#'
