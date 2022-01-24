#' Create a List of Repeated Initial Values for each Chain
#' 
#' @param inits a list of initial values which will be repeated according to arg `nc`
#' @param nc number of chains (number of times the inits will be repeated)
#' @export make_inits_list
make_inits_list <- function(inits, nc=3){
inits.out <- list()
for(i in 1:nc) {
  inits.out[[i]] <- inits
}
stopifnot(length(inits.out)==nc)
return(inits.out)

}
