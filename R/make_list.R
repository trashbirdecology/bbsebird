#' Make a List With Named
#' Take multiple objects from the global environment and place into a single list.
#' @param input character vector names of data objects in the workspace to be included in the outputted list
#' @export make_list
make_list <- function(input){

new.list <- list()
for(i in seq_along(input)){
  new.list[[i]] <- eval(parse(text=paste(input[i])))
  # doing this inside loop to prevent issues where data DNE
  names(new.list)[[i]] <- input[i]
}
names(new.list) <- input

return(new.list)

}
