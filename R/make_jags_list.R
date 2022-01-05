#' Make a List for Use in JAGS 
#' @param input character vector names of data objects in the workspace to be included in the outputted list
#' 
#' @export make_jags_list
make_jags_list <- function(input){
  
jags.data <- list()
for(i in seq_along(input)){
  jags.data[[i]] <- eval(parse(text=paste(input[i])))
  # doing this inside loop to prevent issues where data DNE
  names(jags.data)[[i]] <- input[i] 
}
names(jags.data) <- input

return(jags.data)

}
