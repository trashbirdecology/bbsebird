#' Import the Prepared RDS Data For Use in JAGS
#' @param dir.jags Path name to where the jags data files (.rds) are stored
#' @param filename Name of the file to import.
#' @export import_jdat
import_jdat <- function(dir.jags, filename="jdat.rds") {

fn <- list.files(path = dir.jags, full.names = TRUE, "jdat.rds")
if(length(fn)>1)  "more than one filename in object `fn`. check filenames in dir.jags"
if(length(fn)==0) stop("no files named jdat.rds exist in dir.jags" )

jdat <- readRDS(fn)

expr <- paste(strsplit(names(jdat), split = "#"),
              collapse = "\n",
              sep="#")

cat("jdat is of class", toupper(class(jdat)) , "and contains", length(jdat), "elements:\n", expr)
return(jdat)
}
