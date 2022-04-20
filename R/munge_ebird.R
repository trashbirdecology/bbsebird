#' Munge eBird Data
#'
#' Filter and zero-fill ebird data.
#' @param fns.obs Filenames to observations files
#' @param fns.samps Filenames to sampling events file
#' @param dir.out Directory of where to save the filtered and munged data files
#' @export munge_ebird
munge_ebird <- function(fns.obs,
                        fns.samps,
                        dir.out,
                        complete.only = TRUE,
                        years = NULL,
                        countries = c("US", "CA"),
                        states    = NULL,
                        species = NULL,
                        protocol = c("Traveling", "Stationary"),
                        remove.bbs.obs = TRUE,
                        max.effort.km = NULL,
                        max.effort.mins = 360,
                        max.num.observers = 10
                        ){
## ARGS
dir.create(dir.out, showWarnings = FALSE)
countries <- toupper(countries)
## IMPORT SAMPLING
f.equal <-
  list(
    "COUNTRY CODE" = countries,
    "STATE CODE" = states,
    "ALL SPECIES REPORTED" = complete.only,
    "PROTOCOL TYPE" = protocol)
less.equal<-list(
    "EFFORT DISTANCE KM" = max.effort.km,
    "DURATION MINUTES" = max.effort.mins,
    "NUMBER OBSERVERS" = max.num.observers

  )
range.equal<-list(
    "OBSERVATION DATE" = years
)
filters <- list("equal"=f.equal, "less"=less.equal, "range"=range.equal)
filters <- lapply(filters, function(x){
  x <- x[!unlist(lapply(x, is.null))]
})
samps <- vector("list", 2)
fns <- list(obs=fns.obs, samps=fns.samps)
tictoc::tic()
for(i in seq_along(fns)){
   fs    <- fns[[i]]
  ## import files
   cat("importing files:", fs, sep="\n","this may take a while...\n")
   DT <- rbindlist(lapply(fs, function(x) {
      data.table::fread(x,
                        nThread = ncores,
                        # nrows = 1e2, ## FOR DEV PURPOSES
                        fill=FALSE,
                        drop=c("SPECIES COMMENTS","V48", "TRIP COMMENTS"))
    }))
    # subset by EQUAL TO filters
    filt.temp <- filters$equal[names(filters$equal)  %in% toupper(colnames(DT))]##keep only those relevnat to file (obs vs samp)
    for(j in seq_along(filt.temp)){
      f <- as.vector(unlist(filt.temp[j]))
      n <- names(filt.temp)[j]
      # set key
      eval(parse(text=paste0("setkey(DT,`", n ,"`)")))
      # filter
      DT <- DT[eval(parse(text=paste0("`",n,"`"))) %in% f]
      # remove key
      data.table::setkey(DT, NULL)
      cat("\n\t\tend j",j, " ", nrow(DT))
    }#end j loop EQUAL
  cat("\nEND i loop ", i,"..rows remaining:",  nrow(DT))
  samps[[i]] <- DT
  rm(DT)
}# end i loop
tictoc::toc()
# cl <- parallel::makeCluster(ncores)
# doParallel::registerDoParallel(cl, cores=ncores)
# library(foreach)
  # foreach::foreach(i=1:length(fns.samps), .combine="rbind")%dopar%{
# parallel::stopCluster(cl)
nrow(samps)



return(out)
} # END FUNCTION
