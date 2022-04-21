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
                        zerofill = TRUE,
                        years = NULL,
                        countries = c("US", "CA"),
                        states    = NULL,
                        species = NULL,
                        protocol = c("Traveling", "Stationary"),
                        remove.bbs.obs = TRUE,
                        max.effort.km = NULL,
                        max.effort.mins = NULL,
                        max.num.observers = NULL
                        ){

# EVAL ARGS ----------------------------------------------------------
dir.create(dir.out, showWarnings = FALSE)
countries <- toupper(countries)
stopifnot(is.logical(zerofill))
stopifnot(is.logical(remove.bbs.obs))

# CREATE LISTS FOR SUBSETTING ---------------------------------------------
f.equal <-
  list(
    "COUNTRY CODE" = countries,
    "STATE CODE" = states,
    "ALL SPECIES REPORTED" = ifelse(zerofill, c(1, TRUE), c(0, FALSE)),
    "PROTOCOL TYPE" = protocol)
less.equal<-list(
    "EFFORT DISTANCE KM" = max.effort.km,
    "DURATION MINUTES" = max.effort.mins,
    "NUMBER OBSERVERS" = max.num.observers

  )
range.equal<-list(
    "OBSERVATION DATE" = years
)
more.equal <-list(NULL)

filters <- list("equal"=f.equal, "less"=less.equal, "range"=range.equal, "more"=more.equal)
filters <- lapply(filters, function(x){
  x <- x[!unlist(lapply(x, is.null))]
})

# IMPORT & FILTER OBS + SAMP EVENTS------------------------------------------------------------------
# tictoc::tic()
message("!!keep an eye on memory usage. this is where shit gets sticky...\n")
fns <- list(observations=fns.obs, samplingevents=fns.samps)
tictoc::tic("FILTER THEN RBIND")
for(i in seq_along(fns)){
  fs    <- fns[[i]]
  type  <- names(fns)[i]
  if(i==1) fns.grab <- NULL
  myfn <- paste0(dir.out, "filtered_", type, ".csv.gz")
  fns.grab <- c(fns.grab, myfn) ## save this for grabbing later on...
  if (file.exists(myfn) &&
      !overwrite) {
    message(myfn," exists and overwrite = FALSE. Not overwriting the filtered ", type, "data. \n");
    next()
  }
  ## import files
  cat("importing and filtering on", type," files:\n\n", paste0(fs, sep="\n"),"\nthis may take a while...\n")
  for(ii in seq_along(fs)){
    if(ii==1) data<- vector("list", length=length(fs))
    x <- fs[ii]
    DT <-
      # <- rbindlist(lapply(fs, function(x) {
      data.table::fread(x,
                        nThread = ncores,
                        # nrows = 1e2, ## FOR DEV PURPOSES
                        fill=FALSE,
                        drop=c("SPECIES COMMENTS","V48", "TRIP COMMENTS", "REASON", "REVIEWED", "HAS MEDIA", "AGE/SEX"))
    # }))
    cat("...import success. jagshemash!\n")
    # subset by filter types
    for(k in seq_along(filters)){
      filt.ind <- tolower(names(filters)[k])
      filt.temp <- filters[[k]][names(filters[[k]])  %in% toupper(colnames(DT))] ##keep only those relevnat to file (obs vs samp)
      if(length(filt.temp)==0) next()
      for(j in seq_along(filt.temp)){
        f <- as.vector(unlist(filt.temp[j]))
        n <- names(filt.temp)[j]
        # set key
        eval(parse(text=paste0("setkey(DT,`", n ,"`)")))
        # filter
        if(filt.ind == "equal") DT <- DT[eval(parse(text=paste0("`",n,"`"))) %in% f]
        if(filt.ind == "less")  DT <- DT[eval(parse(text=paste0("`",n,"`"))) <= f]
        if(filt.ind == "more")  DT <- DT[eval(parse(text=paste0("`",n,"`"))) >= f]
        if(filt.ind == "range") {
          DT <- DT[eval(parse(text=paste0("`",n,"`"))) >= min(f)]
          DT <- DT[eval(parse(text=paste0("`",n,"`"))) >= max(f)]
        }
        # remove key
        data.table::setkey(DT, NULL)
        cat("\n\tend ", names(fns)[i]," i-loop",i,  "& j-loop", j ,nrow(DT) , "rows remain after", names(filt.temp)[j], "filter")
      }#end j loop one fitler type
    } # end k loop all filters
    if(ii==1) data <- vector("list", length=length(length(fs)))
    data[[ii]] <- DT
    rm(DT)
    if(i==2) browser()
  } # end ii loop
  length(data)==length(fs)
  cat("writing the filtered ", type,  "to in the process stops mid-way.......:\n", myfn)
  data.table::fwrite(x = rbindlist(data), file = myfn, nThread = ncores)
  rm(data) # clear from mem.
}# end i loop
tictoc::toc()


# Import the filtered and combine and do zero-filling.. -------------------



# MERGE -----------------------------------------------------------------

colnames(data$observations)[which(colnames(data$observations) %in%  colnames(data$samplingevents))]
colnames(data$samplingevents)[!which(colnames(data$samplingevents) %in%  colnames(data$observations))]
colnames(data$observations)[!which(colnames(data$observations) %in%  colnames(data$samplingevents))]

# END FUN -----------------------------------------------------------------


return(out)
} # END FUNCTION
