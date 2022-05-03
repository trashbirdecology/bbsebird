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
                        max.birds.checklist = 55,
                        max.num.observers = 10,
                        complete.only = TRUE,
                        ncores=NULL,
                        ydays = NULL,
                        overwrite=FALSE
                        ){

# EVAL ARGS ----------------------------------------------------------
dir.create(dir.out, showWarnings = FALSE)
countries <- toupper(countries)
stopifnot(is.logical(zerofill))
stopifnot(is.logical(remove.bbs.obs))
if(is.null(ncores))ncores <- parallel::detectCores()-1
setDTthreads(ncores)

# CREATE LISTS FOR SUBSETTING ---------------------------------------------
f.equal <-
  list(
    "COUNTRY CODE" = countries,
    "STATE CODE" = states,
    "ALL SPECIES REPORTED" = ifelse(complete.only, c(1, TRUE), c(0, FALSE)),
    "PROTOCOL TYPE" = protocol)
less.equal <- list(
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


# SEE IF MUNGED DATA EXISTS AND IMPORT ------------------------------------
fn.out <- paste0(dir.out, "munged_ebird_data", ".csv.gz")

if(file.exists(fn.out) && !overwrite){
  cat("Munged data exists and overwrite=FALSE. Importing previously munged eBird data...\n",fn.out,"\n")
  data <- data.table::fread(fn.out, nThread = ncores)
}else{

# IMPORT & FILTER OBS + SAMP EVENTS------------------------------------------------------------------
fns <- list(observations=fns.obs, samplingevents=fns.samps)
# tictoc::tic()

dataout<-data<-list(NULL)
tictoc::tic("FILTER THEN RBIND")
for(i in seq_along(fns)){
  fs    <- fns[[i]]
  type  <- names(fns)[i]
  if(i==1) myfns <- NULL
  myfns  <- c(myfns, paste0(dir.out, "filtered_", type ,".csv.gz"))
  if(file.exists(myfns[i])&&!overwrite){
    message("file ", myfns[i], " exists. Not overwriting existing data while overwrite=FALSE.\n")
    next()
    }
  message("!!keep an eye on memory usage. this is where shit gets sticky...\n")

  ## import files
  cat("importing and performing initial filtering on", type," files:\n\n", paste0(fs, sep="\n"),"\nthis may take a while...\n")
  for(ii in seq_along(fs)){
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
          DT <- DT[eval(parse(text=paste0("`",n,"`"))) <= max(f)]
        }
        # remove key
        data.table::setkey(DT, NULL)
        cat("\n\tend ", names(fns)[i], " loop ", k,"-",j,"-",ii, nrow(DT) , "rows remain after", names(filt.temp)[j], "filter")
      }#end j loop one fitler type
    } # end k loop all filters
    if(ii==1) data<-vector("list", length(fs))
    data[[ii]] <- DT
    rm(DT)
  } # end ii loop
  # browser()
  cat("\nwriting the filtered ", names(fns)[i], "to file in case your machine crashes....:\n", myfns[i],"\n")
  data.table::fwrite(rbindlist(data), file = myfns[i], nThread = ncores)

  rm(data) # empty data list for next i
}# end i loop
tictoc::toc()
gc()


# IMPORT FILTERED FILES ---------------------------------------------------
names(myfns) <- names(fns)## filtered data filenames
data <- vector("list", length(myfns)); names(data) <- names(myfns)
cat("importing the filtered observations and sampling events data (", length(myfns),"files)\n")
## not doing this in parallel because of potential memory crashes on non HPC
for(i in seq_along(data)){
  data[[i]] <- data.table::fread(file = myfns[i], nThread = ncores)#, verbose = TRUE)
}


# COMBINE -----------------------------------------------------------------
cat("binding the filtered datasets....\n")
data <- data.table::rbindlist(data, fill=TRUE)
gc()


# FILTER YDAYS ------------------------------------------------------------
cat("filtering data by ydays arg...\n")
if(!is.null(ydays)) data <- data[yday(`OBSERVATION DATE`) %in% ydays]


# REMOVE presence-only ----------------------------------------------------
## i want to see how much time is saved if igrab row numbers from a vector then only grab those rows
cat("removing presence-only data (i.e., 'OBSERVATION COUNT' == X) & zero-filling data...")
# data <- data[,`OBSERVATION COUNT`!="X"]
data <- data[`OBSERVATION COUNT`!="X"]
if (zerofill) {
  data <- # pretty sure the reassignment isnt necessary whwen ":=" is in place...
    data[, `OBSERVATION COUNT` := ifelse(is.na(`OBSERVATION COUNT`),
                                         0,
                                         as.integer(`OBSERVATION COUNT`))] ## change NAs to ZERO and THEN force to integer
}

if(!is.null(max.birds.checklist)){
  data <- data[`OBSERVATION COUNT` <= max.birds.checklist]
}

## If i convert to integer before remocving "X", the "X" goes to NA so don't do that first!
cat("....done\n")
cat("Taking out the garbage because this data can be massive.....\n")
gc()

# remove BBS obs ----------------------------------------------------------
if(remove.bbs.obs){
  cat("Removing what are suspected to be BBS observations. \n")
  bbsdays <- 152:188 # a liberal removal of ebird based on instructions: https://www.pwrc.usgs.gov/bbs/participate/BBS%20Instructions.pdf
    ### ideally this would be adjusted to account for location (latitude)
  k <- which(which(data$`PROTOCOL TYPE` == "Stationary") %in% which(data$`DURATION MINUTES` == "3"))
  k <- which(k %in% which(yday(data$`OBSERVATION DATE`) %in% bbsdays))
  data <- data[!k]
  rm(k, bbsdays)
}
### waiting for Dave Z to send ideal dates for bbs routes to narrow down potential BBS obs....

# Keep One Checklist from Each Group Event ID --------------------------------------------------
blanks    <- data[`GROUP IDENTIFIER`==""]
noblank   <- unique(data[`GROUP IDENTIFIER`!=""], by = c("GROUP IDENTIFIER"))
data   <- data.table::rbindlist(list(blanks, noblank))
rm(blanks, noblank)
gc()


# Handle column names -----------------------------------------------------
data <- munge_col_names(data)

# Random Light Munging ----------------------------------------------------
## remove any column where all values are NA (this is mostly just for rouge "country" variable)
data <- data[,which(unlist(lapply(data, function(x)!all(is.na(x))))), with=FALSE]

## For non-traveling protocol, force effiort_distance_lkm to zero
data[,effort_distance_km := ifelse(protocol_type != "Traveling",
                                   0, effort_distance_km)]

data[,year  := year(date)]
data[,yday  := yday(date)]
data[,month := month(date)]



# Save it .... -------------------------------------------------
cat("saving munged data to file:\n  ", fn.out, "\n")
data.table::fwrite(data, file = fn.out)

} ## END if data DNE then munge it...

# END FUN -----------------------------------------------------------------
return(data)
} # END FUNCTION
