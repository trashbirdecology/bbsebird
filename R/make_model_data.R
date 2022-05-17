#' Make List of Data and Constants for Nimble/JAGS
#'
#' @param data List of data objects and constants as produced by make_bundle
#' @param site.cov.output one of c("vector", "array", "matrix"). Defines the format of the site-level covariates
#' @export make_model_data
make_model_data <- function(data, site.cov.output="vector") {

  stopifnot(site.cov.output %in% c("vector", "array", "matrix"))

  if(site.cov.output %in% "vector"){
    fyr        = data$bbs.df$obsfirstyearroute ## change to obsfirstyearbbs if desired.
    asst       = data$bbs.df$assistant
    mins       = data$ebird.df$duration_minutes
    party      = data$ebird.df$number_observers
    starttime  = data$ebird.df$starttime ## this needs to be fixed -- comes in as a character...
  }
  if(site.cov.output %in% "matrix"){
    fyr        = data$Xb$obsfirstyearroute ## change to obsfirstyearbbs if desired.
    asst       = data$Xb$assistant
    mins       = data$Xe$duration_minutes
    party      = data$Xe$number_observers
    starttime  = data$Xe$starttime ## this needs to be fixed -- comes in as a character...
  }

  ### i need to fix this funciton such that it first creates NEW data/indexes (e..g, nobsrb)
  ### then it grabs character vector of shit that already appears in `data`
  ### then it uses the pluck_multiple helper fun
  ## this is necessary because mke_bundle has toption to pick and choose variables.
  ### eventaully i will need to add option to keep only the coviartes i want inside an array such taht
  ### we can loop over the linear model additive site- and grid-level covariates in hte JGS/nimb model
  new <- list(
    nobsb     = nrow(data$bbs.df),
    nobse     = nrow(data$ebird.df),
    rteobs    = data$bbs.df$rteobsind,
    nrteobs   = length(unique(data$bbs.df$rteobsind)),
    # rteobs    = data$bbs.df |> dplyr::distinct(siteind, obsind) |> dplyr::select(siteind, obsind),
    # nrteobs   = nrow(data$bbs.df |> distinct(siteind, obsind)),
    Cb        = data$bbs.df$c,
    Ce        = data$ebird.df$c,
    yearb     = data$bbs.df$yearind,
    siteb     = data$bbs.df$siteind,
    cellb     = data$bbs.df$cellind,
    yeare     = data$ebird.df$yearind,
    sitee     = data$ebird.df$siteind,
    celle     = data$ebird.df$cellind,
    nobsrb    = length(unique(data$bbs.df$obsind)),
    obsrb     = data$bbs.df$obsind,
    fyr       = fyr, ## change to obsfirstyearbbs if desired.
    asst      = asst,
    mins      = mins,
    party     = party,
    starttime = starttime,
   # NN        = length(data$adj),  ## NOTE THIS OVERWRITES NN PRODUCEDBY MAKE_BUNDLE..NEED TO FIX IN PKG
    hab       = sqrt(abs(data$Xg[[1]]))# made-up hab variable
  )

  ## ensure matrices comprise numbers and not characters
  for(i in seq_along(new)){
    if(any(c("matrix", "array") %in% class(new[[i]]))){
      if(is.character(new[[i]][!is.na(new[[i]])])){
        c=ncol(new[[i]]); r = nrow(new[[i]])
        newvec <- as.numeric(new[[i]])
        new[[i]] <- matrix(newvec, nrow=r, ncol=c)
      }
    }
  } # end make new.mat

  ## if data already comprises elements made herein, then keep only the newly created ones...
  remove<- names(data)[names(data) %in% names(new)]
  data <- pluck_multiple(data, remove=remove)
  out <- c(new, data)
  return(out)
}

