#' Make List of Data and Constants for Nimble/JAGS
#'
#' @param data List of data objects and constants as produced by make_bundle
#'
#' @export make_model_data
make_model_data <- function(data) {
  ### i need to fix this funciton such that it first creates NEW data/indexes (e..g, nobsrb)
  ### then it grabs character vector of shit that already appears in `data`
  ### then it uses the pluck_multiple helper fun
  ## this is necessary because mke_bundle has toption to pick and choose variables.
  ### eventaully i will need to add option to keep only the coviartes i want inside an array such taht
  ### we can loop over the linear model additive site- and grid-level covariates in hte JGS/nimb model
  new <- list(
    nobsb     = nrow(data$bbs.df),
    nobse     = nrow(data$ebird.df),
    rteobs    = data$bbs.df$rteobs.ind,
    nrteobs   = length(unique(data$bbs.df$rteobs.ind)),
    # rteobs    = data$bbs.df |> dplyr::distinct(site.ind, obs.ind) |> dplyr::select(site.ind, obs.ind),
    # nrteobs   = nrow(data$bbs.df |> distinct(site.ind, obs.ind)),
    Cb        = data$bbs.df$c,
    Ce        = data$ebird.df$c,
    yearb     = data$bbs.df$year.ind,
    siteb     = data$bbs.df$site.ind,
    cellb     = data$bbs.df$cell.ind,
    yeare     = data$ebird.df$year.ind,
    sitee     = data$ebird.df$site.ind,
    celle     = data$ebird.df$cell.ind,
    nobsrb    = length(unique(data$bbs.df$obs.ind)),
    obsrb     = data$bbs.df$obs.ind,
    fyr       = data$Xb$obsfirstyearroute, ## change to obsfirstyearbbs if desired.
    asst      = data$Xb$assistant,
    # wind      = data$Xb[,,"windmean"],
    mins      = data$Xe$duration_minutes,
    party     = data$Xe$number_observers,
    # starttime = data$Xe[,,"time_observations_started"], ## this needs to be fixed -- comes in as a character...
    # dist      = data$Xe[,,"effort_distance_km"],
    # effarea   = data$Xe[,,"effort_area_ha"],
    # NN        = length(data$adj),  ## NOTE THIS OVERWRITES NN PRODUCEDBY MAKE_BUNDLE..NEED TO FIX IN PKG
    hab       = sqrt(abs(data$Xg$area))# made-up hab variable
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

