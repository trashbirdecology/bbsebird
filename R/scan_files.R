#' Check For Existing Files
#'
#' Checks for existing files to import to avoid length data munging processes.
#'
#' @param scan.for which types of files to scan for. Must be one or more of c("jdat", "spatial","eBird.obs", "bbs.obs"). If multiples are provided and found, the returned objct will be a named list of those imported files.
#' @param dir.proj project directory, within which first level subdirectories may include c(jags, ebird, bbs, spatial)
#' @export scan_files

scan_files <- function(dir.proj,
                       scan.for = c("jags","ebird_obs", "bbs_obs", "grid", "bbs_spatial", "ebird_spatial")
                       ){

  x = list.files(dir.proj, full.names=TRUE)
  scan.for <- tolower(scan.for)

  ## grid
  if("grid" %in% scan.for){
        sd <- list.files(x[str_detect(x,"spatial")], full.names=TRUE)
        s <- sd[str_detect(sd, "grid.rds")]
        if(length(s)>0){grid <- readRDS(s)
        }else{grid <- NULL; cat('No file named "grid.rds" found in dir: \n', sd, '\n')}
    }else{grid<-NULL}

  ## ebird over spatial grid
  if("ebird_spatial" %in% scan.for){
    sd <- list.files(x[str_detect(x,"spatial")], full.names=TRUE)
      s <- sd[str_detect(sd, "ebird")]
      if(length(s)>0){ebird_spatial <- readRDS(s)
      }else{ebird_spatial <- NULL; cat('No file named "ebird_spatial.rds" found in dir: \n', sd, '\n')}
  }else{ebird_spatial<-NULL}

  ## bbs obs over spatial grid
  if("bbs_spatial" %in% scan.for){
      sd <- list.files(x[str_detect(x,"spatial")], full.names=TRUE)
      s <- sd[str_detect(sd, "bbs")]
      if(length(s)>0){bbs_spatial <- readRDS(s)}else{bbs_spatial <- NULL; cat('No file named "bbs_spatial.rds" found in dir: \n', sd, '\n')}
    }else{bbs_spatial<-NULL}

  ## "Raw" ebird observations data
  if("ebird_obs" %in% scan.for){
    sd <- list.files(x[str_detect(x,"ebird")], full.names=TRUE)
      s <- sd[str_detect(sd, "ebird")]
      if(length(s)>0){ebird_obs <- readRDS(s)}else{ebird_obs <- NULL; cat('No file named "TBA" found in dir: \n', sd, '\n')}
      }else{ebird_obs<-NULL}

  ## "Raw" observations data
  if("bbs_obs" %in% scan.for){
    sd <- list.files(x[str_detect(x,"bbs")], full.names=TRUE)
      s <- sd[str_detect(sd, "bbs_orig.rds")]
      if(length(s)>0){bbs_obs <- readRDS(s)}else{bbs_obs <- NULL; cat('No file named "bbs_orig.rds" found in dir: \n', sd, '\n')}
    }else{bbs_obs<-NULL}


    # JAGS Data
  if("jdat"%in% scan.for){
     sd <- list.files(x[str_detect(x,"jags")], full.names=TRUE)
      s <- sd[str_detect(sd, "jdat.rds|jagsdata.rds")]
      if(length(s)>0){jdat <- readRDS(s)}else{jdat <- NULL; cat('No file named "jdat.rds or jagsdata.rds" found in dir: \n', sd, '\n')}
    }else{jdat<-NULL}


  # All potential objects
  names <- c("ebird_obs", "bbs_obs", "grid", "ebird_spatial","bbs_spatial")
 # parse(eval(as.name(names[2]))
  # objs <- list(ebird_obs, bbs_obs, grid, ebird_spatial,bbs_spatial)

  out <- list()
  for(i in 1:length(names)){
    id <- names[i]
    obj <- eval(parse(text=id))
    if(is.null(obj))next()
    z=1+length(out)
    out[[z]] <- obj
    names(out)[z] <- names[i]
  }

  cat("contents of output object: ")
  cat(names(out), sep="\n")

  # Return object
  return(out)


}
