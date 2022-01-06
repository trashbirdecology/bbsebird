#' Check For Existing Files
#'
#' Checks for existing files to import to avoid length data munging processes.
#'
#' @param scan.for which types of files to scan for. Must be one or more of c("jdat", "spatial","eBird.obs", "bbs.obs"). If multiples are provided and found, the returned objct will be a named list of those imported files.
#' @param dir.proj
#' @export

scan_files <- function(dir.proj, scan.for = c("jags","eBird.obs", "bbs.obs", "grid", "bbs.spatial", "ebird.spatial")
                       ){

  x = list.files(dir.proj, full.names=TRUE)


    ## spatial grid
    if(tolower(scan.for)==c("grid")){
        sd <- list.files(fns[str_detect(x,"spatial")], full.names=TRUE)
        s <- sd[str_detect(sd, "grid.rds")]
        if(length(s)>0){grid <- readRDS(s)
        cat("Importing file ", s)
        }else{grid <- NULL; cat('No file named "grid.rds" found in dir: \n', sd)}
    }
    if(tolower(scan.for)==c("ebird.spatial")){
      sd <- list.files(fns[str_detect(x,"spatial")], full.names=TRUE)
      s <- sd[str_detect(sd, "ebird")]
      cat("Importing file ", s)
      if(length(s)>0){bbs_spatial <- readRDS(s)}else{bbs_spatial <- NULL; cat('No file named "ebird_spatial.rds" found in dir: \n', sd)}
    }
    if(tolower(scan.for)==c("bbs.spatial")){
      sd <- list.files(fns[str_detect(x,"spatial")], full.names=TRUE)
      s <- sd[str_detect(sd, "bbs")]
      cat("Importing file ", s)
      if(length(s)>0){bbs_spatial <- readRDS(s)}else{bbs_spatial <- NULL; cat('No file named "bbs_spatial.rds" found in dir: \n', sd)}
    }

    ## "Raw" ebird observations data
    if(tolower(scan.for)==c("ebird.obs")){
      sd <- list.files(fns[str_detect(x,"ebird")], full.names=TRUE)
      s <- sd[str_detect(sd, "ebird")]
      cat("Importing file ", s)
      if(length(s)>0){bbs_spatial <- readRDS(s)}else{bbs_spatial <- NULL; cat('No file named "" found in dir: \n', sd)}
    }

    ## "Raw" observations data
    if(tolower(scan.for)==c("bbs.obs")){
      sd <- list.files(fns[str_detect(x,"bbs")], full.names=TRUE)
      s <- sd[str_detect(sd, "bbs_orig.rds")]
      if(length(s)>0){bbs_orig <- readRDS(s)}else{bbs_orig <- NULL; cat('No file named "bbs_orig.rds" found in dir: \n', sd)}
    }


    # JAGS Data
    if(tolower(scan.for)==c("jags")){
      sd <- list.files(fns[str_detect(x,"jags")], full.names=TRUE)
      s <- sd[str_detect(sd, "jdat.rds|jagsdata.rds")]
      if(length(s)>0){jdat <- readRDS(s)}else{jdat <- NULL; cat('No file named "jdat.rds or jagsdata.rds" found in dir: \n', sd)}
    }



  # All potential objects
  objs <- c("ebird_obs", "bbs_obs", "grid", "ebird_spatial","bbs_spatial")
  ## see which were imported, if any
  # out <- objs[which(objs %in% ls())] # can use this if make_list fails
  out <- make_list(out)

  # Return object
  return(out)


}
