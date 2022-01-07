#' Make lists of data objects for use in JAGS
#' @param dat a single or a list of data objects to munge into JAGS data.
#' @export make_jags_list
make_jags_list <- function(dat){


  dat <- bbs_spatial
  dat.list <- list(ebird_spatial, bbs_spatial, grid)


  if("list" %in%  class(dat)){
    dat.type.ind <- "list"
    len=length(dat)
  }
  if(any(c("tibble", "tbl", "data.frame", "data_frame") %in% class(dat))){
  if ("sf" %in% class(dat)) {dat <- sf::st_drop_geometry(dat)}
    dat.type.ind <- "df"
    len=1
  }

  for(i in 1:len){
    # grab first (if releavnat) data object

    print(substitute(dat))


  }



  ## BBS
  if(dat.type=="bbs"){

    p.bbs.wind    <- make_array(bbs, val="wind.z")
    p.bbs.cars    <- make_array(bbs, val="car.z")
    p.bbs.noise   <- make_array(bbs, val="noise.z")
    p.bbs.yday    <- make_array(bbs, val="yday")
    p.bbs.jday    <- make_array(bbs, val="julian")
    p.bbs.obsfirstyearbbs     <- make_array(bbs, val="obsfirstyearbbs")
    p.bbs.obsfirstyearroute   <- make_array(bbs, val="obsfirstyearroute")




  }



}
