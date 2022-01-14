#' Munge and Output a Single or List of Data Frames for Use in mgcv::jagam
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param dat.names character string or vector of strings for names of input data objects. If not supplied, the function will guess whether the data is ebird, bbs, or grid.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param fn.out Filename  of the object output as a .RDS file
#' @export make_gam_dat
make_gam_dat <- function(dat, drop.nas=FALSE, dir.out, fn.out="jagamdat"){
  # In case a single data frame is supplied, add a NULL list element (because im lazy and don't want to rewrite the fucntion rn)
  if(!"list" %in% class(dat)) dat <- list(dat)
  # Naming the list objects
  dat.names <- NULL
  for(i in 1:length(dat)){
    if(any(class(dat[[i]]) %in% c("sf", "spdf"))) dat[[i]] <- dat[[i]] %>% sf::st_drop_geometry()

      ind <-  colnames(dat[[i]])
      if("checklist_id" %in% ind) dat.names[i] <- "ebird"
      if("rteno" %in% ind) dat.names[i] <- "bbs"

      }#end naming for loop
  names(dat) <- dat.names

  # Initialize empty objects in case they aren't filled later we can still recall them without asking ls() or exists("")
  objs.bbs <- objs.grid <- objs.ebird <- NULL

ebird.out <- bbs.out <- NULL
for(i in seq_along(dat)){
  # print(i)
    ind <- names(dat)[i] # make lazy indicator for which data we are munging
    if(is.na(ind)) next()
    if(ind == "bbs"){
      bbs <- dat[[i]] %>%
        units::drop_units() %>%
        arrange(gridcellid, rteno, year)
      if("sf" %in% class(bbs)) bbs <- bbs %>% sf::st_drop_geometry()
      names(bbs) <- tolower(names(bbs))
      if(drop.nas){
        bbs <- bbs %>% filter(!is.na(c), !is.na(rteno))
      }

      cat("building bbs objects.....")
      lon    <- bbs$cell.lon.centroid
      lat    <- bbs$cell.lat.centroid
      y      <- bbs$c
      year   <- bbs$year
      site   <- bbs$rteno %>% as.factor()
      grid   <- bbs$gridcellid

      ## DEFINE OUTPUT OBJECTS FOR BBS
      objs.bbs <- c("lon",
                      "lat",
                      "year",
                      "y",
                      "site",
                      "grid"
      )

      ## Create a data frame for BBS data
      if(exists("objs.bbs")){
        bbs.out <- matrix(NA,
                      nrow=length(eval(parse(text=objs.bbs[1]))),
                      ncol=length(objs.bbs))
        for(j in seq_along(objs.bbs)){
          if(j==1){names<-NULL;keep=NULL}
          if(exists(objs.bbs[j])){
            keep <- c(keep, j)
            bbs.out[,j] <- get(objs.bbs[j])
            names <- c(names, objs.bbs[j])
          }
        }
        colnames(bbs.out) <- names
      }
      cat("done.\n")
      }
    if(ind == "ebird"){
      ebird <- dat[[i]] %>%
        units::drop_units() %>%
        arrange(gridcellid, checklist_id, year)
      if("sf" %in% class(ebird)) ebird <- ebird %>% sf::st_drop_geometry()
      names(ebird) <- tolower(names(ebird))
      if(drop.nas){
        ebird <- ebird %>% filter(!is.na(c), !is.na(checklist_id))
      }

      cat("building ebird objects.....")
      lon    <- ebird$cell.lon.centroid
      lat    <- ebird$cell.lat.centroid
      y      <- ebird$c
      year   <- ebird$year
      site   <- ebird$checklist_id %>% as.factor()
      grid   <- ebird$gridcellid

      ## DEFINE OUTPUT OBJECTS FOR ebird
      objs.ebird <- c("lon",
                      "lat",
                      "year",
                      "y",
                      "site",
                      "grid"
      )
      ## Create a data frame for ebird data
      if(exists("objs.ebird")){
        ebird.out <- matrix(NA,
                            nrow=length(eval(parse(text=objs.ebird[1]))),
                            ncol=length(objs.ebird))
        for(j in seq_along(objs.ebird)){
          if(j==1){names<-NULL;keep=NULL}
          if(exists(objs.ebird[j])){
            keep <- c(keep, j)
            ebird.out[,j] <- get(objs.ebird[j])
            names <- c(names, objs.ebird[j])
          }
        }
        colnames(ebird.out) <- names
      }
      cat("done.\n")
    }
}# END DAT I LOOP

output <- list(as.data.frame(bbs.out) %>% arrange(year, site), as.data.frame(ebird.out) %>% arrange(year, site)) # mgcv wont take mats or arrays
names(output) <- c("bbs", "ebird")

# save to  file
fn=paste0(paste0(dir.out, "/", fn.out,".RDS"))
cat("Saving output to file: ", fn)
saveRDS(output, file=fn)
cat("..done.")

#export
return(output)


} # end fun
