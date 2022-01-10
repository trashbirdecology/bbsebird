#' Munge and Output a List or List of Lists for Use in JAGS
#'
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param dat.names character string or vector of strings for names of input data objects. If not supplied, the function will guess whether the data is ebird, bbs, or grid.
#' @param scale.vars Logical If TRUE will scale variables. This needs to be improved/checked.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param fn.out Filename of the object output as a .RDS file
#' @export make_jags_list
make_jags_list <- function(dat, dat.names=NULL, drop.nas=TRUE, scale.vars=TRUE, dir.out, fn.out="jdat"){

  # Force object(s) in dat to a list, despite length
  if(!is.list(dat)) dat <- list(dat)
  if(!is.null(dat.names)) names(dat) <- tolower(dat.names)

  # Naming the list objects
  if(is.null(dat.names)){
    for(i in 1:length(dat)){
    ind <-  names(dat[[i]])
    if("checklist_id" %in% ind) dat.names[i] <- "ebird"
    if("rteno" %in% ind) dat.names[i] <- "bbs"

    if(!("rteno" %in% ind) & !("checklist_id" %in% ind)) dat.names[i] <- "grid"
    }#end naming for loop
    names(dat) <- dat.names
  }# end dat.names is null

# Initialize empty objects in case they aren't filled later we can still recall them without asking ls() or exists("")
objs.bbs <- objs.grid <- objs.ebird <- NULL


# Munge data/make jdat based on what data we are dealing with
  for(i in seq_along(dat)){
    ind <- names(dat)[i] # make lazy indicator for which data we are munging
    if(ind == "bbs"){
        bbs <- dat[[i]] %>%
          units::drop_units() %>%
          arrange(gridcellid, rteno, year)
      if("sf" %in% class(bbs)) bbs <- bbs %>% sf::st_drop_geometry()
        names(bbs) <- tolower(names(bbs))
        if(drop.nas){
          bbs <- bbs %>% filter(!is.na(c), !is.na(rteno))
        }

        ## Observed counts as 3D array (dims: rteno by year by gridcellid)
        # yBBS.grid   <- make_array(bbs, val="c")
        ## Observed counts as 2D matrix (dims: rteno by year)
        yBBS.site   <- make_mat(bbs %>% distinct(rteno, year, c), row = "rteno", col="year", val = "c")

        if(scale.vars){
          bbs <- bbs %>%
            ### create a variable for the wind "average"
            group_by(rteno, year) %>%
            mutate(windmean = abs(startwind-endwind)/2) %>%
            ungroup() %>%
            ### z-scale covariates
            mutate(
              windmean = (windmean - mean(windmean, na.rm=TRUE))/sd(windmean, na.rm=TRUE),
              noisemean = (noisemean - mean(noisemean, na.rm=TRUE))/sd(noisemean, na.rm=TRUE),
              carmean = (carmean - mean(carmean, na.rm=TRUE))/sd(carmean, na.rm=TRUE)
            )
        }#end scale.vars bbs

      # Linear model covariates (2D, since grid/route locations don't change over time.)
      grid.bbs <- bbs %>% distinct(gridcellid, rteno, .keep_all=TRUE)

      ## area of each grid cell
      if(scale.vars) area.bbs <- scale(grid.bbs$area)else{area.bbs <- grid.bbs$area}

      ## proportion of rteno in cell
      prop.site.in.cell.bbs <- make_mat(grid.bbs, val="proprouteincell", replace.na=TRUE)

      # Loop indexes for JAGS
      S.bbs <- length(unique(bbs$rteno))
      T.bbs <- length(unique(bbs$year))
      G.bbs <- length(unique(bbs$gridcellid))

### IDENTIFY DESIREd BBS OBJS AS CHARACTER STRING
      objs.bbs <- c("prop.site.in.cell.bbs", ## proportion of a route in a grid cell
                    "yBBS.site", ## observed counts for bbs
                    "S.bbs", "T.bbs", "G.bbs" ## loop length indexes
                    )
    }#end bbs loop

    if(ind == "grid"){
      grid <- dat[[i]] %>%
        units::drop_units() %>%
        arrange(gridcellid) %>%
        distinct(gridcellid, .keep_all=TRUE)

      if("sf" %in% class(grid)) grid <- grid %>% sf::st_drop_geometry()


      names(grid) <- tolower(names(grid))
      G <- length(unique(grid$gridcellid))
      XY <- cbind(grid$cell.lon.centroid, grid$cell.lat.centroid)
      area <- grid$area
      objs.grid <- c("G", "XY", "area")
      }#end grid loop


    if(ind == "ebird"){
      ebird <- dat[[i]] %>%
        arrange(gridcellid, checklist_id, year)
      if("sf" %in% class(ebird)) ebird <- ebird %>% sf::st_drop_geometry()


      names(ebird) <- tolower(names(ebird))

      if(drop.nas) ebird <- ebird %>% filter(!is.na(c), !is.na(checklist_id))

      # Observed counts as 3D array (dims: rteno by year by gridcellid)
      cat("working...\n\n")
      # yeBird.grid   <- make_array(ebird, row="checklist_id", val="c")
      yeBird.site   <- make_mat(ebird %>% distinct(checklist_id, year, c), row = "checklist_id", col="year", val = "c")

      G.ebird <- length(unique(ebird$gridcellid))
      S.ebird <- length(unique(ebird$checklist_id))
      T.ebird <- length(unique(ebird$year))

      # specify objects of interest
      objs.ebird <- c("yeBird.site",
                      "G.ebird", "S.ebird","T.ebird"
                      )
    }#end ebird loop

}#end i loop for munging `dat`


# Finally, grab the objects and smash into a single list or list of lists
objs <- c(objs.ebird, objs.bbs, objs.grid)
for(i in seq_along(objs)){

  if(i==1){new.list<-list(); keep=NULL}
    if(exists(objs[i])){
      keep <- c(keep, i)
      new.list[[i]] <- get(objs[i])
      # new.list[[i]] <- eval(parse(text=paste(objs[i])))
      names <- c(names, objs[i])
  }
}

# drop empty lists
output <- new.list[!sapply(new.list, is.null)]
names(output) <- objs[keep]

# save to  file
fn=paste0(paste0(dir.out, "/", fn.out,".RDS"))
cat("Saving output to file: ", fn)
saveRDS(output, file=fn)

# export from function
return(output)

} # END FUN
