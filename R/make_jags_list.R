#' Munge and Output a List or List of Lists for Use in JAGS
#'
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param dat.names character string or vector of strings for names of input data objects. If not supplied, the function will guess whether the data is ebird, bbs, or grid.
#' @param scale.vars Logical If TRUE will scale variables. This needs to be improved/checked.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param fn.out Filename of the object output as a .RDS file
#' @export make_jags_list
make_jags_list <- function(dat, dat.names=NULL, drop.nas=FALSE, scale.vars=TRUE, dir.out, fn.out="jdat"){

  # Force object(s) in dat to a list, despite length
  if(!is.list(dat))       dat <- list(dat)
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
        cat("building bbs objects..\n")


        ## Observed counts as 3D array (dims: rteno by year by gridcellid)
        yBBS.grid   <- make_array(bbs, val="c")
        ## Observed counts as 2D matrix (dims: rteno by year)
        yBBS.site   <- make_mat(bbs %>% distinct(rteno, year, c) %>% filter(!is.na(rteno)), row = "rteno", col="year", val = "c")
        yBBS.site   <- yBBS.site %>% select(sort(names(yBBS.site))) # use select to ensure the colnames(years) are in order...

        # make mean values for detection covariates (some were already done elsewhere -- need to add these to that location at some point)
        bbs <- bbs %>%
          group_by(rteno, year) %>%
          mutate(windmean=abs(startwind-endwind)/2) %>%
          mutate(skymean=abs(startsky-endsky)/2)


        p.wind      <- make_mat(bbs %>% distinct(rteno, year, windmean),
                                row = "rteno", col="year",
                                val="windmean")
        p.car       <- make_mat(bbs %>% distinct(rteno, year, carmean),
                                row = "rteno", col="year",
                                val="carmean")
        p.noise     <- make_mat(bbs %>% distinct(rteno, year, noisemean),
                                row = "rteno", col="year",
                                val="noisemean")
        p.fyrbbs    <- make_mat(bbs %>% distinct(rteno, year, obsfirstyearbbs),
                                row = "rteno", col="year",
                                val="obsfirstyearbbs")
        p.fyrroute  <- make_mat(bbs %>% distinct(rteno, year, obsfirstyearroute),
                                  row = "rteno", col="year",
                                  val="obsfirstyearroute")
        p.assistant <- make_mat(bbs %>% distinct(rteno, year, assistant),
                                row = "rteno", col="year",
                                val="assistant")
        p.assistant <- p.assistant[p.assistant=="NULL"] <- NA



        if(scale.vars){
          bbs <- bbs %>%
            ### create a variable for the wind "average"
            group_by(rteno, year) %>%
            mutate(windmean = abs(startwind-endwind)/2) %>%
            ungroup() %>%
            ### z-scale covariates
            mutate(
              windmean  = (windmean - mean(windmean, na.rm=TRUE))/sd(windmean, na.rm=TRUE),
              noisemean = (noisemean - mean(noisemean, na.rm=TRUE))/sd(noisemean, na.rm=TRUE),
              # skymean   = (skymean - mean(skymean, na.rm=TRUE))/sd(skymean, na.rm=TRUE),
              carmean   = (carmean - mean(carmean, na.rm=TRUE))/sd(carmean, na.rm=TRUE)
            )
        }#end scale.vars bbs

      # Linear model covariates (2D, since grid/route locations don't change over time.)
      grid.bbs <- bbs %>% distinct(gridcellid, rteno, .keep_all=TRUE)

      ## area of each grid cell
      if(scale.vars) area.bbs <- scale(grid.bbs$area)else{area.bbs <- grid.bbs$area}

      ## proportion of rteno in cell
      # browser()
      prop.site.in.cell.bbs <- make_mat(grid.bbs, val="proprouteincell", replace.na=TRUE)
      prop.site.in.cell.bbs[is.na(prop.site.in.cell.bbs)] <- 0 # these must be zero for jags model


      # Loop indexes for JAGS
      rtes <- bbs %>% filter(!is.na(rteno)) # to ensure we remove the NA rteno
      S.bbs <- length(unique(rtes$rteno))
      T.bbs <- length(unique(bbs$year))
      G.bbs <- length(unique(bbs$gridcellid))

### IDENTIFY DESIRED BBS OBJS AS CHARACTER STRING
      objs.bbs <- c("prop.site.in.cell.bbs", ## proportion of a route in a grid cell
                    "yBBS.site", ## observed counts for bbs
                    "yBBS.grid", ## counts at route within grid (array)
                    # "S.bbs", "T.bbs", "G.bbs", ## loop length indexes,
                    "p.wind", "p.car", "p.noise", "p.fyrbbs", "p.fyrroute"  ## detection covariates
                    )
    }#end bbs loop

    if(ind == "grid"){
      grid <- dat[[i]] %>%
        units::drop_units() %>%
        arrange(gridcellid) %>%
        distinct(gridcellid, .keep_all=TRUE)

      if("sf" %in% class(grid)) grid <- grid %>% sf::st_drop_geometry()
      cat("building grid objects..\n")


      names(grid) <- tolower(names(grid))
      G <- length(unique(grid$gridcellid))
      XY <- cbind(grid$cell.lon.centroid, grid$cell.lat.centroid)
      XY.scaled <- cbind(scale(XY[,1]), scale(XY[,2])) # not
      area <- grid$area
      objs.grid <- c("G", "XY", "XY.scaled", "area")
      }#end grid loop


    if(ind == "ebird"){
      ebird <- dat[[i]]
      if("sf" %in% class(ebird)) ebird <- ebird %>% sf::st_drop_geometry()

      names(ebird) <- tolower(names(ebird))

      if(drop.nas) ebird <- ebird %>% filter(!is.na(c), !is.na(checklist_id)) %>%
        arrange(gridcellid, checklist_id, year)

      # Observed counts as 3D array (dims: rteno by year by gridcellid)
      cat("building eBird objects..\n")
      yeBird.grid   <- make_array(ebird %>% filter(!is.na(year)), row="checklist_id", val="c")
      yeBird.site <- make_mat(ebird %>% distinct(checklist_id, year, c) %>% filter(!is.na(checklist_id)), row = "checklist_id", col="year", val = "c")
      yeBird.site <- yeBird.site %>% select(sort(names(yeBird.site))) # use select to ensure the colnames(years) are in order...


      temp.ebird <- ebird %>% filter(!is.na(checklist_id),
                                     !is.na(year)
                                     )

      G.ebird <- length(unique(ebird$gridcellid))
      S.ebird <- length(unique(temp.ebird$checklist_id))
      T.ebird <- length(unique(temp.ebird$year))


      # # get the maximum value (count) for each column (year) across all slices
      # ## used in GAM --
      # Ni_ebird <- NULL
      # for(j in 1:dim(yeBird.grid)[3]){
      #   x=yeBird.grid[,,j]
      #   ### need to add a silencer to avoid rbind telling me about NAs
      #   suppressWarnings(Ni_ebird <- rbind(Ni_ebird, as.integer(sub(".*:", "", summary(x)[6,]))))
      # }
      # Ni_ebird <- Ni_ebird %>% replace(is.na(.), 0) # replace NA with zeroes
      #




      # specify objects of interest
      objs.ebird <- c("yeBird.site", "yeBird.grid",
                      "G.ebird", "S.ebird","T.ebird"
                      # "Ni_ebird"
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
