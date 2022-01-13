#' Munge and Output a Single or List of Data Frames for Use in mgcv::jagam
#' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' @param dat.names character string or vector of strings for names of input data objects. If not supplied, the function will guess whether the data is ebird, bbs, or grid.
#' @param dir.out Directory within which the output list will be saved  as "jdat"
#' @param fn.out Filename of the object output as a .RDS file
#' @export make_gam_dat
make_gam_dat <- function(dat, dat.names=NULL, drop.nas=FALSE, dir.out, fn.out="jagamdat"){
  
  # Force object(s) in dat to a list, despite length
  if(!is.list(dat))       dat <- list(dat)
  if(!is.null(dat.names)) names(dat) <- tolower(dat.names)
  
  # Naming the list objects
  if(is.null(dat.names)){
    for(i in 1:length(dat)){
      ind <-  names(dat[[i]])
      if("checklist_id" %in% ind) dat.names[i] <- "ebird"
      if("rteno" %in% ind) dat.names[i] <- "bbs"
      
      if(!("rteno" %in% ind) & !("checklist_id" %in% ind)) stop("Input data must comprise bbs and/or ebird data. If supplying a list of data objects, please sure it is defined as a list (e.g., `dat=list(df_1, df_2)`)) ensure argument `dat` ")
    }#end naming for loop
    names(dat) <- dat.names
  }# end dat.names is null
  
  # Initialize empty objects in case they aren't filled later we can still recall them without asking ls() or exists("")
  objs.bbs <- objs.grid <- objs.ebird <- NULL
  
  
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
      lon.bbs <- bbs$cell.lon.centroid
      lat.bbs <- bbs$cell.lat.centroid
      y.bbs   <- bbs$c
      t.bbs   <- bbs$year  
      
      ## DEFINE OUTPUT OBJECTS FOR BBS   
      objs.bbs <- c("lon.bbs",
                    "lat.bbs", 
                    "t.bbs",
                    "y.bbs")  
      
    }#end bbs ifelse
    
    if(ind == "ebird"){
      ebird <- dat[[i]]
      if("sf" %in% class(ebird)) ebird <- ebird %>% sf::st_drop_geometry()
      
      names(ebird) <- tolower(names(ebird))
      
      if(drop.nas) ebird <- ebird %>% filter(!is.na(c), !is.na(checklist_id)) %>%
        arrange(gridcellid, checklist_id, year)
      
      # Observed counts as 3D array (dims: rteno by year by gridcellid)
      cat("building eBird objects..\n")
      lon.ebird <- ebird$cell.lon.centroid
      lat.ebird <- ebird$cell.lat.centroid
      y.ebird   <- ebird$c
      t.ebird   <- ebird$year
      
      ## DEFINE OUTPUT OBJECTS FOR BBS   
      objs.ebird <- c("lon.ebird",
                      "lat.ebird", 
                      "t.ebird",
                      "y.ebird")  
      
      
    }#end ebird ifelse
  }# END DAT I LOOP
  
  
  # Finally, grab the objects and smash into a single list or list of lists
  
  ## Create a data frame for BBS data
  if(exists("objs.ebird")){
    ebird <- matrix(NA, 
                    nrow=length(eval(parse(text=objs.ebird[1]))),
                    ncol=length(objs.ebird))
    for(i in seq_along(objs.ebird)){
      if(i==1){names<-NULL;keep=NULL}
      if(exists(objs.ebird[i])){
        keep <- c(keep, i)
        ebird[,i] <- get(objs.ebird[i])
        names <- c(names, objs.ebird[i])
      }
    }
    colnames(ebird) <- names
  }else(ebird<-NULL)
  
  ## Create a data frame for BBS data
  if(exists("objs.bbs")){
    bbs <- matrix(NA, 
                  nrow=length(eval(parse(text=objs.bbs[1]))),
                  ncol=length(objs.bbs))
    for(i in seq_along(objs.bbs)){
      if(i==1){names<-NULL;keep=NULL}
      if(exists(objs.bbs[i])){
        keep <- c(keep, i)
        bbs[,i] <- get(objs.bbs[i])
        names <- c(names, objs.bbs[i])
      }
    }
    colnames(bbs) <- names
  }else(bbs<-NULL)
  
  output <- list(as.data.frame(bbs), as.data.frame(ebird)) # mgcv wont take mats or arrays
  names(output) <- c("bbs", "ebird")
  
  # save to  file
  fn=paste0(paste0(dir.out, "/", fn.out,".RDS"))
  cat("Saving output to file: ", fn)
  saveRDS(output, file=fn)
  
  # export from function
  return(output)
  
  
}