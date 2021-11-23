#' @title Manipulate the BBS route shapefiles
#' @description This function is meant to intake two shapefiles, one per cws and usgs munges them such that they conform to the USGS data release for BBS observations and route metadata.
#' @param cws.routes.dir Directory for where the CWS (Canadian BBS) shapefiles are stored.
#' @param cws.layer Name of the layer to import. Defaults to "ALL_ROUTES"
#' @param usgs.routes.dir Directory for where the USGS (USA BBS) shapefiles are stored.
#' @param usgs.layer Name of the layer to import.
#' @proj.target One of c("USGS", "CWS")
#' @export

munge_bbs_shapefiles <- function(cws.routes.dir,
                                 usgs.routes.dir,
                                 cws.layer="ALL_ROUTES", #name of cws layer in cws.routes.dir
                                 usgs.layer="bbsrte_2012_alb", # name of usgs layer in usgs.routes.dir
                                 proj.target="USGS" # defaults to the USGS projection.
){
  # Warning for proceeding when SLDFs already exist in the workspace
  if(exists("bbs_routes_sldf")|"SpatialLinesDataFrame" %in% sapply(ls(), function(x){class(get(x))})){
    ind=menu(title = "Objects of class SpatialLinesDataFrame already exist. This function may take a few minutes.Are you sure you want to proceed?",
             choices = c("yes", "no"))
    if(ind==2) stop("Function cancelled.")
  }

  # LOAD DATA
  ## CWS route shapefiles
  cws.gdb = list.files(cws.routes.dir, pattern=".gdb",full.names=TRUE) %>% str_remove(".zip") %>% unique()
  cws_routes <- readOGR(dsn=cws.gdb,layer=cws.layer)
  ## USGS route shapefiles (circa. 2012)
  ## USGS BBS routes layer obtained from John Sauer.
  ## Indexing by state-route combination
  ### Ex:  state 46 and route 029, RTENO==46029
  usgs_routes <- readOGR(dsn=usgs.routes.dir,layer=usgs.layer)

  # PROJECTIONS
  if(proj.target=="USGS") proj.target=proj4string(usgs_routes)
  if(proj.target=="CWS") proj.target=proj4string(cws_routes)
  usgs_routes <- spTransform(usgs_routes, proj.target)
  cws_routes <- spTransform(cws_routes, proj.target)


  # Housekeeping for USGS and CWS routes to match BBS dataset release
  usgs_routes@data$CountryNum=840
  cws_routes@data$CountryNum=124

  usgs_routes@data$StateNum= substr(usgs_routes@data$rteno, 1, 2)
  cws_routes@data$StateNum= substr(cws_routes@data$ProvRoute, 1, 2)

  usgs_routes@data$Route= substr(usgs_routes@data$rteno,3,5)
  cws_routes@data$Route= substr(cws_routes@data$ProvRoute, 3,5)

  usgs_routes@data$RouteName = usgs_routes@data$RTENAME
  cws_routes@data$RouteName = str_replace_all(cws_routes@data$Nbr_FullNa, "[:digit:]|-", "")#remove route no and prov no
  cws_routes@data$RouteName = trimws(cws_routes@data$RouteName, "left")#whitespace

  usgs_routes@data$RouteLength = usgs_routes@data$rte_length
  cws_routes@data$RouteLength = cws_routes@data$Shape_Leng

  # Join the two SpatialLinesDataFrames for CWS and USGS route spatial layers
  # first, keep only the variables that have been mapped back to the BBS observations dataset.
  colnames <- intersect(names(usgs_routes), names(cws_routes))
  usgs_routes.subset <- usgs_routes[, colnames]
  cws_routes.subset <- cws_routes[, colnames]


  # Checks
  if(suppressWarnings(!proj4string(cws_routes)==proj4string(usgs_routes)))warning("CRS for CWS and USGS route SLDFs do not match. ")
  if(!length(cws_routes.subset@lines)==length(cws_routes@lines)|
     !length(usgs_routes.subset@lines)==length(usgs_routes@lines)) warning("Some lines went missing when removing columns in USGS and/or CWS routes layers.")

  # join the two SLDFs
  bbs_routes_sldf <- rbind(usgs_routes.subset, cws_routes.subset)

  # create var RTENO for quick indexing.
  names(bbs_routes_sldf)
  bbs_routes_sldf@data<-make.rteno(bbs_routes_sldf@data)

  return(bbs_routes_sldf)

}



