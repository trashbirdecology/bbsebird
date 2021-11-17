
# BBS route shapefiles -----------------------------------------------------
## this function is meant to intake two shapefiles, one per cws and usgs and,
## munge them such taht they conform to the USGS data release for BBS observations and route metadata.
munge.bbs.route.shps <- function(cws.routes.dir,
                                 usgs.routes.dir,
                                 cws.layer="ALL_ROUTES", #name of cws layer in cws.routes.dir
                                 usgs.layer="bbsrte_2012_alb", # name of usgs layer in usgs.routes.dir
                                 proj.target="USGS" # defaults to the USGS projection.
                                 ){
# Warning for overwriting existing bbs_routes_sldf.
  if(exists("bbs_routes_sldf")){
    ind=menu(title = "Object `bbs_routes_sldf` already exists. Overwriting will take a few minutes. Do you want to proceed?",
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

  return(bbs_routes_sldf)

        }



# Filter BBS by species ---------------------------------------------------
## ID LOVE to be able to throw all the BBS fitlering into a single function within which I use
## across(fun_x(list elemnt )) and use lapply to apply the function to the entire list. but for now i am just gonna do it ,"by hand"
  # if("citation" %in% names(list)) bbs$citation<-bbs$citation %>% as.data.frame() # this is required for quickly scanning and filtering lists, sorry

filter.bbs.by.species <- function(list, search = interest.species){#, unid=FALSE) {
  # grab the unique AOU codes
  list$species_list <- list$species_list %>%
    mutate(across(starts_with("English_Common_Name"), tolower)) %>%
    filter(across(any_of("English_Common_Name"), ~ .x %in% tolower(search)))
  # use the aou to filter down the observations
  list$observations <- list$observations %>%
    filter(as.double(AOU) %in% as.double(unique(list$species_list$AOU))) # just hneed to ensure the variables are of same type

  print(cat("The following species are in your BBS data: ", paste(unique(list$species_list$English_Common_Name))))
  return(list) # return the entire list now as a subset of the original list
  }

# Filter BBS list by AOU by species ---------------------------------------------------
# filter.bbs.by.aou <- function(df) {
#   # create an indicator for unique aous present in species_list
#   aou.ind <- df %>%
#     filter(across(any_of("English_Common_Name"), ~ .x %in% tolower(search)))
#   df.out <- df %>%
#     mutate(across(starts_with("English_Common_Name"), tolower)) %>%
#     filter(across(any_of("English_Common_Name"), ~ .x %in% tolower(search))) # this should subset the species_list df to only those species of interest.
#   return(df.out)
# }

# MODE --------------------------------------------------------------------
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
