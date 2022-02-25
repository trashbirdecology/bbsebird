#' #' @title Large Wrapper Function for Munging eBird, BBS, and Spatial Grid Data
#' #' @description Provides a wrapper around the following functions:
#' #' 'make_spatial_grid', 'grab_bbs_data', 'munge_bbs_data', 'match_col_names', 'make_bbs_spatial',
#' #' 'id_ebird_files', 'munge_ebird_data', 'make_ebird_spatial', and 'bundle_data'. Given the number of arguments
#' #'  within the contained functions, this function is not as flexible as its components. If you need additional
#' #'  functionality, please call contained functions individually.
#' #' @param
#' #' @param
#' #' @param
#' #' @param
#' #' @param
#' #'
#' #' @export
#' lazy_make_jags_data <- function(
#'   dir.ebird.in,
#'   mmyyyy,
#'   dir.files.out       = NULL,
#'   dir.shapefiles.in   = NULL,
#'   fn.cws.layer,
#'   fn.usgs.layer,
#'   crs.target          = 4326,
#'   grid.size           = 1.00, # size in decimal degrees (for US/CAN a good est is 1.00dec deg == 111.11km)
#'   species             = c("House Sparrow"),
#'   species.abbr        = c("houspa"),
#'   states              = c("us-fl"),
#'   countries           = c("US"),
#'   years               = 2008:2019,
#'   base.julian.date    = lubridate::ymd(paste0(min(years), c("-01-01"))),
#'   min.yday            = 91,
#'   max.yday            = 245,
#'   complete.checklists.only=TRUE,
#'   jagam.args          = list(bs="ds",k=20, family="poisson", sp.prior="log.uniform", diagonalize=TRUE)
#' ){
#' # HANDLE ADDITIONAL ARGUMENTS ---------------------------------------------
#' pkg.funs  <- as.vector(lsf.str("package:bbsebird"))
#' for(i in seq_along(pkg.funs)){
#' if(i==1) pkg.args <- NULL
#' pkg.args <- c(pkg.args, names(as.list(args(pkg.funs[i]))))
#' }
#' pkg.args <- pkg.args[pkg.args!="..."]
#' pkg.args <- pkg.args[pkg.args!=""]
#' addl.args
#' any(addl.args %in% pkg.args)
#'
#'
#' # If user specifies additional arguments,then we need to ensure there aren't duplicates.
#' if(!is.null(addl.args)){
#'   ## if only a single additional arg provided, just assign it to the environment
#'   if(length(addl.args)==1) assign(names(addl.args), unname(paste(addl.args[1])))
#'   if(length(addl.args) >1 & !is.list(addl.args)){stop("'addl.args' must be a list.")}
#'
#'
#'
#'   ## first unpack the original arguments
#'   args=unlist(as.list(args(lazy_make_jags_data)))
#'   list2env(addl.args)
#'   ## next, unpack the
#'
#' } # end addl.args is not null
#'
#'
#'
#'
#' # ARGUMENT TESTS ----------------------------------------------------------
#' ## spatial indicators (states, countries)
#' if(is.null(states)){ states.ind <- NULL}else{states.ind<-gsub(x=toupper(states), pattern="-", replacement="")}
#'
#' # MAKE SPATIAL GRID -------------------------------------------------------
#' grid <- make_spatial_grid(dir.out = dirs[['dir.spatial.out']],
#'                             # overwrite=overwrite.grid,
#'                             states = states.ind,
#'                             countries = countries,
#'                             crs.target = crs.target,
#'                             grid.size = grid.size
#'   )
#'
#'
#' # BBS DATA -------------------------------------------------------------------
#' ## First, check specified directories for files
#' ### BBS routes shapefiles
#' if(!is.null(dir.shapefiles.in)){
#'   shps <- tolower(list.files(dir.shapefiles.in, recursive = TRUE, pattern=".shp"))
#'   gdbs <- tolower(list.files(dir.shapefiles.in, recursive = TRUE, pattern=".gdb"))
#'
#'
#'
#' }
#'
#'
#' ### ebird data in
#' ##
#' ##
#' ##
#'
#'
#' #  SPECIFY EMPTY / NULL ARGS --------------------------------------------------------------------
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' }
