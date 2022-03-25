#' #' Munge and Output a Single or List of Data Frames for Use in mgcv::jagam
#' #' @param dat a single data object or a list of multiple objects to munge and collapse into a list of data for use in JAGS
#' #' @param dir.out Directory within which the output list will be saved  as "jdat"
#' #' @param drop.nas logical if FALSE will not remove sites/grid cells where no data exists.
#' #' @param fn.out Filename  of the object output as a .RDS file
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom dplyr distinct group_by summarise
#' #' @export make_gam_dat
#' make_gam_dat <-
#'   function(dat,
#'            drop.nas = FALSE,
#'            dir.out,
#'            fn.out = "jagamdat"
#'            ) {
#'     # In case a single data frame is supplied, add a NULL list element (because im lazy and don't want to rewrite the fucntion rn)
#'     if (!"list" %in% class(dat))
#'       dat <- list(dat)
#'     # Naming the list objects
#'     dat.names <- NULL
#'     for (i in 1:length(dat)) {
#'       if (any(class(dat[[i]]) %in% c("sf", "spdf")))
#'         dat[[i]] <- dat[[i]] |> sf::st_drop_geometry()
#'
#'       ind <-  colnames(dat[[i]])
#'       if ("checklist_id" %in% ind)
#'         dat.names[i] <- "ebird"
#'       if ("rteno" %in% ind)
#'         dat.names[i] <- "bbs"
#'
#'     }#end naming for loop
#'     names(dat) <- dat.names
#'
#'     # GAM on max observed - 1 value per grid cell id -----------------------------------------
#'       output <- list()
#'       for (i in seq_along(dat)) {
#'         colnames(dat[[i]]) <- tolower(colnames(dat[[i]]))
#'         suppressWarnings(
#'           output[[i]] <- dat[[i]] |>
#'             dplyr::distinct(gridcellid, cell.lat.centroid, cell.lon.centroid, c) |>
#'             dplyr::group_by(gridcellid) |>
#'             dplyr::summarise(
#'               Cmax = max(c, na.rm = TRUE),
#'               x = max(cell.lon.centroid),
#'               y = max(cell.lat.centroid) # doing max lat lon here b/c im lazy
#'             ) |>
#'             ungroup()
#'         )
#'         output[[i]]$Cmax[output[[i]]$Cmax %in% c("Inf", "-Inf")] <- 0
#'       }
#'       names(output) <- names(dat)
#'
#'       fn = paste0(paste0(dir.out, "/", fn.out, ".RDS"))
#'       cat("Saving output to file: ", fn)
#'       saveRDS(output, file = fn)
#'       cat("..done.")
#'
#'       return(output)
#'
#'
#'   } # end fun
