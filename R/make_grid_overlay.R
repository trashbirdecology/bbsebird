#' @title Make Spatial Grid Overlay
#' @description Create a spatial grid overlay for assigning observation and covariate data to spatial point processes.
#' @param extent OBJ TYPE The extent of the spatial grid.
#' @param grid.type One of c("hexagonal", "square").
make_grid_overlay <- function(extent=NULL, grid.type="hexagonal", dir.grid.out=NULL){

  if(is.null(extent))
  if(is.null(dir.grid.out)) dir.grid.out <- "data-local/spatial/"

  if(tolower(grid.type)=="hexagonal"){}
  if(tolower(grid.type)=="square"){}
}
