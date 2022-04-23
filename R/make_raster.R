make_raster <-
  function(baselayer = "worldclim",
           dir.out = NULL,
           resolution = 10, 
           states = NULL,
           restrict.uscan = TRUE, 
           buffer.dd = 5, 
           countries = c("US", "CA")){
    
    
    ctry<-NULL
      countries <- toupper(countries)
      states <- toupper(states)
      
    # filter out countries  
    x <- ccodes()
    ctry   <-  x[x$ISO3 %in% countries | x$ISO2 %in% countries, ]
    
  ### CREATE POLOTC BOUNDARY OVERLAY FOR VIZ 
  if(nrow(ctry)>1){ overlay <-
    do.call("bind", lapply(ctry$ISO3, function(x)
      raster::getData(
        'GADM', country = x, level = 1
      ))) } else{
        overlay <- raster::getData(
          'GADM', country = ctry$ISO3[1], level = 1
        )
      }
  if(!is.null(states)) overlay <- overlay[!overlay$NAME_1 %in% states,]    
  if(restrict.uscan) overlay <- overlay[!overlay$NAME_1 %in% c("Alaska", "Hawaii"),]
  box <- raster::extent(overlay)+buffer.dd

  ### CREATE RASTER SPATIAL GRID
  grid <- raster::getData(name="worldclim", var="bio", res=resolution)
  grid <- raster::crop(grid, box)
  grid$cellid <- 1:ncell(grid)
  # plot(grid[[2]])
  # plot(overlay, add=TRUE)
  
  return(ctry_shps)
} # END FUNCTION
