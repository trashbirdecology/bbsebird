#' Create a Spatial Sampling Grid as Raster using WorldClim
#'
#' @export make_raster
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
    x <- raster::ccodes()
    ctry   <-  x[x$ISO3 %in% countries | x$ISO2 %in% countries, ]
    rm(x)

    ### CREATE POLOTC BOUNDARY OVERLAY FOR VIZ
    if (nrow(ctry) > 1) {
      overlay <- NULL
      for (i in seq_along(ctry$ISO3)) {
        temp <- (raster::getData('GADM', country = ctry$ISO3[i], level = 1))
        if (i == 1)
          overlay <- temp
        else
          overlay <- rbind(temp, overlay)
      }
    } else{
      overlay <-
        raster::getData('GADM', country = ctry$ISO3[1], level = 1)
    }

    if(!is.null(states)) overlay <- overlay[!overlay$NAME_1 %in% states,]


    if (restrict.uscan)
      overlay <- overlay[!overlay$NAME_1 %in% c("Alaska", "Hawaii"), ]
    box <- raster::extent(overlay) + buffer.dd

  ### CREATE RASTER SPATIAL GRID
  grid <- raster::getData(name="worldclim", var="bio", res=resolution)
  grid <- raster::crop(grid, box)
  grid$cellid <- 1:ncell(grid)
  # plot(grid[[2]])
  # plot(overlay, add=TRUE)

  return(grid)
} # END FUNCTION
