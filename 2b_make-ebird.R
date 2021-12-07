if(exists("args.save")) {
  rm(list = setdiff(ls(), args.save))
} else
  (rm(list = ls()))
devtools::load_all()
source("1_spatial-grid.R")


# Warning -----------------------------------------------------------------
gc(full = TRUE)
if (detectCores() <= 4 |
    memory.limit() < 25000)
  warning(
    "You don't have enough RAM and/or CPU to munge the eBird data. Don't blame me if your machine crashes."
  )
message(
  "Tossing out the garbage (`gc`) and about to deal with this eBird data. Buckle up, buttercup."
)



# Filter the eBird Data ---------------------------------------------------
fns.ebird <- id_ebird_files(dir.ebird.in = dir.ebird.in)

ebird_filtered <- filter_ebird_data(fns.ebird = fns.ebird,
                                    overwrite = FALSE,
                                    dir.ebird.out = dir.ebird.out,
                                    countries = countries,
                                    states = states, protocol = c("Traveling","Stationary"),
                                    species = interest.species
                                    )


# Zero-fill the eBird Data -------------------------------------------------
ebird_zf <- zerofill_ebird(ebird_filtered, save=TRUE)


# Filter eBird ------------------------------------------------------------
## keep only modern data...(ebird dataset has dates back to year 1880). should help
ebird_zf <- ebird_zf %>%
    filter(year >= 1966)


gc()

## eBird to Spatial Layers -----------------------------------------------------
###NEED TO PUSH THIS TO MUNGE_EBIRD_SPATIAL or something like that
# convert ebd to spatial object
coordinates(ebird_zf) <-
  ~ longitude + latitude # 1-2 minutes for all of N.Amer.
# define projection for lat long (ebird documentation states CRS is 4326)
proj4string(ebird_zf) <-
  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# transform spatial object to sf
ebird_zf <- sf::st_as_sf(ebird_zf)
# match proj to target proj
ebird_zf <-
  st_transform(ebird_zf, crs = CRS(paste0("+init=epsg:", crs.target)))

## Combine grid and eBird -----------------------------
# this will take a minute
tic()
ebird_spatial <- grid %>% #  37sec for Ohio//2min for FL,GA,SC//2 min for Great Lakes
  st_join(ebird_zf)
toc()


# Export Data -------------------------------------------------------------
saveRDS(ebird_spatial, file = paste0(dir.proj.out, "/", "ebird_spatial.rds"))


