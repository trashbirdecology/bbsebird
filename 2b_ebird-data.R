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
  "Tossing out the garbage (`gc`) and about to deal with this eBird data.
  Buckle up, buttercup."
)


# Filenames ---------------------------------------------------------------
fns.ebird <- id_ebird_files(dir.ebird.in = dir.ebird.in)

if(!exists("samp")) samp <- data.table::fread(fns.ebird[3])

# Write Sampling File as Parquet ------------------------------------------
# ## code borrowed from: https://rpubs.com/FelipeMonroy/591813
# dir.parquet <-
#   paste0(dir.ebird.out, "/parquet")
# dir.create(dir.parquet, showWarnings = FALSE)
# #It checks if there are files in the output directory [output_dir]. If there are not, the code continues.
# if (length(list.files(dir.ebird.out, full.names = TRUE)) == 0) {
#   #It creates an object with all the .csv files names in the data/ folder
#   txts <- fs::dir_ls(dir.ebird.out, glob = "*.txt")
#
#   #If there are no .csv files in the data/ folder the code stops
#   if (length(txts) == 0)
#     stop("No txt files found in data/")
#   #For every .csv file, it executes the write_chunk_data
#   walk(csvs, write_chunk_data, dir.parquet)
# }


# Filter the eBird Data ---------------------------------------------------

ebird_filtered <- filter_ebird_data(fns.ebird = fns.ebird,
                                    overwrite = FALSE,
                                    dir.ebird.out = dir.ebird.out,
                                    countries = countries,
                                    states = states,
                                    protocol = c("Traveling","Stationary"),
                                    species = interest.species,
                                    years=c(1966:year(Sys.Date())),
                                    method="data.table"
                                    )


# Zero-fill the eBird Data -------------------------------------------------
ebird_zf <- zerofill_ebird(ebird_filtered, save=TRUE)


# Filter eBird ------------------------------------------------------------
## keep only modern data...(ebird dataset has dates back to year 1880). should help
ebird_zf <- ebird_zf %>%
    filter(year >= 1966)

# Make sure we dont have multiple checklists for same group birding event
t=ebird_zf %>%
  filter(!is.na(`group identifier`))
x=t %>% summarise(n_distinct(`observation date`, `group identifier`))

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
saveRDS(ebird_spatial, file = paste0(dir.spatial.out, "ebird_spatial.rds"))

