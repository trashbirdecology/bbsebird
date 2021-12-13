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

# if ebird spatial is already in memory run nothing here.

# Filenames ---------------------------------------------------------------
fns.ebird <- id_ebird_files(dir.ebird.in = dir.ebird.in)

# Filter the eBird Data ---------------------------------------------------
if(!exists("ebird_filtered")) ebird_filtered <- filter_ebird_data(
                                    fns.ebird = fns.ebird,
                                    overwrite = FALSE,
                                    dir.ebird.out = dir.ebird.out,
                                    countries = countries,
                                    states = states,
                                    protocol = c("Traveling","Stationary"),
                                    species = interest.species,
                                    years=years
                                    )


# Zero-fill the eBird Data -------------------------------------------------
fns <- list.files(dir.ebird.out, full.names = TRUE, pattern = "filtered.txt")

# would like to get this functional but auk_zerofill currently requires
## VERY specific coltypes and names. not flexible in coltypes...
# ebird_zf <- auk::auk_zerofill(x=fns[fns %>% str_detect("obs")],
#                               sampling_events = fns[fns %>% str_detect("samp")])

ebird_zf <- zerofill_ebird(myList=ebird_filtered, overwrite=FALSE, dir.out=dir.spatial.out)
gc()

# Create the eBird Spatial Layer  -----------------------------------------------------
ebird_spatial <- make_ebird_spatial(df=ebird_zf, crs.target = crs.target)

# Export Data -------------------------------------------------------------
saveRDS(ebird_spatial, file = paste0(dir.spatial.out, "ebird_spatial.rds"))

# Clear mem ---------------------------------------------------------------------
if(exists("args.save")){
args.save <- c(args.save, "ebird_spatial")
  rm(list=setdiff(ls(), args.save))
}


# END RUN -----------------------------------------------------------------


