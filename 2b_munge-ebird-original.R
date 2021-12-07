if(exists("args.save")) {
  rm(list = setdiff(ls(), args.save))
} else
  (rm(list = ls()))
source("0_setup.R")
devtools::load_all()


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
# Grab a list of the data files
fns.ebird <- id_ebird_files(dir.ebird.in = dir.ebird.in, regions = states)



ebird_filtered <- filter_ebird_data(fns.ebird = fns.ebird,
                                    overwrite = FALSE,
                                    dir.ebird.out = dir.ebird.out)


# ENDRUN ------------------------------------------------------------------

fns <- tolower(list.files(dir.ebird.in, full.names=TRUE))

fns.samp <- fns[str_detect(fns,"samp_out")]
fns.ebd  <- setdiff(fns,fns.samp)

# ## Read files into R
# ebd  <- vroom::vroom(fns.ebd)
# samp <- vroom::vroom(fns.samp)
#
#
# ## Identify potential BBS counts for removal.
# potential.bbs <- ebd %>% filter(`DURATION MINUTES`==3 & `PROTOCOL TYPE`=="Stationary") %>%
#   distinct(`OBSERVATION DATE`,`LOCALITY ID`,  `OBSERVER ID`, `SCIENTIFIC NAME`, .keep_all=TRUE)
#
# ## Reduce the sampling events data
# samp.filtered <- samp %>%
#   filter(country %in% unique(ebd$COUNTRY)) %>%
#   filter(STATE %in% ebd$STATE)
#
# ## Create zero-filled data
# ebird_zf <- full_join(ebd, samp.filtered)
