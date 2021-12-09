if(exists("args.save")){rm(list=setdiff(ls(), args.save))}else(rm(list=ls()))
devtools::load_all()

# Create or Load in Post-spatial Munging BBS and eBird Data ----------------------------
fns <- c("bbs.rds", "ebird.rds", "grid.rds")

if(all(fns %in% list.files(dir.jags))){
  cat("importing the munged bbs, ebird and grid sf objects.")
  fns=list.files(dir.jags, full.names = TRUE)
  bbs <- readRDS(fns[str_detect(fns, "bbs_spat")])
  grid <- readRDS(fns[str_detect(fns, "grid")])
  ebird <- readRDS(fns[str_detect(fns, "ebird_spat")])
}else{
  cat("Building the BBS and eBird data and spatial objects. Sit back and relax......\nor be worried for errors halfway through")
  source("3_make-bird-data.R")
}


# Create JAGS data ------------------------------------------------
jags <- list()


## munge bbs for jags ------------------------------------------------------
names(bbs)



## munge ebird for jags -----------------------------------------------------
names(ebird)


