handle_bbs <-
  function(dirs,
           overwrite.bbs = FALSE,
           states,
           countries = NULL,
           species,
           species.abbr = NULL,
           year.range) {

# DATA IN --------------------------------------------------------------------
# original BBS data
fns <- tolower(list.files(dirs$dir.bbs.out))
if("bbs_obs.rds" %in% fns & !overwrite.bbs){
  cat("importing munged BBS data from  ",  dirs$dir.bbs.out, "\n")
  bbs_obs <- readRDS(paste0(dirs$dir.bbs.out,"bbs_obs.rds"))
  }else{
if("bbs_orig.rds" %in% fns & !overwrite.bbs){
  cat("importing BBS data from ", dirs$dir.bbs.out,"\n")
  bbs_orig <- readRDS(paste0(dirs$dir.bbs.out,"bbs_orig.rds"))}else{
  # if bbs_orig DNE, create and import original BBS data using bbsAssistant
  cat("downloading BBS data from ScienceBase\n")
  bbs_orig <- grab_bbs_data(bbs_dir = dirs$dir.bbs.out)
  saveRDS(bbs_orig,
          paste0(dirs$dir.bbs.out,"bbs_orig.rds"))
} # end bbs_orig

# munge bbs orig
  cat("munging bbs observations\n")
  # check to ensure species requested are present in BBS data
  sp.ind <- tolower(species)
  suppressMessages(b.spp <- bbsAssistant::species_list)
  b.spp$sp.ind.common <- tolower(b.spp$English_Common_Name)
  b.spp$sp.ind.sci    <- tolower(b.spp$Scientific_Name)

  sp.ind.keep <- sp.ind[sp.ind %in% c(b.spp$sp.ind.common, b.spp$sp.ind.sci)]
  stopifnot(length(sp.ind.keep)>=1)
  ## if not all species were found, give user a choice to proceed or quit BBS data munging task.
  if(length(sp.ind)!=length(species)){
    stopifnot(menu(title=cat("of the species you requested (" ,
                             species,
                             ") the following identifiers were found in the BBS dataset:\n", sp.ind, "do you wish to proceed with BBS data munging?"),
         choices = c("Yes, that's okay. Proceed.",
                     "No, stop. I need to fix argument `species`"
                     ))==1)
  }else{cat("all species requested were found in the BBS datasets. Proceeding normally.")}

  bbs_obs <-
      munge_bbs(
        list = bbs_orig,
        species = sp.ind.keep,
        states = states,
        year.range = year.range
      )

  }

saveRDS(bbs_obs, paste0(dir.bbs.out, "bbs_obs.rds"))




}#END FUN
