# unloadNamespace("dubcorms")
devtools::install_github("trashbirdecology/dubcorms", force=FALSE)
devtools::install_github("cboettig/birddb", force=FALSE)
pkgs <- c("dubcorms","bbsAssistant","ggplot2","reshape2",
          "stringr","dplyr","sf", "birddb")
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs)

#Directories
dir.orig.data <- "C:/Users/jburnett/OneDrive - DOI/research/cormorants/dubcorm-data-backup/"
dir.proj <- "C:/Users/jburnett/documents/github/dubcorms/inst/tutorials/DCCO/"
dir.ebird.in <- paste0(dir.orig.data, "ebird")

# "Import" eBird Data using `birddb`
## find relevant files 
# fns <- id_ebird_files(dir.ebird.in, mmyyyy = "nov-2021")
fns <- list.files(dir.ebird.in, all.files = TRUE, full.names=TRUE)
fns.nov <- grep("(sep-2021|tar)(?:.+)(sep|oct-2021)", fns, value = TRUE, ignore.case = TRUE)


checklists_tar   <- fns[str_detect(fns, "ebd_sampling")]
observations_tar <- grep("(hospa)(?:.+)", fns.nov, value = TRUE, ignore.case = TRUE)
  

import_ebird(observations_tar)
import_ebird(checklists_tar)

# Access the database
observations <- observations()
checklists <- checklists()


# Import to R
df <- observations %>% 
  # count(observation_date, sort = TRUE) %>% 
  as_tibble()

df <- observations %>% 
  # count(observation_date, sort = TRUE) %>% 
  as.data.frame()