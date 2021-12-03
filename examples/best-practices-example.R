library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
select <- dplyr::select

auk_set_ebd_path("data/", overwrite=TRUE)

list.files("data", full.names=TRUE)

# ebd <- auk_ebd("ebd_US-FL_doccor_relSep-2021.txt", 
#                file_sampling = "ebd_sampling_relSep-2021.txt")

file="data/ebd_US-FL_doccor_relSep-2021.txt"
file_sampling="data/ebd_sampling_relSep-2021.txt"


# ebd <- auk_ebd("data/ebd_US-FL_doccor_relSep-2021.txt", 
#                file_sampling = "data/ebd_sampling_relSep-2021.txt")
