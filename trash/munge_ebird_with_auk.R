# munge_ebird_with_auk <- function(f_ebd, f_samp,
#                                  interest.species=NULL,
#                                  interest.spatial=NULL,
#                                  complete.checklists.only=TRUE,
#                                  ebird.protocol=NULL,
#                                  out.file = NULL,
#                                  dir.ebird.out="data-local/ebird/",
#                                  overwrite=FALSE
#                                  ){
#
#   # define the output file if NULL
#   if(is.null(out.file)){out.file <- paste0(dir.ebird.out, "ebird_filtered_",Sys.Date(),".txt")}
#   if(length(f_ebd)>1){choice=menu(choices=c(f_ebd), title="Multiple observation files detected. Please choose which to import:")
#   if(choice==0|!exists("choice")){message("No choice specified. Defaulting to first in list."); choice=1}
#   f_ebd <- f_ebd[choice]}
# # having a hard time figuring out how to COMBINE the country data and auk reads it in correctly so...one country at a time i suppose
# #   # read in the observations (EBD-eBird basic database)
# #   if(length(f_ebd)>1 & !exists("ebird_observations")){
# #     fn.out = paste0(dir.ebird.out, "ebird_observations", Sys.Date() , ".txt")
# #       ebird_observations <- do.call(dplyr::bind_rows,lapply(f_ebd, vroom::vroom))
# #       vroom::vroom_write(
# #         x = ebird_observations,
# #         file=fn.out,
# #         delim="\t")
# #       f_ebd <- fn.out
# #   }
#
#   # only one sampling events .txt should be available.
#   if(length(f_samp)>1){
#     choice=menu(choices=c(f_samp), title="Multiple sampling events files detected. Please choose which to import:")
#     if(choice==0|!exists("choice")){message("No choice specified. Defaulting to first in list."); choice=1}
#     f_samp <- f_samp[choice]
#   }
#
#
#   ebd_filters <- auk_ebd(file = f_ebd, file_sampling = f_samp) %>%
#     auk_unique()
#   # Step 2. Define filters
#   # 2. define filters
#   if(!is.null(ebird.protocol)){
#     ebd_filters <- ebd_filters %>%
#       auk_protocol(protocol = ebird.protocol)
#   }
#   if(!is.null(interest.species)){
#     interest.species = interest.species[interest.species %in% unique(ebird_observations$`COMMON NAME`)]
#     ebd_filters <- ebd_filters %>%
#       auk_species(species=interest.species)
#   }
#   if(!is.null(interest.spatial)){
#     int.code <- interest.spatial[interest.spatial %in% unique(ebird_observations$`COUNTRY CODE`)]
#     int.ctry <-   interest.spatial[interest.spatial %in% unique(ebird_observations$COUNTRY)]
#     int.state <- interest.spatial[interest.spatial %in% unique(ebird_observations$STATE)]
#     int.state.code <- interest.spatial[interest.spatial %in% unique(ebird_observations$`STATE CODE`)]
#     if(!is.null(int.ctry)){ebd_filters <- ebd_filters %>% auk_country(country = int.ctry)}
#     if(!is.null(int.code)){ebd_filters <- ebd_filters %>% auk_country(country = int.code)}
#     if(!is.null(int.state)){ebd_filters <- ebd_filters %>% auk_country(country = int.state)}
#     if(!is.null(int.state.code)){ebd_filters <- ebd_filters %>% auk_country(country = int.state.code)}
#   }
#   if(complete.checklists.only==TRUE){
#   ebd_filters <- ebd_filters %>% auk_complete()
#   }
# # ebird_data
# print("Go grab a coffee, work out, or hug your dog. This is going to take....hours?")
# ebd_filtered <- auk_filter(ebd_filters, file=f_ebd, file_sampling = f_samp, overwrite=overwrite )
#
#
#
# }
