#
#
# ## eBird data---------------------------------------------------------
# ##coerce the ebird data to a data frame.
# ebird.df <- ebird %>% st_drop_geometry() %>%
#   as.data.frame() %>%
#   distinct(checklist_id, year, id,.keep_all=TRUE)
#
# ## Counts
# ebird.temp <- split(ebird.df %>% select(checklist_id, year, C, id), f = ebird.df$id )
# counts.ebird <-lapply(1:length(ebird.temp),
#                       function(x) (pivot_wider(ebird.temp[[x]],
#                                                id_cols=checklist_id,
#                                                names_from = year,
#                                                values_from = "C")))
# names(counts.ebird) <- names(ebird.temp)
# rm(ebird.temp)
#
