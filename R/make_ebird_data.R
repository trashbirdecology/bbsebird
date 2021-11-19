#' @title Create eBird presence-absence data
#' @description
#' @param fns.ebird.in
#' @param dir.ebird.in
#' @export make_ebird_data
make_ebird_data <- function(fns.ebird.in, dir.ebird.in){

f_ebd <- fns.ebird.in[i]
f_smp <- f_samp_in
ebd <- auk_ebd(file = f_ebd,
               file_sampling = f_smp)
filters <-  ebd %>%
  auk_species(species = "Double-crested Cormorant", taxonomy_version = 2021) %>%
  auk_country(country = c("United States", "Canada")) %>%
  auk_complete() %>%
  auk_protocol(c("Stationary", "Traveling"))

ebd_sed_filtered <- filters
ebd_sed_filtered$output <- "data-local/ebird/ebd-filtered.txt"
ebd_sed_filtered$output_sampling <- "data-local/ebird/sampling-filtered.txt"


ebd_sed_filtered <- auk_filter(filters,
                               file = ebd_sed_filtered$output,
                               file_sampling = ebd_sed_filtered$output_sampling)

ebd_zf <- auk_zerofill(ebd_sed_filtered)
fn=paste0(dir.ebird.in, "ebd_zf_filtered_",i,".rds")
writeRDS(ebd_zf,

}
