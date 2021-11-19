#' #' @title Create eBird presence-absence data
#' #' @description
#' #' @param fns.ebird.in
#' #' @param dir.ebird.in
#' #' @export make_ebird_data
#' make_ebird_data <- function(fns.ebird.in, dir.ebird.in){
#'
#'   for(i in seq_along(fns.ebird.in)){
#'     print(i)
#'     #skip the sampling events file...
#'     if(i==1) {f_samp_in <- list.files(dir.ebird.in, "ebd_sampling", full.names=TRUE)
#'     f_samp_in <- f_samp_in[!str_detect(f_samp_in, ".tar|.gz|.zip")]
#'     }
#'     if(str_detect(fns.ebird.in[i], 'sampling_events'))next()
#'
#'
#' f_ebd <- fns.ebird.in[i]
#' f_smp <- f_samp_in
#' ebd <- auk_ebd(file = f_ebd,
#'                file_sampling = f_smp)
#' filters <-  ebd %>%
#'   auk_species(species = "Double-crested Cormorant", taxonomy_version = 2021) %>%
#'   auk_country(country = c("United States", "Canada")) %>%
#'   auk_complete() %>%
#'   auk_protocol(c("Stationary", "Traveling"))
#'
#' ebd_filtered <- filters
#' ebd_filtered$output <- "data-local/ebird/ebd-filtered.txt"
#' ebd_filtered$output_sampling <- "data-local/ebird/sampling-filtered.txt"
#'
#'
#' ebd_filtered <- auk_filter(filters,
#'                                file = ebd_filtered$output,
#'                                file_sampling = ebd_filtered$output_sampling,
#'                            overwrite=TRUE)
#'
#' ebd_zf <- auk_zerofill(ebd_filtered) ## TAKES SOOO LONG....
#' fn=paste0(dir.ebird.in, "ebd_zf_filtered_",i,".rds")
#' writeRDS(ebd_zf,filename=fn)
#'   }
#'
#'
#' # head(ebd_zf$observations)
#' # glimpse(ebd_zf$sampling_events)
#' ## if necessary, craete a singel df (but less comp efficient)
#' # ebd_zf_df <- auk_zerofill(ebd_filtered, collapse = TRUE)
#' # ebd_zf_df <- collapse_zerofill(ebd_zf)
#' # class(ebd_zf_df)
#' # ebd_zf_df
#'
#' }
