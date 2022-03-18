## usethis namespace: start
#' @keywords internal
#' @name bbsebird
"_PACKAGE"
#' @importFrom auk auk_unique
#' @importFrom bbsAssistant munge_bbs_data
#' @importFrom data.table fread
#' @importFrom data.table fwrite
#' @importFrom dplyr across
#' @importFrom dplyr as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom hms as_hms
#' @importFrom lubridate as_date
#' @importFrom lubridate yday
#' @importFrom lubridate year
#' @importFrom mgcv jagam
#' @importFrom parallel detectCores
#' @importFrom parallel splitIndices
#' @importFrom readr col_character
#' @importFrom readr col_date
#' @importFrom readr col_double
#' @importFrom readr col_time
#' @importFrom reshape2 acast
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersection
#' @importFrom sf st_make_grid
#' @importFrom sf st_transform
#' @importFrom sp CRS
#' @importFrom sp proj4string
#' @importFrom sp spTransform
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom suncalc getSunlightTimes
#' @importFrom tidyr expand
#' @importFrom tidyr separate
#' @importFrom units drop_units
#' @importFrom vroom vroom
#' @importFrom foreach %dopar%
#' @importFrom stats complete.cases
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @importFrom utils browseURL
#' @importFrom utils memory.limit
#' @importFrom utils menu
#' @importFrom utils timestamp
#' @importFrom utils unzip
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## usethis namespace: end
NULL
