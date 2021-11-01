
# FOR MUNGING EBIRD DATA --------------------------------------------------
## convert time observation to HOURS SINCE MIDNIGHT
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}
