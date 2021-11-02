
# FOR MUNGING EBIRD DATA --------------------------------------------------
## convert time observation to HOURS SINCE MIDNIGHT
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}


# Unpack files in a directory ---------------------------------------------
unpack_dir <- function(dir="data-raw", overwrite=FALSE){
  ebd.files <- grep("doccor", list.files(dir, full.names=TRUE), value=TRUE)

  for(i in seq_along(ebd.files)){
      tmp<-stringr::str_remove(ebd.files[i], ".zip")
      if(tmp %in% list.files(dir)){
        menu(choices=c("Yes", "No"),
              "testyes",
             cat("Unpacking cancelled.\n")
             )
       if(overwrite=TRUE) unzip(, exdir=dir)
      }else()

  }

    # Unzip the States.zip into the sb_id item directory
  unzip(list.files(sb_dir, full.names=TRUE, pattern="States.zip"), exdir = sb_dir)
  # create an object containing all .zip files in the sb_dir
  states.zipped <- list.files(state_dir, full.names=TRUE, ".zip")

  # UNPACK ALL STATES FILES -------------------------------------------------
  # If neither state nor country are specified, just unpack all the state files.
  if(is.null(state) & is.null(country)){lapply(states.zipped, unzip, exdir=state_dir)}
