
# .make.integer -----------------------------------------------------------
#' @keywords internal
#' @noRd
make.integer <- function(x, var=c("AOU", "aou")){
for(i in seq_along(var)){
  ind=var[i]
    if(ind %in% names(x)){
      x=as.data.frame(x)
      x[,paste(ind)] = as.integer(dplyr::pull(x, ind))
    }else{return(x)}
  }
}



# garbage collection and clear junk -----------------------------------------------------------
#' @keywords internal
#' @noRd
.junk_it <- function(args.save, new.args.save=NULL){
  args.save <- c(args.save, new.args.save)
  rm(list=setdiff(ls(envir = .GlobalEnv), args.save), envir = .GlobalEnv)
  # return(args.save)
}



# mode --------------------------------------------------------------------
#' @keywords internal
#' @noRd
.get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# function to convert time observation to hours since midnight ------------------------------------------------------------------
#' @keywords internal
#' @noRd
.time_to_decimal <- function(x) {
  x <- hms(x)
  hour(x) + minute(x) / 60 + second(x) / 3600
}


# a song to tell me something has stopped ------------------------------------------------------------------
#' @keywords internal
#' @noRd
.song <- function() {
  for(i in 1:2) {
    for(i in 1:4) {
      for(i in 1:4) {
        beep(7)
        Sys.sleep(0.25)
        beep()
        Sys.sleep(0.22)
      }
      beep(2)
    }
    beep(11)
  }
  beep(4)
}



# windows alert -----------------------------------------------------------
#' @keywords internal
#' @noRd
.windows_alert <- function(message=rep("The really long process you started has completed. Hopefully you got it right the first time and don't have to redo it....that took a while.....BEEEEEEEEEEEEEEEP", 3)){
  if(.Platform$OS.type == "windows"){return()} # only run for windows OS
  system2(command = "PowerShell",
          args = c("-Command",
                   "\"Add-Type -AssemblyName System.Speech;",
                   "$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;",
                   paste0("$speak.Speak('", message, "');\"")
          ))
}

