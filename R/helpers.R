# create var RTENO --------------------------------------------------------
make.integer <- function(x, var=c("AOU", "aou")){
for(i in seq_along(var)){
  ind=var[i]
    if(ind %in% names(x)){
      x=as.data.frame(x)
      x[,paste(ind)] = as.integer(dplyr::pull(x, ind))
    }else{return(x)}
  }
}



# create date and julian date ---------------------------------------------
make.dates <- function(x){
  if(all(c("day","month","year") %in% tolower(names(x)))){
    x= x %>%
      mutate(Date = as.Date(paste(Day, Month, Year, sep = "/"), format = "%d/%m/%Y"))
    x= x %>%
    mutate(julian=julian(Date, origin=min(x$Date)))

    }else(return(x))
}
# create var RTENO --------------------------------------------------------
make.rteno <- function(x){
  # browser()
  if(all(c("CountryNum","StateNum","Route") %in% names(x))){
  RTENO=paste0(
    str_pad(x$CountryNum, width=3, side="left", pad="0"),
    str_pad(x$StateNum, width=2, side="left", pad="0"),
    str_pad(x$Route, width=3, side="left", pad="0"))
  x = x %>% mutate(RTENO=RTENO) %>%
    dplyr::select(-CountryNum, -StateNum, -Route) # delete for mem and b/c this info is already captured in RTENO

  }else(return(x))
}


# mode --------------------------------------------------------------------
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# function to convert time observation to hours since midnight ------------------------------------------------------------------

time_to_decimal <- function(x) {
  x <- hms(x)
  hour(x) + minute(x) / 60 + second(x) / 3600
}


# a song to tell me something has stopped ------------------------------------------------------------------
song <- function() {
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
windows_alert <- function(message=rep("The really long process you started has completed. Hopefully you got it right the first time and don't have to redo it....that took a while.....BEEEEEEEEEEEEEEEP", 3)){
  if(.Platform$OS.type == "windows"){return()} # only run for windows OS
  system2(command = "PowerShell",
          args = c("-Command",
                   "\"Add-Type -AssemblyName System.Speech;",
                   "$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;",
                   paste0("$speak.Speak('", message, "');\"")
          ))
}

