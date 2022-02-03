#' @title Identify and Decompress Relevant eBird Files for Downstream Importing and Munging
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param species.abbr Character vector comprising species identifiers for filenames.
#' @param mmyyyy The month (mm) and year (yyyy) of the ebird dataset you wish to use. Suggested to use most recent on file.
#' @param get.full.data Logical if TRUE will retrieve the filenames for the ENTIRE database, thereby ignoring the species and e.regions
#' @param dir.ebird.out Location of where to search for existing, created .rds files
#' @param states.ind one or more two-letter state codes
#' @importFrom stringr str_detect
#' @importFrom bbsAssistant region_codes
#' @importFrom utils untar
#' @export
id_ebird_files <- function(dir.ebird.in,
                           dir.ebird.out = NULL,
                           mmyyyy = "nov-2021",
                           species.abbr = "doccor",
                           get.full.data = FALSE,
                           states.ind = NULL) {
  ### NEED TO ADD A MENU FOR WHEN NOT ALL STATES.IND ARE IN THE IDENTIFIED
  ### THEN IT JUST GRABS ALL THE SPECIES-COUNTRY COMBNATONS AND UOTPUTS FOR OBS FILES.

  ### ALSO NEED TOE NSURE A COUNTRY-ONLY ARGUMENT



  rc <- bbsAssistant::region_codes
  rc.temp <-
    tolower(x = gsub(rc$iso_3166_2, pattern = "-", replacement = ""))
  states.ind <-
    gsub(x = tolower(states.ind),
         pattern = "-",
         replacement = "")
  states.ind <- rc[which(rc.temp %in% states.ind),]$iso_3166_2

  # Simple Tests and Create Simple Indexes
  if (!str_detect(mmyyyy, "-"))
    stop("argument `mmyyyy` must include hyphen between month and year (i.e. mm-yyyy).")
  mmyyyy <- tolower(mmyyyy)
  e.regions          <- c("us", "ca", "usa", "mx", "mex")
  country.spp      <-
    paste(tolower(apply(
      expand.grid(paste0("ebd_", e.regions, "_"), species.abbr), 1, paste, collapse = ""
    )), collapse = "|")

  state.spp       <-
    paste(tolower(apply(
      expand.grid(paste0(e.regions, sep = "-.{2}_"), species.abbr), 1, paste, collapse = ""
    )), collapse = "|")

  # List All Files in Main and Subdirectories
  fns.all <-
    tolower(list.files(dir.ebird.in, recursive = TRUE, full.names = FALSE))
  fns.all <-
    fns.all[!grepl(fns.all, pattern = "usfws|terms_of_use|citation|ibacodes|bcrcodes|metadata")] # remove junk
  # Identify files/dirs with mmyyyy
  fns.all <- fns.all[grepl(fns.all, pattern = mmyyyy)] # remove junk
  stopifnot(length(fns.all) >= 1)
  # Step 1: Identify the sampling events data filename
  fns_samp     <- fns.all[grepl(fns.all, pattern = "ebd_sampling")]
  fns_samp.tar <-
    paste0(dir.ebird.in, "/", fns_samp[grepl(fns_samp, pattern = ".tar")])
  fns_samp.tar.contents <-
    untar(tarfile = fns_samp.tar, list = TRUE)
  f_samp <-
    paste0(dir.ebird.in, "/", fns_samp.tar.contents[grepl(fns_samp.tar.contents, pattern =
                                                            ".gz")]) # get the name of the .txt.gz filwithin the tarball that is our target.
  while (!file.exists(f_samp)) {
    cat("Unpacking ", fns_samp.tar, "\n")
    untar(tarfile = fns_samp.tar,
          list = FALSE,
          exdir = dir.ebird.in)
  } # unpack it if it DNE...


  # Step 3: Identify the observations data filename(s)
  fns_obs <-
    setdiff(fns.all, fns_samp) # remove the sampling events filenames

  ## Filter by species if specified
  if (!is.null(species.abbr) & !get.full.data) {
    if (is.null(states.ind)) {
      region = country.spp
    } else{
      region = tolower(paste(states.ind, collapse = "|"))
    }
    ### get files by species
    temp <-
      fns_obs[grepl(fns_obs, pattern = ".zip")]# possibles for the species
    fns_obs.zip <-
      temp[grepl(temp, pattern = region)]# possibles for the region
    ### if no files found then grab the country-level ones
    if (length(fns_obs.zip) == 0) {
      fns_obs.zip <-
        temp[grepl(temp, pattern = country.spp)]
    }

    f_obs <- NULL

    ### keep only those with the sp.abbreviation
    for (i in seq_along(fns_obs.zip)) {
      if (i == 1)
        f_obs <- NULL
      f_zip <- paste0(dir.ebird.in, "/", fns_obs.zip[i])
      temp  <-
        tolower(unzip(f_zip, list = TRUE)[, 1])# grab file names
      f_txt <- temp[grepl(temp, pattern = region)]
      if (length(f_txt) == 0)
        f_txt <- temp[grepl(temp, pattern = country.spp)]
      f_txt <-
        f_txt[grepl(f_txt, pattern = ".txt")]# yes i need to keep both greps here
      if (length(f_txt) == 0) {
        next()
      }
      f_txt.full <- paste0(dir.ebird.in, "/", f_txt)
      ind   <- file.exists(f_txt.full)
      if (!ind)
        suppressWarnings(unzip(f_zip, exdir = dir.ebird.in, overwrite = FALSE))
      f_obs <- c(f_obs, f_txt.full)
    } # end i loop fns_obs.spp
  } # END FILTER FILES BY SPECIES WHEN get.full.data==FALSE

  if (get.full.data) {
    fns_obs.full <-
      fns_obs[grepl(fns_obs, pattern = paste0("ebd_rel", mmyyyy))]
    if (length(fns_obs.full) == 0) {
      warning(
        "No files named ebd_rel found. Please specify get.full.data=FALSE or check mmyyyy specification."
      )
    }
    f_tar <- fns_obs.full[grepl(fns_obs.full, pattern = ".tar")]
    temp <-
      tolower(untar(paste0(dir.ebird.in, "/", f_tar), list = TRUE))
    f_txt <-
      temp[grepl(temp, pattern = paste0("ebd_rel", mmyyyy, ".txt.gz"))]

    f_tar.full <-  paste0(dir.ebird.in, "/", f_tar)
    f_txt.full <-  paste0(f_tar.full, "/", f_txt)

    f_txt.out <- paste0(dir.ebird.in, "/", f_txt)
    ind <- file.exists(f_txt.out)

    # If f_txt.out DNE, then unpack the primary tar ball to dir.ebird.in
    while (!file.exists(f_tar.full)) {
      untar(tarfile = f_tar.full, exdir = dir.ebird.in)
      cat(
        "Decompressing the full observations file within ",
        f_tar,
        ". This may take a few minutes.\n"
      )
    }

    f_obs <- f_text.out
  } # end get.full.data
  f_rds <-
    list.files(dir.ebird.out, full.names = TRUE, pattern = ".rds")


  # Step 4: Return object
  output <- c(f_samp, f_obs, f_rds)
  return(output)

} # END FUNCTION
