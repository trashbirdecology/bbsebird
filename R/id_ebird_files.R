#' @title Identify and Decompress Relevant eBird Files for Downstream Importing and Munging
#' @param dir.ebird.in Directory for where the ebird data are stored.
#' @param species Character vector comprising species identifiers.
#' @param country.code.identifier Identifier used by eBird to define countries. Should not be changed unless eBird changes its practice of using the iso2c
#' @param mmyyyy The month (mm) and year (yyyy) of the ebird dataset you wish to use. Suggested to use most recent on file.
#' @param get.full.data Logical if TRUE will retrieve the filenames for the ENTIRE database, thereby ignoring the species and regions
#' @export
#' @param states.ind one or more two-letter state codes
#' @importFrom stringr "str_detect"
id_ebird_files <-function(dir.ebird.in,
                           mmyyyy = "nov-2021",
                           species="doccor",
                           country.code.identifier="iso2c", ## need to incorporate this or remove.
                           get.full.data=FALSE,
                           states.ind=NULL
){

  # Simple Tests and Create Simple Indexes
  if(!str_detect(mmyyyy, "-")) stop("argument `mmyyyy` must include hyphen between month and year (i.e. mm-yyyy).")
  mmyyyy <- tolower(mmyyyy)
  regions          <- c("us", "ca", "usa", "mx", "mex")
  country.spp      <-
    paste(tolower(apply(
      expand.grid(paste0("ebd_", regions, "_"), species), 1, paste, collapse = ""
    )), collapse = "|")

  state.spp       <-
    paste(tolower(apply(
      expand.grid(paste0(regions, sep = "-.{2}_"), species), 1, paste, collapse = ""
    )), collapse = "|")

  # List All Files in Main and Subdirectories
  fns.all <- tolower(list.files(dir.ebird.in, recursive = TRUE, full.names=FALSE))
  fns.all <- fns.all[!grepl(fns.all, pattern = "usfws|terms_of_use|citation|ibacodes|bcrcodes|metadata")] # remove junk
  # Identify files/dirs with mmyyyy
  fns.all <- fns.all[grepl(fns.all, pattern = mmyyyy)] # remove junk

  # Step 1: Identify the sampling events data filename
  fns_samp     <- fns.all[grepl(fns.all, pattern="ebd_sampling")]
  fns_samp.tar <- paste0(dir.ebird.in, "/", fns_samp[grepl(fns_samp, pattern=".tar")])
  fns_samp.tar.contents <- untar(tarfile = fns_samp.tar, list = TRUE)
  f_samp <- paste0(dir.ebird.in, "/", fns_samp.tar.contents[grepl(fns_samp.tar.contents, pattern=".gz")]) # get the name of the .txt.gz filwithin the tarball that is our target.
  while(!file.exists(f_samp)){
    cat("Unpacking ", fns_samp.tar, "\n")
    untar(tarfile = fns_samp.tar, list = FALSE, exdir = dir.ebird.in)} # unpack it if it DNE...


  # Step 3: Identify the observations data filename(s)
  fns_obs <- setdiff(fns.all, fns_samp) # remove the sampling events filenames

  ## Filter by species if specified
  if(!is.null(species) & !get.full.data){
    if(is.null(states.ind)){region=country.spp}else{region=state.spp}
      fns_obs.zip <- fns_obs[grepl(fns_obs,pattern= ".zip")]
      
  for(i in seq_along(fns_obs.zip)){
      if(i==1) f_obs <- NULL
      f_zip <- paste0(dir.ebird.in, "/", fns_obs.zip[i])
      temp  <- tolower(unzip(f_zip, list=TRUE)[,1])# grab filenames
      f_txt <- temp[grepl(temp, pattern=region)]
      if(length(f_txt)==0)next()
      f_txt.full <- paste0(dir.ebird.in,"/",f_txt)
      ind   <- file.exists(f_txt.full)

      if(!ind) suppressWarnings(unzip(f_zip, exdir = dir.ebird.in, overwrite = FALSE))
      f_obs <- c(f_obs, f_txt.full)
    } # end i loop fns_obs.spp
  } # END FILTER FILES BY SPECIES WHEN get.full.data==FALSE

if(get.full.data){
    fns_obs.full <- fns_obs[grepl(fns_obs, pattern=paste0("ebd_rel", mmyyyy))]
    if(length(fns_obs.full)==0){warning("No files named ebd_rel found. Please specify get.full.data=FALSE or check mmyyyy specification.")}
    f_tar <- fns_obs.full[grepl(fns_obs.full, pattern=".tar")]
    temp <- tolower(untar(paste0(dir.ebird.in,"/",f_tar), list=TRUE))
    f_txt <- temp[grepl(temp, pattern=paste0("ebd_rel", mmyyyy, ".txt.gz"))]

    f_tar.full <-  paste0(dir.ebird.in,"/",f_tar)
    f_txt.full <-  paste0(f_tar.full,"/",f_txt)

    f_txt.out <- paste0(dir.ebird.in, "/", f_txt)
    ind <- file.exists(f_txt.out)

    # If f_txt.out DNE, then unpack the primary tar ball to dir.ebird.in
    while(!file.exists(f_tar.full)){ untar(tarfile = f_tar.full, exdir=dir.ebird.in)
      cat("Decompressing the full observations file within ", f_tar,". This may take a few minutes.\n")
    }

    f_obs <- f_text.out
  } # end get.full.data

  # Step 4: Return object
output <- c(f_samp, f_obs)
  return(output)

} # END FUNCTION
