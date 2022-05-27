# # https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip
# ## this script will download and save BCR shapefile to inst
# con = "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"
#
# pkgfn <- paste0(system.file(package = "bbsebird"), "/bcr.zip")
#
# if(!file.exists(pkgfn)){
#
# dir <- ifelse(Sys.info()[1]=="Linux",  "/tempdir/" , tempdir())
# dir.create(dir, showWarnings = FALSE)
#
# destfile  = paste0(dir, "/bcr.zip")
#
# download.file(url = con,
#               destfile = destfile,
#               quiet = TRUE)
#
# file.copy(from=destfile, to=pkgfn, overwrite = FALSE)
# unlink(dir)
# } # end download
#
#
# fns <- unzip(pkgfn,list=TRUE) #list filenames
#
#
#
#
#
#
