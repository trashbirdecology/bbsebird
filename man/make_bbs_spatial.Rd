% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_bbs_spatial.R
\name{make_bbs_spatial}
\alias{make_bbs_spatial}
\title{Manipulate the BBS Route Shapefiles and/or Overlay BBS Data to Grid}
\usage{
make_bbs_spatial(
  df,
  grid,
  cws.routes.dir = NULL,
  usgs.routes.dir = NULL,
  cws.layer = "ALL_ROUTES",
  usgs.layer = "US_BBS_Route-Paths-Snapshot_Taken-Feb-2020",
  crs.target = 4326,
  ncores = parallel::detectCores() - 1,
  keep.empty.cells = TRUE,
  overwrite = FALSE,
  dir.out = NULL,
  save.route.lines = TRUE
)
}
\arguments{
\item{df}{A data frame with columns c(latitude, longitude) and optional attributes (columns).}

\item{grid}{a spatial grid over which the eBird data will be overlaid.}

\item{cws.routes.dir}{Directory for where the CWS (Canadian BBS) shapefiles are stored.}

\item{usgs.routes.dir}{Directory for where the USGS (USA BBS) shapefiles are stored.}

\item{cws.layer}{Name of the layer to import. Defaults to "ALL_ROUTES"}

\item{usgs.layer}{Name of the layer to import.}

\item{crs.target}{the integer representing the target CRS.}

\item{ncores}{max number of cores to engage for parallel data processing. Defaults to one fewer CPUs than the machine has. Parallel processing is used only when a high number of routes and/or grid cells are in the data.}

\item{keep.empty.cells}{logical if FALSE will remove any grid cells with which BBS data do not align. Do not recommend doing this.}

\item{overwrite}{logical if TRUE will overwrite any existing file named "ebird_spatial.rds" in path dir.out}

\item{dir.out}{path to where the resulting spatial data frame will be saved. If NULL will not save to file.}

\item{save.route.lines}{logical. If TRUE (default) will save the BBS routes segments as .RDS to dir.out path.}
}
\description{
This function is meant to intake two shapefiles, one per cws and usgs munges them such that they conform to the USGS data release for BBS observations and route metadata.
}
