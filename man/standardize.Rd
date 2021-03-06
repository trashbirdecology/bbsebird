% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-standardize.r
\name{standardize}
\alias{standardize}
\title{Center and/or Z-Score Transform a Vector or Array}
\usage{
standardize(x, center = TRUE, scale = TRUE)
}
\arguments{
\item{x}{a vector or 2D array of numeric or integer values}

\item{center}{logical if TRUE will center the resulting object}

\item{scale}{logical if TRUE data will be returned as z-score transform of x.}
}
\description{
If both arguments center and scale are FALSE, function will throw an error to ensure user is aware of this potential mistake.
}
