% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idfocean.R
\name{idfocean}
\alias{idfocean}
\title{idfocean}
\usage{
idfocean(lon, lat)
}
\arguments{
\item{lon}{Input the longitude.}

\item{lat}{Input the latitude.}
}
\value{
the ocean of \code{lon} and \code{lat}.
}
\description{
Return The Pacific Ocean(PAC), Indian Ocean(IND) or Atlantic Ocean(ATL) of your coordinate.
}
\examples{
idfocean(125,20)
}
