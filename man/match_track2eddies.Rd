% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/eddies_matching.r
\name{match_track2eddies}
\alias{match_track2eddies}
\title{Match a track to the edges of an eddy}
\usage{
match_track2eddies(x, track, lon = NULL, lat = NULL, ...)
}
\arguments{
\item{x}{An "eddy" object as obtained using \code{\link{eddies}}.}

\item{track}{a set of locations. First column is longitude and second is latitude.}

\item{lon}{locations longitude}

\item{lat}{locations latitude}

\item{...}{arguments to be passed to \code{\link{ell.coords}}. Setting \code{n}
to a lower level speeds up computing time.}
}
\description{
Match a track to the edges of an eddy
}
\details{
If \code{lon} and \code{lat} are non-null then \code{track} is ignored.
}
\seealso{
\code{\link{match_loc2eddies}}
}

