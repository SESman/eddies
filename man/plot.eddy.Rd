% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/generics.r
\name{plot.eddy}
\alias{plot.eddy}
\title{Plot eddies data}
\usage{
\method{plot}{eddy}(x, cnd = NULL, add = FALSE, cex = 1, col = c(cyc =
  "blue", anticyc = "red"), ...)
}
\arguments{
\item{x}{An \code{"eddy"} table such as returned by \code{\link{read_eddy}}.}

\item{cnd}{A logical vector to be used to subset x befor plotting.}

\item{add}{Should the data be plotted on existing graph ?}

\item{cex}{the size inflation factor of the features.}
}
\description{
Plot eddies data
}
\details{
Blue ellipses indicate cyclonic eddies (rotating clockwise in the
southern hemisphere) while red ellipses indicate anticyclonic eddies.
The solid lines represent the boundaries of the eddies for the latest
date available in the input \code{x}. Points indicate the center of the features
with their size proportional to the eddy solidity. Line width indicate the mean
geostrophic speed
}
\examples{
\dontrun{
edd <- eddies(as.POSIXct("2010-01-01"))
plot(edd)
}
}

