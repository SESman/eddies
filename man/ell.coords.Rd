% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/pkg_tools.r
\name{ell.coords}
\alias{ell.coords}
\title{Ellipse coordinates}
\usage{
ell.coords(x0, y0, major, minor, agl, n = 50, ...)
}
\arguments{
\item{x0}{x coordinate of the center}

\item{y0}{y coordinate of the center}

\item{major}{semi-length of the major axe}

\item{minor}{semi-length of the minor axe}

\item{agl}{rotation angle  from the x-axis}

\item{n}{the number of points to use}
}
\description{
Ellipse coordinates
}
\examples{
str(ell.coords(.5, .5, .3, .1, pi/6))
}
\keyword{internal}

