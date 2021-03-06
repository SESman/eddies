% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/import.R
\name{eddies}
\alias{eddies}
\title{Main function: import eddies data into R}
\usage{
eddies(date, type = c("both", "cyc", "antcyc"), ...)
}
\arguments{
\item{date}{Date of the eddy}

\item{type}{Type of the eddy}

\item{...}{Other arguments to be passed to \code{\link{read_eddy}}.}
}
\description{
Main function: import eddies data into R
}
\details{
\itemize{
\item Area: the  number of pixels in the eddy feature
\item MajorAxisLength: Scalar specifying the length (in pixels) of the major
axis of the eddy feature that has the same normalized second central moments
as the eddy region.
\item MinorAxisLength: the length (in pixels) of the minor axis of the eddy
that has the same normalized second central moments as the eddy region.
\item Orientation: the angle (in degrees ranging from -90 to 90 degrees)
between the x-axis and the major axis of the eddy that has the same
second-moments as the eddy region.
\item ConvexImage: Binary image (0 or 1) that specifies the convex hull,
with all pixels within the hull filled in (i.e., set to on). A Convex
Hull is minimal set that can contain a set of pixels
\item Extrema: 8-by-2 matrix that specifies the extrema points in the region
\item Solidity: Scalar specifying the proportion of the pixels in the convex
hull that are also in the feature. This tell us how compact or distorted
the feature is
\item Extent: Scalar that specifies the ratio of pixels in the region
to pixels in the total bounding box.
\item PixelIdList: p-element vector containing the linear indices of the
pixels the form of the eddy's body
\item Intensity the maximum (for anticyclonic) or minimum (for cyclonic)
SLA value within the eddy
\item Lat: the latitude value of eddy centroid.
\item Lon: the longitude value of eddy centroid.
\item Amplitude: the magnitude of the difference between the estimated
basal height of the eddy boundary and the extremum value of SLA within the
eddy interior.
\item ThreshFound: thresholding value when the eddy was found.
\item SurfaceArea: The surface area in squared kilometers
\item Date: the date eddy was found.
\item MeanGeoSpeed: the mean geostrophic speed computed from the SLA anomalies
within the eddy's contour
\item Cyc: the eddy's rotational direction -1 for cyclonic, 1 for anti-cyclonic.
}
}
\examples{
\dontrun{
days <- as.POSIXct(c("2010-01-01", "2011-01-01"))
edd <- eddies(days)
}
}
\references{
Faghmous, J. H., Frenger, I., Yao, Y., Warmka, R., Lindell, A. and Kumar, V.
(2015). A daily global mesoscale ocean eddy dataset from satellite altimetry.
Scientific Data 2, 150028.
}
\seealso{
\code{\link{select_eddy}}, \code{\link{read_eddy}}
}

