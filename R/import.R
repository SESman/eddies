#' Read an eddy file
#'
#' @param x filename
#' @param use.rds Should the \code{".rds"} copy be used to speed up the function
#' if it exists ? Otherwise, the original \code{".mat"} (matlab) file will be used.
#' @param rds.save Should a \code{".rds"} copy of \code{x} be saved for a faster
#' access  to data later on ?
#' @param overwrite Should the \code{"rds"} file be overwritten if it already exists ?
#' @export
#' @details The function uses the path provided by \code{options("eddies.area")}
#' to subset a geographical region of focus. This behavior can be turned off by
#' setting this option to \code{NULL}
#' @examples
#' \dontrun{
#' edd <- read_eddy("F:/data/eddies/2010/anticyc_20100101.mat")
#' }
read_eddy <- function(x, use.rds = TRUE, rds.save = TRUE, overwrite = FALSE) {
  x_rds <- sub("\\.mat$", "\\.rds", x)
  rds_copied <- file.exists(x_rds)
  if (use.rds && rds_copied) {
    return(readRDS(x_rds))
  } else {
    tmp <- R.matlab::readMat(x)[[1]]
  }

  stopifnot(require("data.table"))
  df <- rblist(apply(tmp, 3, function(x) as.list(x[-1])))
  names(df) <- names(tmp[ , , 1])[-1]

  stats_array <- apply(tmp, 3, function(x) as.list(x[[1]]))
  nms <- attr(stats_array[[1]], "dimnames")[[1]]
  tmp <- lapply(stats_array, function(x) {
    attributes(x) <- list(dim = 10, dimnames = list(nms))
    lapply(x, list)
  })
  df[ , nms] <- rblist(tmp)

  if (!is.null(area <- options()$eddies.area)) {
    cnd <- df$Lon <= max(area$lon) & df$Lon >= min(area$lon) &
      df$Lat <= max(area$lat) & df$Lat >= min(area$lat)
    out <- df[cnd, ]
  } else{
    out <- df
  }
  class(out) <- c("eddy", "data.frame")

  if (rds.save && (!rds_copied || overwrite)) saveRDS(out, x_rds)

  out
}

#' Find eddy files
#'
#' @param date Date of the eddy
#' @param type Type of the eddy
#' @export
#' @details The function uses the path provided by \code{options("eddies.path")}
#' to access to the eddies database. The database only includes years from
#' 2010 to 2014 but years from 1993 to 2009 are also available from
#' the downloaded database
#' (\url{http://datadryad.org/resource/doi:10.5061/dryad.gp40h/1}). To get
#' more recent data in a similar format
#' the software used in the article (matlab) was made available
#' online at \url{https://github.com/jfaghm/OceanEddies}.
#' @examples
#' \dontrun{
#' days <- as.POSIXct(c("2010-01-01", "2011-01-01"))
#' select_eddy(day, "both")
#' }
select_eddy <- function(date, type = c("both", "cyc", "antcyc"),
                        ext = c("mat", "rds", "any")) {
  if (length(date) > 1) return(lapply(date, select_eddy, type = type, ext = ext))
  ymd <- format(date, "%Y%m%d")
  type <- switch(match.arg(type, type),
                 antcyc = "^anticyc_", cyc = "^cyclonic_", "^[ac].*_")
  ext <- switch(match.arg(ext, ext),
                any = "\\.((mat)|(rds))$", mat = "\\.mat$", rds = "\\.rds.$")
  patt <- paste0(type, ymd, ext)
  list.files(options()$eddies.path, patt, full.names = TRUE, recursive =  TRUE)
}

#' Main function: import eddies data into R
#'
#' @inheritParams select_eddy
#' @param ... Other arguments to be passed to \code{\link{read_eddy}}.
#' @export
#' @details
#' \itemize{
#' \item Area: the  number of pixels in the eddy feature
#' \item MajorAxisLength: Scalar specifying the length (in pixels) of the major
#' axis of the eddy feature that has the same normalized second central moments
#' as the eddy region.
#' \item MinorAxisLength: the length (in pixels) of the minor axis of the eddy
#' that has the same normalized second central moments as the eddy region.
#' \item Orientation: the angle (in degrees ranging from -90 to 90 degrees)
#' between the x-axis and the major axis of the eddy that has the same
#' second-moments as the eddy region.
#' \item ConvexImage: Binary image (0 or 1) that specifies the convex hull,
#' with all pixels within the hull filled in (i.e., set to on). A Convex
#' Hull is minimal set that can contain a set of pixels
#' \item Extrema: 8-by-2 matrix that specifies the extrema points in the region
#' \item Solidity: Scalar specifying the proportion of the pixels in the convex
#' hull that are also in the feature. This tell us how compact or distorted
#' the feature is
#' \item Extent: Scalar that specifies the ratio of pixels in the region
#' to pixels in the total bounding box.
#' \item PixelIdList: p-element vector containing the linear indices of the
#' pixels the form of the eddy's body
#' \item Intensity the maximum (for anticyclonic) or minimum (for cyclonic)
#' SLA value within the eddy
#' \item Lat: the latitude value of eddy centroid.
#' \item Lon: the longitude value of eddy centroid.
#' \item Amplitude: the magnitude of the difference between the estimated
#' basal height of the eddy boundary and the extremum value of SLA within the
#' eddy interior.
#' \item ThreshFound: thresholding value when the eddy was found.
#' \item SurfaceArea: The surface area in squared kilometers
#' \item Date: the date eddy was found.
#' \item MeanGeoSpeed: the mean geostrophic speed computed from the SLA anomalies
#' within the eddy's contour
#' \item Cyc: the eddy's rotational direction -1 for cyclonic, 1 for anti-cyclonic.
#' }
#' @seealso \code{\link{select_eddy}}, \code{\link{read_eddy}}
#' @references
#' Faghmous, J. H., Frenger, I., Yao, Y., Warmka, R., Lindell, A. and Kumar, V.
#' (2015). A daily global mesoscale ocean eddy dataset from satellite altimetry.
#' Scientific Data 2, 150028.
#' @examples
#' \dontrun{
#' days <- as.POSIXct(c("2010-01-01", "2011-01-01"))
#' edd <- eddies(days)
#' }
eddies <- function(date, type = c("both", "cyc", "antcyc"), ...) {
  stopifnot(require("data.table"))
  fls <- select_eddy(date, type)
  if (!is.recursive(fls)) fls <- list(fls)

  tmp <- lapply(fls, function(x, ...) lapply(x, read_eddy, ...), ...)
  tmp <- lapply(tmp, function(x) as.data.frame(rbindlist(x)))

  out <- rblist(tmp)
  out$Date <- as.POSIXct(out$Date, format = "%Y%m%d", tz = "UTC")
  class(out) <- c("eddy", "data.frame")
  out
}
