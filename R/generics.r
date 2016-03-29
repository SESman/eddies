#' Plot eddies data
#'
#' @param x An \code{"eddy"} table such as returned by \code{\link{read_eddy}}.
#' @param cnd A logical vector to be used to subset x befor plotting.
#' @param add Should the data be plotted on existing graph ?
#' @param cex the size inflation factor of the features.
#' @details Blue ellipses indicate cyclonic eddies (rotating clockwise in the
#' southern hemisphere) while red ellipses indicate anticyclonic eddies.
#' The solid lines represent the boundaries of the eddies for the latest
#' date available in the input \code{x}. Points indicate the center of the features
#' with their size proportional to the eddy solidity. Line width indicate the mean
#' geostrophic speed
#' @export
#' @examples
#' \dontrun{
#' edd <- eddies(as.POSIXct("2010-01-01"))
#' plot(edd)
#' }
plot.eddy <- function(x, cnd = NULL, add = FALSE, cex = 1,
                      col = c(cyc = "blue", anticyc = "red"), ...) {
  x <- x["if"(is.null(cnd), rep(TRUE, nrow(x)), cnd), ]

  if (!add) plot(Lat ~ Lon, x, type = "n")
  is_last <- x$Date == max(x$Date)
  is_cycl <- x$Cyc == -1
  cols <- ifelse(is_cycl, col[1], col[2])
  with(x, ellipses(Lon, Lat,
                   major = unlist(MajorAxisLength) * options()$eddies.pixel_size / 2 *
                     cex * unlist(Extent),
                   minor = unlist(MinorAxisLength) * options()$eddies.pixel_size / 2 *
                     cex * unlist(Extent),
                   agl = unlist(Orientation) * pi /180,
                   col = cols,
                   lwd = x$MeanGeoSpeed / 100,
                   lty = ifelse(is_last, 1, 3))
  )
  with(x[is_last, ],
       points(Lon, Lat, cex = unlist(Solidity), pch = 20,
              col = cols[is_last])
  )

  invisible()
}
