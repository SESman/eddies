#' Plot eddies data
#'
#' @param x An \code{"eddy"} table such as returned by \code{\link{read_eddy}}.
#' @param cnd A logical vector to be used to subset x befor plotting.
#' @param add Should the data be plotted on existing graph ?
#' @param cex the size inflation factor of the features.
#' @details Blue ellipses indicate cyclonic eddies (rotating clockwise in the
#' southern hemisphere) while red ellipses indicate anticyclonic eddies.
#' The thicker lines represent the boundaries of the eddies for the latest
#' date available in the input \code{x}.
#' @export
#' @examples
#' \dontrun{
#' edd <- eddies(as.POSIXct("2010-01-01"))
#' plot(edd, cex = .5)
#' }
plot.eddy <- function(x, cnd = NULL, add = FALSE, cex = 1, ...) {
  x <- x["if"(is.null(cnd), rep(TRUE, nrow(x)), cnd), ]

  if (!add) plot(Lat ~ Lon, x, type = "n")
  with(x, Map(ellipse, Lon, Lat,
              unlist(MajorAxisLength) * options()$eddies.pixel_size * unlist(Extent) * cex,
              unlist(MinorAxisLength) * options()$eddies.pixel_size * unlist(Extent) * cex,
              unlist(Orientation) * pi /180,
              col = ifelse(Cyc == 1, "blue", "red"),
              lwd = ifelse(x$Date == max(x$Date), 3, 1))
  )

  invisible()
}
