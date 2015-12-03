#' rbindlist alias
#'
#' @inheritParams data.table::rbindlist
#' @export
#' @keywords internal
rblist <- function(l, use.names = fill, fill = FALSE)
  as.data.frame(rbindlist(l, use.names, fill))

#' Plot an ellipse to an existing plot
#'
#' @param x0 x coordinate of the center
#' @param y0 y coordinate of the center
#' @param major semi-length of the major axe
#' @param minor semi-length of the minor axe
#' @param agl rotation angle  from the x-axis
#' @param ... other arguments to be passed to \code{\link{lines}}
#' @export
#' @keywords internal
#' @examples
#' plot(0:1, 0:1)
#' ellipse(.5, .5, .3, .1, pi/6)
ellipse <- function(x0, y0, major, minor, agl, n = 50, ...) {

  theta <- seq(0, 2 * pi, length = n)
  x <- x0 + major * cos(theta) * cos(agl) - minor * sin(theta) * sin(agl)
  y <- y0 + major * cos(theta) * sin(agl) + minor * sin(theta) * cos(agl)

  lines(x, y, type = "l", ...)
}
