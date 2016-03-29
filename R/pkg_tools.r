#' rbindlist alias
#'
#' @inheritParams data.table::rbindlist
#' @export
#' @keywords internal
rblist <- function(l, use.names = fill, fill = FALSE)
  as.data.frame(rbindlist(l, use.names, fill))

#' Plot ellipses on a existing graph
#'
#' @inheritParams ell.coords
#' @param ... other arguments to be passed to \code{\link{lines}}
#' @export
#' @keywords internal
#' @seealso \code{\link{ell.coords}}
#' @examples
#' plot(0:1, 0:1)
#' ellipses(.5, .5, .3, .1, pi/6)
#'
#' # Arguments are recycled
#' plot(0:1, 0:1)
#' ellipses(c(.5, .75), c(.5, .75), .3, .1, c(pi/6, -pi/2))
ellipses <- function(x0, y0, major, minor, agl, n = 50, ...) {
  # Get all named arguments in a list
  args <- as.list(environment(), all.names = TRUE)
  args <- args[names(args) != "..."]

  # Dispatch task according to the length of elements
  len_args <- sapply(args, length)
  if (any(len_args == 0)) {
    stop("One argument has length 0!")
  } else if (all(len_args == 1)) {
    with(do.call(ell.coords, args), lines(x, y, type = "l", ...))
  } else {
    do.call(Map, c(list(ellipses), args, list(...)))
  }

  invisible(NULL)
}

#' Ellipse coordinates
#' @param x0 x coordinate of the center
#' @param y0 y coordinate of the center
#' @param major semi-length of the major axe
#' @param minor semi-length of the minor axe
#' @param agl rotation angle  from the x-axis
#' @param n the number of points to use
#' @export
#' @keywords internal
#' @examples
#' str(ell.coords(.5, .5, .3, .1, pi/6))
ell.coords <- function(x0, y0, major, minor, agl, n = 50, ...) {
  theta <- seq(0, 2 * pi, length = n)
  list(
    x = x0 + major * cos(theta) * cos(agl) - minor * sin(theta) * sin(agl),
    y = y0 + major * cos(theta) * sin(agl) + minor * sin(theta) * cos(agl)
  )
}
