#' Compute the coordinates of the edges of an eddy
#'
#' @param x An "eddy" object as obtained using \code{\link{eddies}}.
#' @param ... arguments to be passed to \code{\link{ell.coords}}. Setting \code{n}
#' to a lower level speeds up computing time.
#' @keywords internal
#' @export
eddies_edges <- function(x, ...) {
  pix_size <- options()$eddies.pixel_size / 2
  out <- Map(ell.coords,
             x0 = x$Lon, y0 = x$Lat,
             major = unlist(x$MajorAxisLength) * pix_size * unlist(x$Extent),
             minor = unlist(x$MinorAxisLength) * pix_size * unlist(x$Extent),
             agl = unlist(x$Orientation) * pi /180, ...)
  out <- lapply(out, as.data.frame)
  nr <- median(sapply(out, nrow))
  out <- rblist(out)
  out$no_eddy <- rep(seq_along(x$Lat), each = nr)
  out
}

#' Match a location to the edges of an eddy
#'
#' @param lon location longitude
#' @param lat location latitude
#' @param edges_coords the coordinates of the edges of the eddies in \code{x}.
#' This arguments allows to speed up calculation when this function is called
#' from \code{\link{match_track2eddies}} and the same eddies are considered again
#' and again. See \code{\link{eddies_edges}}.
#' @inheritParams eddies_edges
#' @seealso \code{\link{match_track2eddies}}
#' @export
match_loc2eddies <- function(x, lon, lat, edges_coords = NULL, ...) {
  stopifnot(require("fields"))
  df <- na.omit(data.frame(lon, lat))
  if (nrow(df) == 0) return(NULL)

  # Get coordinates of the eddies' edges
  if (is.null(edges_coords)) {
    edges_coords <- eddies_edges(x, ...)
  }

  # Find closest match to location
  dist_mat <- as.vector(rdist.earth(df, edges_coords, miles = FALSE))
  mtch_idx <- which.min(dist_mat) %else% return(NULL)

  df$edd.dist <- dist_mat[mtch_idx]
  df$edd.edge_lon <- edges_coords[mtch_idx, 1]
  df$edd.edge_lat <- edges_coords[mtch_idx, 2]
  df$no_eddy <- edges_coords[mtch_idx, 3]
  df[ , paste0("edd.", names(x))] <- x[df$no_eddy, ]

  df
}

#' Match a track to the edges of an eddy
#'
#' @param track a set of locations. First column is longitude and second is latitude.
#' @param lon locations longitude
#' @param lat locations latitude
#' @inheritParams eddies_edges
#' @details If \code{lon} and \code{lat} are non-null then \code{track} is ignored.
#' @seealso \code{\link{match_loc2eddies}}
#' @export
match_track2eddies <- function(x, track, lon = NULL, lat = NULL, ...) {
  if (!missing(track) & is.null(lon) & is.null(lat)) {
    lon <- track[ , 1]
    lat <- track[ , 2]
  }
  edgs <- eddies_edges(x, ...)
  out <- Map(match_loc2eddies, list(x), lon, lat, list(edges_coords = edgs))
  out <- rblist(out)
  merge(data.frame(lon = lon, lat = lat), out, by = c("lon", "lat"), all = TRUE)
}
