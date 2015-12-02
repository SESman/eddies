.onLoad <- function(libname = find.package("eddies"), pkgname = "eddies") {
  op <- options()
  op.eddies <- list(
    # Path to the database
    eddies.path = "F:/data/eddies",
    # Area for geographical subset
    eddies.area = list(lon = c(0, 115), lat = c(-30, -70)),
    # Size of data pixels:
    #    360 deg / 1440 pixels for longitude
    #    2 * 90 deg / 720 pixels for latitude
    eddies.pixel_size = 0.25
  )
  toset <- !(names(op.eddies) %in% names(op))
  if(any(toset)) options(op.eddies[toset])

  invisible()
}
