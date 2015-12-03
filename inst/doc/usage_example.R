## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(fig.dpi = 96)

## ------------------------------------------------------------------------
require("rbl")
require("eddies")

ses <- readRDS("F:/data/ses/post_repro/rbl/large_acc/2011-28.rds")
ses$stat$day <- floorPOSIXct(ses$stat$time)

# Setup temporal subset
days <- seq(
  from = as.POSIXct("2011-11-25", tz = "UTC"), 
  to = as.POSIXct("2011-12-12", tz = "UTC"), 
  by = "day"
  )

# Before using the package make sure that the database path is correct
# Consider setting this option in your .Rprofile if it is not the case
options("eddies.path")

# Setup geographical subset (Optional)
# options("eddies.area") <- list(
#   lon = range(ses$stat$lon), 
#   lat = range(ses$stat$lat)
#   )
# Here we will use defaults
options("eddies.area")

# Extract matching eddies from the database
edd <- eddies(days)

## ------------------------------------------------------------------------
names(edd) # See description of these variables in ?eddies

cnd <- ses$stat$day %in% days
plot(lat ~ lon, ses$stat[cnd, ], type = "n")
plot(edd, add = TRUE)
lines(lat ~ lon, ses$stat[cnd, ], lwd = 2)

