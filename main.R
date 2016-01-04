library(XML)
library(rgl)
source("functions.R")

######### Main ########
file = "6a-caminada-popular-cims-dels-tres-turons.gpx"
geodf <- read_gpx(file)
plot3d(geodf$lon, geodf$lat, geodf$ele, type='l')