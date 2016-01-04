install.packages("plotKML")
install.packages("rgdal")
install.packages("Hmisc")
library("plotKML")
library(XML)

install.packages("rgeos")
require(rgeos)

file = "6a-caminada-popular-cims-dels-tres-turons.gpx"

a <- readGPX("6a-caminada-popular-cims-dels-tres-turons.gpx")

# Parse the GPX file
pfile <- htmlTreeParse(file, error = function (...) {}, useInternalNodes = T)
# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
head(geodf)
