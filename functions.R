library("TTR")
library(XML)

read_gpx <- function(file){
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
  track <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
  
  return(track) 
}

smooth_gpx <- function(track){
  track$lat <- SMA(track$lat)
  track$lon <- SMA(track$lon)
  track$ele <- SMA(track$ele)
  
  return(track)
}