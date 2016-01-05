library(rgl)
source("functions.R")

######### Main ########
file1 = "tracks/trencacames-per-st-llorenc-i-lobac.gpx"
track1 <- read_gpx(file1)
plot3d(track1$lon, track1$lat, track1$ele, type='l', col='blue')
track1 <- smooth_gpx(track1)
lines3d(track1$lon, track1$lat, track1$ele, col='red')

#file2 = "tracks/hospital-de-sang-serra-de-lobac.gpx"
#track2 <- read_gpx(file2)
#lines3d(track2$lon, track2$lat, track2$ele, col='red')