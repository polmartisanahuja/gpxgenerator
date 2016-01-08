library(rgl)
source("functions.R")

######### Main ########
file1 = "tracks/trencacames-per-st-llorenc-i-lobac.gpx"
track <- read_gpx(file1)
track <- smooth_gpx(track)

track <- track[(!is.na(track$lat)),]
rownames(track) <- 1:nrow(track)

track_list <- split_gpx(track)
plot_gpx(track_list)


file2 = "tracks/hospital-de-sang-serra-de-lobac.gpx"

track <- read_gpx(file2)
track <- smooth_gpx(track)

track <- track[(!is.na(track$lat)),]
rownames(track) <- 1:nrow(track)

plot3d(track$lon, track$lat, track$ele, type='l', col=palette()[1])

track_list <- split_gpx(track)
plot_gpx(track_list)
