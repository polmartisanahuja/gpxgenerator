library(rgl)
source("functions.R")

######### Main ########
file1 = "tracks/trencacames-per-st-llorenc-i-lobac.gpx"
track1 <- read_gpx(file1)
#plot3d(track1$lon, track1$lat, track1$ele, type='l', col='blue')
track1 <- smooth_gpx(track1)
#lines3d(track1$lon, track1$lat, track1$ele, col='red')
#plot3d(track1$lon, track1$lat, track1$ele, type='p', col='blue')

track <- track1[(!is.na(track1$lat)),]
rownames(track) <- 1:nrow(track)

track <- nearest_gpx(track)
edges_clean <- edges(track)

n_track_overlap <- max(edges_clean$id_track)

for (i in 1:(nrow(edges_clean)-1)){
  edges_clean <- rbind(edges_clean, c(edges_clean$max[i]+1, edges_clean$min[i+1]-1, i+n_track_overlap))  
}

#mask <- c()
#for (i in 1:nrow(edges_clean)){
#  mask <- c(mask, edges_clean$min[i]:edges_clean$max[i])
#}

#track_clean <- track[setdiff(1:nrow(track), mask),]
#plot3d(track$lon, track$lat, track$ele, type='l', col='blue')
plot3d(track_clean$lon, track_clean$lat, track_clean$ele, type='p', col='red')


#file2 = "tracks/hospital-de-sang-serra-de-lobac.gpx"
#track2 <- read_gpx(file2)
#lines3d(track2$lon, track2$lat, track2$ele, col='red')