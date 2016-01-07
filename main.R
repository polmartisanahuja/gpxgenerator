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

track_overlap <- track[track$nearest != 0,]

edges <- rbind(find_edges(track[track$nearest != 0,]$nearest), find_edges(as.numeric(rownames(track_overlap))))

edges <- edges[order(edges$min),]

m <- edges$min[2:nrow(edges)] - edges$max[1:(nrow(edges)-1)] > 20

edges_clean <- edges[m,]
edges_clean[which(!m),]$min <- edges[!m,]$min

mask <- c()
for (i in 1:nrow(edges_clean)){
  mask <- c(mask, edges_clean$min[i]:edges_clean$max[i])
}

track_clean <- track[setdiff(1:nrow(track), mask),]
#plot3d(track$lon, track$lat, track$ele, type='l', col='blue')
plot3d(track_clean$lon, track_clean$lat, track_clean$ele, type='p', col='red')


#file2 = "tracks/hospital-de-sang-serra-de-lobac.gpx"
#track2 <- read_gpx(file2)
#lines3d(track2$lon, track2$lat, track2$ele, col='red')