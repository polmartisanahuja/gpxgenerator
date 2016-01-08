library(rgl)
source("functions.R")

######### Main ########
file1 = "tracks/trencacames-per-st-llorenc-i-lobac.gpx"
track <- read_gpx(file1)
track <- smooth_gpx(track)

track <- track[(!is.na(track$lat)),]
rownames(track) <- 1:nrow(track)

track <- nearest_gpx(track)
edges_clean <- edges(track)

n_track_overlap <- max(edges_clean$id_track)

for (i in 1:(nrow(edges_clean)-1)){
  edges_clean <- rbind(edges_clean, c(edges_clean$max[i]+1, edges_clean$min[i+1]-1, i+n_track_overlap))  
}
rownames(edges_clean) <- 1:nrow(edges_clean)

n_track_overlap <- max(edges_clean$id_track)

df_edges <- data.frame(min=numeric(n_track_overlap), max=numeric(n_track_overlap),  id_track=numeric(n_track_overlap) )
for (i in 1:n_track_overlap){
  df_edges[i,] <- edges_clean[edges_clean$id_track==i,][1,]
}

id_points <- df_edges$min[1]:df_edges$max[1]
plot3d(track$lon[id_points], track$lat[id_points], track$ele[id_points], type='l', col=palette()[1], 
       xlim=c(min(track$lon),max(track$lon)),
       ylim=c(min(track$lat),max(track$lat)),
       zlim=c(min(track$ele),max(track$ele))
       )
for (i in 2:nrow(df_edges)){
  id_points <- df_edges$min[i]:df_edges$max[i]
  lines3d(track$lon[id_points], track$lat[id_points], track$ele[id_points],  col=palette()[i])
}