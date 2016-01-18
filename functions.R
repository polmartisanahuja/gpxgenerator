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

 nearest_gpx <- function(track){
   track$nearest <- 0 
   delta_lat <- abs(track$lat[2:length(track$lat)]-track$lat[1:(length(track$lat)-1)])
   delta_lon <- abs(track$lon[2:length(track$lon)]-track$lon[1:(length(track$lon)-1)])
   step_distance <- mean(sqrt(delta_lat^2 + delta_lon^2))
   
   for (i in 1:length(track$lat)){
     Dlat = track$lat[i] - track$lat[1:i]
     Dlon = track$lon[i] - track$lon[1:i]
     D = sqrt(Dlat^2+Dlon^2)
     
     id_near <- which(2*step_distance > D)
     id_near <- id_near[abs(i-id_near)>15]
     if(length(id_near)>0){
       id_nearest <- id_near[which(min(D[id_near]) == D[id_near])]
       track$nearest[i] <- id_nearest
     }
   }
   
   return(track)
 }

find_edges <- function(id_overlap){
  id_cut <- which(abs(id_overlap[2:length(id_overlap)]-id_overlap[1:(length(id_overlap)-1)]) > 5)
  id_edges <- sort(c(1, id_cut, id_cut+1, length(id_overlap)))
  
  df_edges <- data.frame(min=numeric(length(id_edges)/2), max=numeric(length(id_edges)/2))
  for (i in 1:(length(id_edges)/2) ){
    df_edges$min[i] <- min(id_overlap[id_edges[2*i-1]:id_edges[2*i]])
    df_edges$max[i] <- max(id_overlap[id_edges[2*i-1]:id_edges[2*i]])
  }
  df_edges$id_track <- 1:nrow(df_edges)
  return(df_edges)
}

edges <- function(track){
  track_overlap <- track[track$nearest != 0,]
  edges <- rbind(find_edges(track[track$nearest != 0,]$nearest), find_edges(as.numeric(rownames(track_overlap))))
  edges <- edges[order(edges$min),]
  
  m <- edges$min[2:nrow(edges)] - edges$max[1:(nrow(edges)-1)] > 20 
  m <- c(m,TRUE)
  m_false <- which(m)
  m_true <- which(!m)
  
  if(nrow(edges)>2){
    edges[m_true+1,]$min <- edges[m_true,]$min
    edges_clean <- edges[m,]
    #edges_clean[which(!m),]$min <- min_edges
  }else{
    if(!m){
      edges_clean <- edges[2,]
      edges_clean[1,]$min<- edges[1,]$min
    }else{
      edges_clean <- edges[2,]
    }
  }
  
  rownames(edges_clean) <- 1:nrow(edges_clean)
  
  return(edges_clean)
}

edges_reduce <- function(edges_clean, track){ 
  n_track_overlap <- max(edges_clean$id_track)

  if(nrow(edges_clean)>1){
    for (i in 1:(nrow(edges_clean)-1)){
      edges_clean <- rbind(edges_clean, c(edges_clean$max[i]+1, edges_clean$min[i+1]-1, i+n_track_overlap))  
    }
  }else{
    i <- 1
  }
  
  if(min(edges_clean$min) != 1) edges_clean <- rbind(edges_clean, c(1, min(edges_clean$min)-1, i+n_track_overlap))  
  if(max(edges_clean$max) != nrow(track)) edges_clean <- rbind(edges_clean, c(max(edges_clean$max)+1, nrow(track), i+n_track_overlap+1)) 
  
  n_track_overlap <- max(edges_clean$id_track)
  
  rownames(edges_clean) <- 1:nrow(edges_clean)
  
  n_track_overlap <- max(edges_clean$id_track)
  
  df_edges <- data.frame(min=numeric(n_track_overlap), max=numeric(n_track_overlap),  id_track=numeric(n_track_overlap) )
  for (i in 1:n_track_overlap){
    df_edges[i,] <- edges_clean[edges_clean$id_track==i,][1,]
  }
  
  return(df_edges)
  
}

track_split <- function(track, df_edges){
  df_list <- list()
  for (i in 1:nrow(df_edges)){
    id_points <- df_edges$min[i]:df_edges$max[i]
    df_list[[i]] <-  data.frame(lon = track$lon[id_points], lat = track$lat[id_points], ele = track$ele[id_points],  id_track=i)
  }
  
  return(df_list)
}

split_gpx <- function(track){
  
  track <- nearest_gpx(track)
  edges_clean <- edges(track)
  df_edges <- edges_reduce(edges_clean, track)
  df_list <- track_split(track, df_edges)
  
  return(df_list)
}

plot_gpx <- function(track_list, track){
  plot3d(track_list[[1]]$lon, track_list[[1]]$lat, track_list[[1]]$ele, type='l', col=palette()[1], 
         xlim=c(min(track$lon),max(track$lon)),
         ylim=c(min(track$lat),max(track$lat)),
         zlim=c(min(track$ele),max(track$ele))
         )
  
  for (i in 2:length(track_list)){
    lines3d(track_list[[i]]$lon, track_list[[i]]$lat, track_list[[i]]$ele,  col=palette()[i])
  }
}

remove_short <- function(track_list){
  track_list_clean <- list()
  j=1
  for (i in 1:length(track_list)){
    if(nrow(track_list[[i]])>15){
      track_list_clean[[j]] <- track_list[[i]]
      j <- 1 + j
    }
  }
  
  return(track_list_clean)
}