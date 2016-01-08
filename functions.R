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
    
    id_near <- which(3*step_distance > D)
    id_near <- id_near[abs(i-id_near)>20]
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
  
  edges_clean <- edges[m,]
  edges_clean[which(!m),]$min <- edges[!m,]$min
  
  rownames(edges_clean) <- 1:nrow(edges_clean)
  
  return(edges_clean)
}