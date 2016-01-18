library(rgl)
source("functions.R")
library(zoom)
library(stats)

step <- 0.0001

delta_dist <- function(track){
  delta_lat <- abs(track$lat[2:length(track$lat)]-track$lat[1:(length(track$lat)-1)])
  delta_lon <- abs(track$lon[2:length(track$lon)]-track$lon[1:(length(track$lon)-1)])
  delta_x <- sqrt(delta_lat^2 + delta_lon^2)  
  return(delta_x)
}

interpol_track <- function(track, num){
  track <- data.frame(lon = approx(track$lon, n=num)$y, lat = approx(track$lat, n=num)$y)
}

######### Main ########
path <- "tracks/"
track <- read.csv(paste0(path, "collserola_scrap.csv"))

track$nearest <- 0 

delta_x <- delta_dist(track)

hist(log10(delta_x[delta_x<0.01]), n = 1000)
step_distance <- mean(delta_x)

hist(step_distance)

track1 <- track[track$id==1,]
plot(track1$lon, track1$lat, type='p',  col='blue', pch=16, cex = .5)
lines(track1_interpol$lon, track1_interpol$lat, type='p',  col='red', pch='.')

delta_x1 <- delta_dist(track1)
h <- hist((delta_x1), n = 1000)
h$mids[which(max(h$counts)==h$counts)]
factor <- h$mids[which(max(h$counts)==h$counts)] / 0.0001

track1_interpol <- interpol_track(track1, round(nrow(track1)*factor, digits=0)) 
delta_x1 <- delta_dist(track1_interpol)
h <- hist((delta_x1), n = 1000)
h$mids[which(max(h$counts)==h$counts)]  


  for (i in 1:length(track$lat)){
    print(i)
    Dlat = track$lat[i] - track$lat[1:i]
    Dlon = track$lon[i] - track$lon[1:i]
    D = sqrt(Dlat^2+Dlon^2)
    
    id_near <- which(10*step > D)
    id_near <- id_near[abs(i-id_near)>15]
    
    if(length(id_near)>0){
      index_split <-  which(id_near[2:length(id_near)] - id_near[1:(length(id_near)-1)]>15)
    
      if(length(index_split)>0){
        id_near_list <- list()
      
        for (j in 1:length(index_split)){
          if(j==1) id_near_list[[j]] <- id_near[1:index_split[j]]    
          else id_near_list[[j]] <- id_near[(index_split[j-1]+1):index_split[j]]      
        }
    
        id_nearest <- c()
        for (j in 1:length(index_split)){
          id_near <- id_near_list[[j]]
          id_nearest <- c(id_nearest, id_near[which(min(D[id_near]) == D[id_near])])
        }
        track$nearest[i] <- paste(id_nearest, collapse = ',')
        
      }else{
          id_nearest <-  id_near[which(min(D[id_near]) == D[id_near])]
          track$nearest[i] <- paste0(id_nearest)
      }
      
    }else{
      track$nearest[i] <- '0'
    }
    
  }

id_rep <- paste(track$nearest, collapse=',')

id_rep <- as.numeric(unlist(strsplit(id_rep, split=",")))
id_rep <- id_rep[id_rep !=0]
id_rep <- unique(id_rep)

plot(track$lon, track$lat, type='p',  col='blue', pch='.')

id_unique <- setdiff(1:nrow(track), id_rep)
track_clean <- track[id_unique,]

zm()
plot(track_clean$lon, track_clean$lat, type='p',  col='blue', pch='.')
