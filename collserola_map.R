# library(rvest)
# library("RJSONIO")
# library("TTR")
library(rgl)
# library(zoom)
# library(gplots)
# library(MASS)
 library(LPCM)
# library(princurve)
#source("functions.R")

path <- "tracks/"
scrap_file <- "collserola_scrap.csv"

####### Functions #######
crop <- function(track, lonlim, latlim){
  track <- track[(track$lon<lonlim[2]) & (track$lon>lonlim[1]) & (track$lat<latlim[2]) & (track$lat>latlim[1]),]
  return(track)  
}

delta_dist <- function(track){
  delta_lat <- abs(track$lat[2:length(track$lat)]-track$lat[1:(length(track$lat)-1)])
  delta_lon <- abs(track$lon[2:length(track$lon)]-track$lon[1:(length(track$lon)-1)])
  delta_x <- sqrt(delta_lat^2 + delta_lon^2)  
  return(delta_x)
}

######## Scrap Collserola Wikiloc #########
remove("track_base")
n=1
for (i in seq(10,14457,10)){
  wikiloc <- read_html(paste0("http://es.wikiloc.com/wikiloc/find.do?act=all&q=collserola&from=", i-10,"&to=",i))
  
  track_link <- wikiloc %>% 
       html_nodes("h3") %>%
       html_nodes("a") %>% html_attr("href") 
  
  for (j in 1:10){
    print(n)
    
    track <- read_html(track_link[j])
    
    a <- track %>% 
         html_nodes("div") %>%
         html_nodes("script") 
    k <- grep('lat',html_text(a))
    a <- a[k]
    k <- grep('trinfo',html_text(a))
    a <-html_text(a[k])
    a <- gsub("\n\t\t\t\t\t\tvar trinfo =", "",  a)
    a <- gsub(";\n\t\t\t\t\t\t", "", a)
    a <- fromJSON(a)
    
    lon <- a$at$n
    ele <- a$at$l
    lat <- a$at$a
    
    id <- rep(n, length(lat))
      
    tr <- data.frame(id = id, lat = lat, lon = lon)
      
    if(!exists("track_base")){track_base <- tr
    }else{track_base <- rbind(track_base, tr)}    
      
    n=n+1
  }
}
write.csv(track_base, paste0(path, scrap_file), row.names = F)

###### Crop scatter map ###### 
track <- read.csv(paste0(path, scrap_file))

lat_lim <- c(41.41, 41.425)
lon_lim <- c(2.09,2.11)

track <- crop(track, lon_lim , lat_lim )

plot(track$lon, track$lat, type='p',  col='black', pch='.')

##### Clean scatter map ######
track$density <- 0

circus <- 0.00001

for (i in 1:nrow(track)){
  print(i)
  track$density[i] <- sum(sqrt((track$lat[i]-track$lat)^2 + (track$lon[i]-track$lon)^2)<circus)
}
write.csv(track, paste0(path, "collserola_scrap_density.csv"), row.names = F)
track <- read.csv(paste0(path, "collserola_scrap_density.csv"))
hist(track$density, breaks=1000)

track_clean <- track[track$density>5,]

plot(track_clean$lon, track_clean$lat, type='p',  col='black', pch='.')

track_clean$density <- NULL
write.csv(track_clean, paste0(path, "collserola_scrap_clean.csv"), row.names = F)

##### Fill seeds #####
track <- read.csv(paste0(path, "collserola_scrap_clean.csv"))

track$id <- NULL

N <- 50
lon_bins <- seq(lon_lim[1],lon_lim[2],(lon_lim[2]-lon_lim[1])/N)
lat_bins <- seq(lat_lim[1],lat_lim[2],(lat_lim[2]-lat_lim[1])/N) 
remove("starting_points")
for(i in 1:(N-1)){
 for(j in 1:(N-1)){
   print(i)
   print(j)
   
   track_sample <- crop(track, lon_bins[i:(i+1)] , lat_bins[j:(j+1)] )
   
  if(nrow(track_sample)>5){
   if(!exists("starting_points")){ starting_points <- track_sample[sample(1:nrow(track_sample),1),1:2]
   }else{starting_points <- rbind(starting_points, track_sample[sample(1:nrow(track_sample),1),1:2])}
  }
 }
}

colors <- palette()[2:length(palette())]
points(starting_points$lon, starting_points$lat, col='red',  type='p' , pch=19, cex=0.5)

new_point <- as.numeric(starting_points[1,])
i <- 0
while(!is.null(new_point)){
  print(i)
  fit <- lpc(track, h=0.0001, scaled=F, x0 = new_point)
  lat=fit$LPC[,1]
  lon=fit$LPC[,2]
  lines(lon, lat, col=colors[i%%length(colors)+1], lwd = 3)
  
  if(i==0){track_fit <- data.frame(lat = lat, lon = lon, id = i)
  }else{track_fit <- rbind(track_fit, data.frame(lat = lat, lon = lon, id = i))}
  
  id_rm <- c()
  for (j in 1:length(lat)){
    distance <- sqrt((lat[j]-starting_points$lat)^2 + (lon[j]-starting_points$lon)^2)
    id_rm <- c(id_rm, which(distance<0.0002))  
  }
  id_rm <- unique(id_rm)
  print(id_rm)
  
  starting_points <- starting_points[setdiff(1:nrow(starting_points), id_rm),]
  new_point <- as.numeric(starting_points[1,])
  i <- i + 1
}
write.csv(track_fit, paste0(path, "track_fit.csv"), row.names = F)

####### Clean acumulation of points #######
track_raw <- read.csv(paste0(path, "track_fit.csv"))

#Interpolation
for (i in 0:max(track_raw$id)){
  num <- nrow(track_raw[track_raw$id==i,])
  if(i==0) track_all <- data.frame(lon = approx(track_raw[track_raw$id==i,]$lon, n=10*num)$y, lat = approx(track_raw[track_raw$id==i,]$lat, n=10*num)$y, id = i)
  else track_all <- rbind(track_all, data.frame(lon = approx(track_raw[track_raw$id==i,]$lon, n=10*num)$y, lat = approx(track_raw[track_raw$id==i,]$lat, n=10*num)$y, id = i))
} 

delta_lat <- abs(track_all$lat[1:(length(track_all$lat)-1)]-track_all$lat[2:length(track_all$lat)])
delta_lon <- abs(track_all$lon[1:(length(track_all$lon)-1)]-track_all$lon[2:length(track_all$lon)])
delta <- sqrt(delta_lat^2 + delta_lon^2)
hist(log10(delta), breaks=100)
mask <- delta>0.000001
track <- track_all[mask,]

####### Clean overlaps ######
step <- 0.00005
track$nearest <- 0 
for (i in 1:length(track$lat)){
  print(i)
  Dlat = track$lat[i] - track$lat[1:i]
  Dlon = track$lon[i] - track$lon[1:i]
  D = sqrt(Dlat^2+Dlon^2)
  
  id_near <- which(step > D)
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

id_nooverlap <- which(track$nearest==0)
plot(track$lon, track$lat, type='p', pch='.',  col='blue')
plot(track$lon[id_nooverlap], track$lat[id_nooverlap], type='p', pch='.',  col='blue')
track <- track[id_nooverlap,]

####### Split disjoint tracks ########
delta_lat <- abs(track$lat[1:(length(track$lat)-1)]-track$lat[2:length(track$lat)])
delta_lon <- abs(track$lon[1:(length(track$lon)-1)]-track$lon[2:length(track$lon)])
delta <- sqrt(delta_lat^2 + delta_lon^2)
#hist(log10(delta), breaks=100)

id_split <- which(delta > 0.0001)

track$id[1:(id_split[2]-1)] <- 1
for (i in 2:(length(id_split)-1)){
  track$id[(id_split[i]+1):id_split[i+1]] <- i  
}
track$id[(id_split[i+1]+1):nrow(track)] <- i+1 

for (i in 0:(max(track$id)-1)){
 if(i==0){plot(track$lon[track$id==i], track$lat[track$id==i], type='l', col=colors[i%%(length(palette())-1)+1], pch='.',
               xlim = c(min(track$lon),max(track$lon)), ylim = c(min(track$lat),max(track$lat)))
 }else{lines(track$lon[track$id==i], track$lat[track$id==i], col=colors[i%%(length(palette())-1)+1], pch='.')}
}

####### Clean small tracks ######
delta_lat <- abs(track$lat[1:(length(track$lat)-1)]-track$lat[2:length(track$lat)])
delta_lon <- abs(track$lon[1:(length(track$lon)-1)]-track$lon[2:length(track$lon)])
delta <- sqrt(delta_lat^2 + delta_lon^2)

track_dist <- c()
for (i in 1:max(track$id)){
  mask <- which(track$id == i)
  mask <- mask[1:(length(mask)-1)]
  track_dist <- c(track_dist, sum(delta[mask]))
}
hist(track_dist, breaks=100)

mask <- track$id %in% which(track_dist > 0.0005)
track2 <- track[mask,]
for (i in unique(track2$id)){
 if(i==1){plot(track2$lon[track2$id==i], track2$lat[track2$id==i], type='l', col=colors[i%%(length(palette())-1)+1], pch='.',
               xlim = c(min(track2$lon),max(track2$lon)), ylim = c(min(track2$lat),max(track2$lat)))
 }else{lines(track2$lon[track2$id==i], track2$lat[track2$id==i], col=colors[i%%(length(palette())-1)+1], pch='.')}
}