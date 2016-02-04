library(rvest)
library("RJSONIO")
library("TTR")
library(rgl)
library(zoom)
library(gplots)
library(MASS)
library(LPCM)
library(princurve)

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

######## Read track database ########
track <- read.csv(paste0(path, scrap_file))

####### Scatter Plot whole map ########
track <- crop(track, c(2,2.2), c(41.35, 41.5))
plot(track$lon, track$lat, type='p',  col='blue', pch='.')
zm()

############## KDE for the whole  map ###############
lon_bins <- seq(2,2.2,(2.2-2)/3)
lat_bins <- seq(41.35,41.5,(41.5-41.35)/3)

for(i in 1:3){
  for(j in 1:3){
    print(i)
    print(j)
    track_small <- crop(track, c(lon_bins[i],lon_bins[i+1]), c(lat_bins[j], lat_bins[j+1]))
    
    x=track_small$lon
    y=track_small$lat
    
    z <- kde2d(x,y, n=500, h=c(0.0001,0.0001))
    z$z[z$z<1] <- 1
    
    png(paste0(path, "png_map_", i, "_",j), width = 1000, height = 1000)
    filled.contour(z$x, z$y, log10(z$z), color = terrain.colors, nlevels = 100)
    dev.off()
    rm(z)
  }
}

###### Find paths ###### 
lat_lim <- c(41.41, 41.425)
lon_lim <- c(2.09,2.11)

track <- crop(track, lon_lim , lat_lim )
track$id <- NULL

#plot(track$lon, track$lat, type='p',  col='blue', pch='.')

#x=track$lon
#y=track$lat
#z <- kde2d(x,y, h=c(0.0001,0.0001), n=500)
#image(z)
#filled.contour(z, color = terrain.colors, nlevels = 100)

#track_matrix <- as.matrix(track)
#fit <- principal.curve(track_matrix)

#fit <- lpc(track, h=0.0001, x0=1, depth=1, way="one",  scaled=F, control=lpc.control(iter=100))
# for (i in 1:50){
# fit <- lpc(track, h=0.0001, scaled=F)
# lines(fit$LPC[,2], fit$LPC[,1], col='red', lwd = 3)
# }

N <- 50
lon_bins <- seq(lon_lim[1],lon_lim[2],(lon_lim[2]-lon_lim[1])/N)
lat_bins <- seq(lat_lim[1],lat_lim[2],(lat_lim[2]-lat_lim[1])/N) 
remove("starting_points")
for(i in 1:(N-1)){
 for(j in 1:(N-1)){
   print(i)
   print(j)
   
   track_sample <- crop(track, lon_bins[i:(i+1)] , lat_bins[j:(j+1)] )
   
  if(nrow(track_sample)>30){
   if(!exists("starting_points")){ starting_points <- track_sample[sample(1:nrow(track_sample),1),1:2]
   }else{starting_points <- rbind(starting_points, track_sample[sample(1:nrow(track_sample),1),1:2])}
  }
 }
}
#starting_points_sample <- starting_points[sample(1:nrow(starting_points), 100),]

plot(track$lon, track$lat, type='p',  col='black', pch='.')
#points(starting_points_sample$lon, starting_points_sample$lat, col='red',  type='p' , pch=19, cex=0.5)
#points(starting_points$lon, starting_points$lat, col='red',  type='p' , pch=19, cex=0.5)

colors <- palette()[2:length(palette())]

new_point <- as.numeric(starting_points[1,])
i <- 0
while(!is.null(new_point)){
  print(i)
  fit <- lpc(track, h=0.0001, scaled=F, x0 = new_point)
  lat=fit$LPC[,1]
  lon=fit$LPC[,2]
  lines(lon, lat, col=colors[i%%length(palette())+1], lwd = 3)
  
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

#points(starting_points$lon[id_rm], starting_points$lat[id_rm], col='red',  type='p' , pch=19, cex=0.5)

for (i in 1:max(track_fit$id)){
  if(i==1){plot(track_fit$lon[track_fit$id==i], track_fit$lat[track_fit$id==i], type='l', col=colors[i%%length(palette())+1], lwd = 3,
                xlim = c(min(track_fit$lon),max(track_fit$lon)), ylim = c(min(track_fit$lat),max(track_fit$lat)))
  }else{lines(track_fit$lon[track_fit$id==i], track_fit$lat[track_fit$id==i], col=colors[i%%length(palette())+1], lwd = 3)}
}

track <- track_fit

track$nearest <- 0 

delta_x <- delta_dist(track)

hist(log10(delta_x[delta_x<0.01]), n = 1000)
step <- mean(delta_x)

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
          track$nearest[i] <- paste(id_nearest, collapse = ',')
      }
      
    }else{
      track$nearest[i] <- '0'
    }
    
  }


id_rep <- paste(track$nearest, collapse=',')

id_rep <- as.numeric(unlist(strsplit(id_rep, split=",")))
id_rep <- id_rep[id_rep !=0]
id_rep <- unique(id_rep)


id_unique <- setdiff(1:nrow(track), id_rep)
track_clean <- track[id_unique,]

zm()
plot(track$lon, track$lat, type='p',  col='red', pch='.')
points(track_clean$lon, track_clean$lat,  col='blue', pch='.')
plot(track_clean$lon, track_clean$lat, type='p',  col='blue', pch='.')

for (i in 1:max(track_clean$id)){
  if(i==1){plot(track_clean$lon[track_clean$id==i], track_clean$lat[track_clean$id==i], type='l', col=colors[i%%length(palette())+1], lwd = 3,
                xlim = c(min(track_clean$lon),max(track_clean$lon)), ylim = c(min(track_clean$lat),max(track_clean$lat)))
  }else{lines(track_clean$lon[track_clean$id==i], track_clean$lat[track_clean$id==i], col=colors[i%%length(palette())+1], lwd = 3)}
}

rname <- as.numeric(rownames(track_clean))
id_split <- which(rname[2:length(rname)]-rname[1:(length(rname)-1)]>10)
for (i in 1:(length(id_split)-1)){
  if(i==1) track_clean$id[1:(id_split[2]-1)] <- 1
  else track_clean$id[(id_split[i]+1):id_split[i+1]] <- i  
}