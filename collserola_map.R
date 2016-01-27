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

###### Plot kde map ###### 
lat_lim <- c(41.41, 41.425)
lon_lim <- c(2.09,2.11)

track <- crop(track, lon_lim , lat_lim )
track$id <- NULL

plot(track$lon, track$lat, type='p',  col='blue', pch='.')

x=track$lon
y=track$lat
z <- kde2d(x,y, h=c(0.0001,0.0001), n=500)
#image(z)
filled.contour(z, color = terrain.colors, nlevels = 100)


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

plot(track$lon, track$lat, type='p',  col='blue', pch='.')
#points(starting_points_sample$lon, starting_points_sample$lat, col='red',  type='p' , pch=19, cex=0.5)
#points(starting_points$lon, starting_points$lat, col='red',  type='p' , pch=19, cex=0.5)

new_point <- as.numeric(starting_points[1,])
i <- 0
while(!is.null(new_point)){
  print(i)
  fit <- lpc(track, h=0.0001, scaled=F, x0 = new_point)
  lat=fit$LPC[,1]
  lon=fit$LPC[,2]
  lines(lon, lat, col='red', lwd = 3)
  
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

points(starting_points$lon[id_rm], starting_points$lat[id_rm], col='red',  type='p' , pch=19, cex=0.5)
