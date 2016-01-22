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

track <- read.csv(paste0(path, scrap_file))
track <- track[(track$lon<2.2) & (track$lon>2) & (track$lat<41.5) & (track$lat>41.35),]

plot(track$lon, track$lat, type='p',  col='blue', pch='.')
zm()

h <- hist2d(track$lon, track$lat, nbins=3000)
image(h$x.breaks, h$y.breaks, log10(h$counts))
zm()

x=track$lon
y=track$lat

z <- kde2d(x,y, h=c(0.0001,0.0001), n=500)



track_small <- track[(track$lon<2.15) & (track$lon>2.11) & (track$lat<41.43) & (track$lat>41.425),]
plot(track_small$lon, track_small$lat, type='p',  col='blue', pch='.')

track_small$id <- NULL


x=track_small$lon
y=track_small$lat
z <- kde2d(x,y, h=c(0.0001,0.0001), n=100)
image(z)
filled.contour(z, color = terrain.colors, nlevels = 100)

track_matrix <- as.matrix(track_small)
fit <- principal.curve(track_matrix)

fit <- lpc(track_small, h=0.0001, x0=1, depth=1, way="one",  scaled=F, control=lpc.control(iter=100))
plot(fit, curvecol = 1)


data(calspeedflow)
lpc1 <- lpc(calspeedflow[,3:4])
lines(fit)

x <- runif(100,-1,1)
x <- cbind(x, x ^ 2 + rnorm(100, sd = 0.1))
fit1 <- principal.curve(x, plot = TRUE)
fit2 <- principal.curve(x, plot = TRUE, smoother = "lowess")
lines(fit1)
points(fit1)
plot(fit1)
whiskers <- function(from, to)
  segments(from[, 1], from[, 2], to[, 1], to[, 2])
whiskers(x, fit1$s)

############## KDE for the whole  map ###############
lon_bins <- seq(2,2.2,(2.2-2)/3)
lat_bins <- seq(41.35,41.5,(41.5-41.35)/3)

for(i in 1:3){
  for(j in 1:3){
    print(i)
    print(j)
    track_small <- track[(track$lon<lon_bins[i+1]) & (track$lon>lon_bins[i]) & (track$lat<lat_bins[j+1]) & (track$lat>lat_bins[j]),]
    
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