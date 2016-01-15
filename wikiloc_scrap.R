library(rvest)
library("RJSONIO")
library("TTR")
library(XML)
library(rgl)
library(MASS)
library(gplots)
library(zoom)

remove("track_base")

for (i in seq(10,200,10)){
  print(i)
  
  wikiloc <- read_html(paste0("http://es.wikiloc.com/wikiloc/find.do?act=1%2C&q=collserola&from=", i-10,"&to=",i))
  #wikiloc <- read_html(paste0("http://es.wikiloc.com/wikiloc/find.do?act=1%2C&q=sant+lloren%C3%A7+del+munt&from", i-10,"&to=",i))
  
  track_link <- wikiloc %>% 
       html_nodes("h3") %>%
       html_nodes("a") %>% html_attr("href") 
  
  for (j in 1:10){
    print(j)
    
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
    
    #print(length(lon))
    if( (max(lon)<2.25) & (min(lon)>2) & (max(lat)<41.5) & (min(lat)>41.35) & (max(ele)<700) & (min(ele)>100)){
      if(!exists("track_base")){track_base <- data.frame(lat = lat, lon = lon, ele = ele)
      }else{track_base <- rbind(track_base, data.frame(lat = lat, lon = lon, ele = ele))}    
    }
  
  }
}

#plot(track_base$lon, track_base$lat, type='p',  col='blue', pch='.')
#plot3d(track_base$lon, track_base$lat, track_base$ele, type='p',  col='blue')
#h2 <- hist2d(data.frame(x = track_base$lon,  y = track_base$lat), nbins=2000)

#zm()
plot(track_base$lon, track_base$lat, type='p',  col='blue', pch='.')
#plot3d(track_base$lon, track_base$lat, track_base$ele, type='p',  col='blue', pch='.', ylim=c(41.35,41.5), xlim=c(2,2.25))
#plot3d(track_base$lon, track_base$lat, 0, type='p',  col='blue', pch='.')

track_base <- track_base[(!is.na(track_base$lat)),]
track_base <- track_base[(!is.na(track_base$lon)),]

track_base_2 <- nearest_gpx(track_base)

