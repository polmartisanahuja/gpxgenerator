library(rvest)
library("RJSONIO")

track <- read_html("http://es.wikiloc.com/wikiloc/view.do?id=2944224")

a <- track %>% 
  html_nodes("script")
a <- a[3]
a <-html_text(a)
a <- gsub("\n\t\t\t\t\t\tvar trinfo =", "",  a)
a <- gsub(";\n\t\t\t\t\t\t", "", a)
a <- fromJSON(a)

ele <- a$at$n
lon <- a$at$l
lat <- a$at$a

track <- data.frame(lat = lat, lon = lon, ele = ele)

plot3d(track$lon, track$lat, track$ele, type='p',  col='blue')