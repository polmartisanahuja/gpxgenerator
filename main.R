library(rgl)
source("functions.R")

######### Main ########
path <- "tracks/"
file_list = c("trencacames-per-st-llorenc-i-lobac.gpx",
              "hospital-de-sang-serra-de-lobac.gpx",
              "6a-caminada-popular-cims-dels-tres-turons.gpx",
              "la-mola-per-morral-del-drac-i-baixada-per-la-canal-del-mico.gpx")
i=4

track <- read_gpx(paste0(path, file_list[i]))
track <- smooth_gpx(track)
track <- track[(!is.na(track$lat)),]

#plot3d(track$lon, track$lat, track$ele, type='p',  col='blue')
#lines3d(track$lon, track$lat, track$ele,  col='yellow')
#points3d(track$lon, track$lat, track$ele,  col='yellow')

rownames(track) <- 1:nrow(track)

track_list <- split_gpx(track)
track_list <- remove_short(track_list)

plot_gpx(track_list, track)