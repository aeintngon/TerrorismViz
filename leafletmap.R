library(leaflet)

library(rgdal)


setwd("C:/Users/Aeint Thet Ngon/Documents/Georgetown University/Data Viz/Takehome")
terrorism=read.csv("TerrorismData.csv")
us_terrorism <- subset(terrorism, country==217)

us_terrorism <- us_terrorism %>% dplyr::group_by(city, provstate) %>% dplyr::mutate(total=n(), totalwound=sum(nwound), totalkill=sum(nkill)) %>%
  select(city, provstate, latitude, longitude, total, totalwound, totalkill)

terror <- terrorism %>% dplyr::group_by(city, provstate) %>% dplyr::mutate(total=n(), totalwound=sum(nwound), totalkill=sum(nkill)) %>%
  dplyr::select(city, provstate, country_txt,latitude, longitude, total, totalwound, totalkill) 
terror <- terror %>% na.omit()
terror[is.na(terror)] <- 0
popup_LU <- paste0("<strong>Location: </strong>", 
                   terror$country_txt,
                   " ", 
                   terror$city,
                   "<br><strong>Count: </strong>", 
                   terror$total,
                   "<br><strong>Total wounded: </strong>", 
                   terror$totalwound,
                   "<br><strong>Total killed: </strong>", 
                   terror$totalkill)

tmap <- leaflet(terror) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(), popup=popup_LU
)
tmap
library(htmlwidgets)
saveWidget(tmap, 'terrorism_map.html', selfcontained = TRUE)
