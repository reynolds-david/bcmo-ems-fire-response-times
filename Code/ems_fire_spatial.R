#EMS and fire response times spatial data analysis
#David Reynolds

#Set working directory
setwd("~/Downloads/Documents/Mizzou/2020-21/EMS response times")

#Import the data
library(tidyverse)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccfnnnncnnnn")

#Create dataframe with first 100 calls from 2019
ems_fire_19 <- ems_fire %>% 
  filter(year == 2019)
ems_fire_19 <- head(ems_fire_19, 500)

#Map the data
library(leaflet)
pal <- colorNumeric(palette = "Reds", domain = c(1:50), reverse = TRUE)
m <- ems_fire_19 %>% 
  leaflet() %>%
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = 4, label = ~call_arrive, color = ~pal(call_arrive))
  addLegend(title = "Call-arrive time", pal = pal, values = c(1, 50), position = "bottomright")
m

#Map the data 2
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addCircleMarkers(data = ems_fire, radius = 4, opacity=1, fill = "darkblue",stroke=TRUE,
                   fillOpacity = 0.75, weight=2, fillColor = "yellow",
                   popup = paste0("Enroute-arrive time: ", ems_fire$Enroute_Arrive,
                                  "<br> Address: ", ems_fire$Street,
                                  "<br> Nature of call: ", ems_fire$Nature)) %>%
  addLayersControl(
    baseGroups = c("Topo","ESRI Aerial"),
    options = layersControlOptions(collapsed = T))
