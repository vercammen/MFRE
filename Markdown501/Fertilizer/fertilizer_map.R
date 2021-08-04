# create a map of fertilizer prices in Africa, broken down into the North and South region


# https://stackoverflow.com/questions/27328372/r-create-a-spatial-bubble-plot-that-overlays-a-basemap-of-the-us-and-other-spati

rm(list = ls())
graphics.off() # close all plots

setwd("C:/Users/Jim/Dropbox/Git/Markdown501/Fertilizer")

library(ggplot2)
#library(ggmap)
#library(maps)
library(mapdata)
library(dplyr)


south <- c(
  "Tanzania", "Burundi",  
  "Kenya", "Malawi", "Mozambique",
  "Rwanda", 
  "Uganda"
)

north <- c(
  "Burkina Faso", "Ivory Coast", "Ghana",
  "Mali",
  "Nigeria", "Senegal"
)

southMap <- map_data("world", region = south)
northMap <- map_data("world", region = north)

# Compute the centroid as the mean longitude and latitude
# Used as label coordinate for country's names
southLabel <- southMap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

northLabel <- northMap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


myData <- read.csv(file = "./Data/fertilizer_price.csv")

southData <- myData[(myData$south == 1), ]
northData <- myData[(myData$south == 0), ]
 
south_plot <- ggplot() +
  geom_path(data = southMap, aes(x = long, y = lat, group = group)) +
  geom_point(data = southData, aes(x = longitude, y = latitude, size = ppp_price_kg), color = "red") +
  geom_text(aes(x = long, y = lat, label = region), data = southLabel,  size = 3, hjust = 0.5)

north_plot <- ggplot() +
  geom_path(data = northMap, aes(x = long, y = lat, group = group)) +
  geom_point(data = northData, aes(x = longitude, y = latitude, size = ppp_price_kg), color = "red") +
  geom_text(aes(x = long, y = lat, label = region), data = northLabel,  size = 3, hjust = 0.5)
 
south_plot
north_plot
