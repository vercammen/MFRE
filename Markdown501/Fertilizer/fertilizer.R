# this program creates a map of fertilizer prices in Africa

# https://stackoverflow.com/questions/27328372/r-create-a-spatial-bubble-plot-that-overlays-a-basemap-of-the-us-and-other-spati

rm(list = ls()) # clear memory
graphics.off() # close all plots

setwd("C:/Users/Jim/Dropbox/Git/Markdown501/Fertilizer")

library(ggplot2)
library(mapdata)
library(dplyr)

# read in data
Data <- read.csv(file = "./Data/fertilizer_data.csv")

# select year (2011, 2014 or 2017) and region (latitude < 4.75 or latitude > 4.75)
myData <- Data[(Data$year == 2017) & (Data$latitude > 4.75), ]

# select unique set of country names
countries <- unique(myData$country)

# draw map of selected countries
myMap <- map_data("world", region = countries)
 
# Compute the centroid as the mean longitude and latitude
# Used as label coordinate for country's names
mapLabel <- myMap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# plot the data 
myPlot <- ggplot() +
  geom_path(data = myMap, aes(x = long, y = lat, group = group)) +
  geom_point(data = myData, aes(x = longitude, y = latitude, size = ppp_price_kg), color = "red") +
  geom_text(aes(x = long, y = lat, label = region), data = mapLabel,  size = 3, hjust = 0.5) +
  ggtitle( "2017")
myPlot



 
