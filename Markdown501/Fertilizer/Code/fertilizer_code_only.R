# create a map of fertilizer prices in Africa, broken down into the northwest and southeast region

# https://stackoverflow.com/questions/27328372/r-create-a-spatial-bubble-plot-that-overlays-a-basemap-of-the-us-and-other-spati

rm(list = ls())
graphics.off() # close all plots

# load packages
pacman::p_load(here, ggplot2, mapdata, dplyr)

# Create two lists for the northwest and southeast set of countries.

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

# Use the map_data() function to create a pair of mapping data frames.

southMap <- map_data("world", region = south)
northMap <- map_data("world", region = north)

# compute the centroid of each country as the mean longitude and latitude.
# create country labels for the maps by extracting names from the data frames.

southLabel <- southMap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

northLabel <- northMap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# Display the maps without the price data
south_plot <- ggplot() +
  geom_path(data = southMap, aes(x = long, y = lat, group = group)) +
  geom_text(aes(x = long, y = lat, label = region), data = southLabel,  size = 3, hjust = 0.5)

north_plot <- ggplot() +
  geom_path(data = northMap, aes(x = long, y = lat, group = group)) +
  geom_text(aes(x = long, y = lat, label = region), data = northLabel,  size = 3, hjust = 0.5)

south_plot
north_plot

# Read in the price data from the csv file
myData <- read.csv(here("Data", "fertilizer_price.csv"))

# Sort the price data into the south and north country categories
southData <- myData[(myData$south == 1), ]
northData <- myData[(myData$south == 0), ]
 
# Plot the maps but this time with the price data included
south_plot2 <- ggplot() +
  geom_path(data = southMap, aes(x = long, y = lat, group = group)) +
  geom_point(data = southData, aes(x = longitude, y = latitude, size = ppp_price_kg), color = "red") +
  geom_text(aes(x = long, y = lat, label = region), data = southLabel,  size = 3, hjust = 0.5)

north_plot2 <- ggplot() +
  geom_path(data = northMap, aes(x = long, y = lat, group = group)) +
  geom_point(data = northData, aes(x = longitude, y = latitude, size = ppp_price_kg), color = "red") +
  geom_text(aes(x = long, y = lat, label = region), data = northLabel,  size = 3, hjust = 0.5)
 
south_plot2
north_plot2

