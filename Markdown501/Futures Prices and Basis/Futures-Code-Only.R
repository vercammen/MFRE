rm(list = ls()) 
graphics.off()

# load packages
pacman::p_load(here, dplyr, ggplot2)

# load the R program which contains the "get_simulated()" function
source(here("price-reformat.R"))

# identify the row to be selected
row_select <-3

# call the "get_simulated()" function with a specified row number for data retrieval
prices_all <- get_simulated(row_select)
prices_all

# transpose the spot prices from a row to a vector
spot <- matrix(priceSpot[row_select,],8,1)

