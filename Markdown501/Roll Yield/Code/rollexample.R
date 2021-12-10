rm(list = ls()) 

# This script contains the codes to create the roll example tables

# load packages
pacman::p_load(here, dplyr, ggplot2, lubridate, kableExtra, readxl, janitor, 
               tidyverse, purrr, stringr)

# read the data
data1 <- read.csv(here("Data", "example_data_1.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
data2 <- read.csv(here("Data", "example_data_2.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 

roll1 <- data1                                        # Duplicate vector
roll1[is.na(roll1)] <- ""                       # Replace NA with blank
roll1 

roll2 <- data2                                        # Duplicate vector
roll2[is.na(roll2)] <- ""                       # Replace NA with blank
roll2 