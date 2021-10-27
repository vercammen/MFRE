# replace code below with updated code for the solution to Assignment Q1

rm(list = ls())
library(dplyr)

setwd("C:/Users/Jim/Dropbox/Teach/2021-2022/FRE 501/Price Dynamics/Convenience Yield/Class Exercise/stock-class-exercise")

# read in price and stocks data
price_data <- read.csv(file = "./data/price.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)
stock_data <- read.csv(file = "./data/stocks.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)

# add a "stckDum" indicator variable: 1 if stocks > 108
stock_data <- stock_data %>% 
  mutate(stckDum = ifelse(stocks>108,1,0) )
head(stock_data)

# add the "stckDum" variable (annual data) to the monthly price data
join_data <- left_join(x = price_data, y = stock_data[,c("year","stckDum")], by = "year")
head(join_data)

# add quarterly dummies to the previous data frame
join_data <- join_data %>% 
  mutate(d1 = ifelse(quarter==1,1,0), d2 = ifelse(quarter==2,1,0), d3 = ifelse(quarter==3,1,0) )
head(join_data)

# create interaction variables
join_data <- join_data %>% 
  mutate(int0 = stckDum, int1 = d1*stckDum, int2 = d2*stckDum, int3 = d3*stckDum )
head(join_data)

# take the first difference of the working data frame
lag_data <- join_data %>%
  mutate(price_diff = price - lag(price), 
         d1Diff = d1 - lag(d1), 
         d2Diff = d2 - lag(d2), 
         d3Diff = d3 - lag(d3),
         int0diff = int0 - lag(int0),
         int1diff = int1 - lag(int1),
         int2diff = int2 - lag(int2),
         int3diff = int3 - lag(int3)) %>%
  slice(-1) #drops first row 

head(lag_data)

# regress price differences on the dummies and interaction terms
regP_diff <- lm(price_diff ~ d1Diff + d2Diff + d3Diff + int0diff + int1diff + int2diff + int3diff + 0, data = lag_data)
summary(regP_diff)
