rm(list = ls()) 
graphics.off()

# load packages
pacman::p_load(here, dplyr, ggplot2, gridExtra)

# load the non-stochastic prices for the five scenarios
P_ns <- read.csv(here("Data", "price_scenarios_nonstochastic.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE)

# define demand curve parameters
a <- 16.21
b <- 3.5
stock_begin <- 14.38 + 2.015

# create consumption values for the five price scenarios
X_ns <- P_ns %>% mutate(
    X_base = a/b - 1/b*P_base,
    X_D_high = a/b - 1/b*P_D_high,
    X_D_low = a/b - 1/b*P_D_low,
    X_H5_low = a/b - 1/b*P_H5_low,
    X_H5_high = a/b - 1/b*P_H5_high) %>% select(X_base,X_D_high,X_D_low,X_H5_low,X_H5_high)

# calculate Q4 ending stocks for five price scenarios
S4_base <- stock_begin-X_ns$X_base[1]-X_ns$X_base[2]-X_ns$X_base[3]-X_ns$X_base[4]
S4_D_high <- stock_begin-X_ns$X_D_high[1]-X_ns$X_D_high[2]-X_ns$X_D_high[3]-X_ns$X_D_high[4]
S4_D_low <- stock_begin-X_ns$X_D_low[1]-X_ns$X_D_low[2]-X_ns$X_D_low[3]-X_ns$X_D_low[4]
S4_H5_low <- stock_begin-X_ns$X_H5_low[1]-X_ns$X_H5_low[2]-X_ns$X_H5_low[3]-X_ns$X_H5_low[4]
S4_H5_high <- stock_begin-X_ns$X_H5_high[1]-X_ns$X_H5_high[2]-X_ns$X_H5_high[3]-X_ns$X_H5_high[4]

# bind the stocks together in a row vector  
s4 <- cbind(S4_base,S4_D_high, S4_D_low, S4_H5_low, S4_H5_high)

# load the R program which contains the "get_simulated()" function
source(here("Code","price-reformat.R"))

# read in the forecasted values for H_5 and D.
demand <- read.csv(here("Data", "demand_forecast.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE)
harvest <- read.csv(here("Data", "harvest_forecast.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE)

# identify the row to be selected
row_select <-3

# slice out the desired row from the two forecast data frames and the "priceSpot" data frame (opened with "price-reformat.R")
H5_frcst <- harvest[row_select,]
D_frcst <- demand[row_select,]
# spot <- priceSpot[row_select,]

# call the "get_simulated()" function with a specified row number for retrieving simulated prices.
prices <- get_simulated(row_select)
print (prices, digits = 4)

# collect diagonal elements and make into a new data frame called "spot_df"  
 
spot_df = as.data.frame(diag(as.matrix(prices)))
colnames(spot_df)<- "spot"
#  prices <- cbind(prices,spot_df)
prices

# Plot the first row of the pricing matrix
forward <- as.data.frame(prices[1,])
colnames(forward)<- "ForwardQ1"
labels <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")
forward <- cbind(labels,forward)

forward_plot <-ggplot(data=forward, aes(x=labels, y=ForwardQ1)) +
  geom_bar(stat="identity") + ggtitle("Q1 Forward Curve") +
  labs(x= "Expiry Date of Futures Contract") + labs(y= "$/bushel") +
  coord_cartesian(ylim=c(3, 4))
#forward_plot

# plot the Q2 forward curve for row 8 in the simulated prices
row_select2 <-8
prices2 <- get_simulated(row_select2)

forward2 <- as.data.frame(prices2[2,-1])

colnames(forward2)<- "ForwardQ2"
labels2 <- c("Q2","Q3","Q4","Q5","Q6","Q7","Q8")
forward2 <- cbind(labels2,forward2)

forward_plot2 <-ggplot(data=forward2, aes(x=labels2, y=ForwardQ2)) +
  geom_bar(stat="identity") + ggtitle("Q2 Forward Curve") +
  labs(x= "Expiry Date of Futures Contract") + labs(y= "$/bushel") +
  coord_cartesian(ylim=c(1, 3))
#forward_plot2

# using the 5000 values, we want to calculate the Q3 to Q7 price spread for the second quarter.
# we need to bind together the second columns of price3 and price7
# then add the two forecast columns

futures_2_3 <- price3 %>% select(P_2_3)
futures_2_7 <- price7 %>% select(P_2_7)
futures_2_3_7 <- cbind(futures_2_3,futures_2_7,harvest[,2],demand[,2])
colnames(futures_2_3_7)[3:4] <- c("H5_frcst","D_frcst")

futures_2_3_7 <- futures_2_3_7 %>% 
  mutate(Sprd_3_7 = P_2_7 - P_2_3)

head(futures_2_3_7)

# calculate the mean and standard deviation of the five columns
mean_2_3_7 <- futures_2_3_7 %>% summarise_if(is.numeric, mean)
sd_2_3_7 <- futures_2_3_7 %>% summarise_if(is.numeric, sd)
mean_2_3_7
sd_2_3_7

# calculate Q4 carry over stocks (S0 + H1 - x1 - x2 - x3 - x4).

stocks_4 <- 14.377 + 2.015 - (4*a/b - (1/b)*(priceSpot$spot1 + priceSpot$spot2 + priceSpot$spot3 + priceSpot$spot4))   

# Add this to the futures_2_3_7 data frame
futures_2_3_7 <- cbind(futures_2_3_7,stocks_4)
head(futures_2_3_7)

# regress the spread on the two forecast variables
reg_sprd <- lm(Sprd_3_7 ~ D_frcst + H5_frcst, data = futures_2_3_7)
summary(reg_sprd)

# regress the Q4 stocks variable on the two forecast variables and save the fitted values
reg_sprd2 <- lm(stocks_4 ~ D_frcst + H5_frcst, data = futures_2_3_7)
summary(reg_sprd2)
stocks_fit2 <- reg_sprd2$fitted.values

# regress the price spread on the fitted stocks variable and H5
reg_sprd3 <- lm(Sprd_3_7 ~ H5_frcst + stocks_fit2 , data = futures_2_3_7)
summary(reg_sprd3)

# regress the price spread on the H5 variable and the  stocks variable
reg_sprd4 <- lm(Sprd_3_7 ~ H5_frcst + stocks_4, data = futures_2_3_7)
summary(reg_sprd4)

# create a data frame consisting of the Q7 contract basis, starting in Q4
# use the first row only, assumes non-stochastic data
basis7_ns <- c() 
basis7_ns[1] <- prices[1,c("pQ1")]-prices[1,c("pQ7")]
basis7_ns[2] <- prices[1,c("pQ2")]-prices[1,c("pQ7")]
basis7_ns[3] <- prices[1,c("pQ3")]-prices[1,c("pQ7")]
basis7_ns[4] <- prices[1,c("pQ4")]-prices[1,c("pQ7")]
basis7_ns[5] <- prices[1,c("pQ5")]-prices[1,c("pQ7")]
basis7_ns[6] <- prices[1,c("pQ6")]-prices[1,c("pQ7")]
basis7_ns[7] <- prices[1,c("pQ7")]-prices[1,c("pQ7")]
basis7_ns <- as.data.frame(basis7_ns)

# plot the basis values besides the non-stochastic prices

forward <- as.data.frame(prices[1,])
colnames(forward)<- "ForwardQ1"
labels <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")
forward <- cbind(labels,forward)

basis7_ns <- as.data.frame(basis7_ns)
colnames(basis7_ns)<- "BasisQ1"
labels7 <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7")
basis <- cbind(labels7,basis7_ns)

forward_plot <-ggplot(data=forward, aes(x=labels, y=ForwardQ1)) +
  geom_bar(stat="identity") + ggtitle("Time Path of Spot Prices") +
  labs(x= "Expiry Date of Futures Contract") + labs(y= "$/bushel") +
  coord_cartesian(ylim=c(3, 4))

basis_plot <-ggplot(data=basis, aes(x=labels7, y=BasisQ1)) +
  geom_bar(stat="identity") + ggtitle("Time Path of Basis for Q7 Contract") +
  labs(x= "Expiry Date of Futures Contract") + labs(y= "$/bushel")

grid.arrange(forward_plot, basis_plot, ncol = 2) 


# repeat the above procedure but use the stochastic version of the prices data frame
basis7 <- c() 
basis7[1] <- prices[1,c("pQ1")]-prices[1,c("pQ7")]
basis7[2] <- prices[2,c("pQ2")]-prices[2,c("pQ7")]
basis7[3] <- prices[3,c("pQ3")]-prices[3,c("pQ7")]
basis7[4] <- prices[4,c("pQ4")]-prices[4,c("pQ7")]
basis7[5] <- prices[5,c("pQ5")]-prices[5,c("pQ7")]
basis7[6] <- prices[6,c("pQ6")]-prices[6,c("pQ7")]
basis7[7] <- prices[7,c("pQ7")]-prices[7,c("pQ7")]

# display basis7_ns and basis7
print(basis7_ns, digits = 4)
print(basis7, digits = 4)

# calculate the quarterly change in the basis
basis7_ns_ch <- basis7_ns - lag(basis7_ns)
basis7_ch <- basis7 - lag(basis7)

print(basis7_ns_ch, digits = 4)
print(basis7_ch, digits = 4)

# using the 5000 prices, create basis for Q7 contract in Q4 and Q6
b7Q4 <- price4$P_4_4 - price7$P_4_7
b7Q6 <- price6$P_6_6 - price7$P_6_7

# create the change in the Q7 basis from Q4 to Q6
b7_ch <- b7Q6 - b7Q4

# bind everything together
b7 <- cbind(b7Q4,b7Q6,b7_ch)
b7 <- as.data.frame(b7)
head(b7)

# calculate the mean and standard deviation of the three columns
mean_b7 <- b7 %>% summarise_if(is.numeric, mean)
sd_b7 <- b7 %>% summarise_if(is.numeric, sd)
mean_b7
sd_b7

# import the Iowa cashbid data, delete two columns and display
cashbids <- read.csv(here("Data", "cashbids_iowa.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
cashbids <- subset(cashbids, select = -c(Location, Notes))
head(cashbids, 6)



# THE CODE BELOW IS NOT USED

# add the forecast variables to the pricing vector.
prices <- cbind(as.data.frame(t(H5_frcst)),as.data.frame(t(D_frcst)),prices, as.data.frame(t(spot)))
colnames(prices)[1:2] <- c("H5_frcst","D_frcst")
colnames(prices)[ncol(prices)] <- "Spot"

# calculate the price spreads and place in a new data frame

spd1_2 <- ifelse(prices$pQ1=="NA", 0, prices$pQ2-prices$pQ1)
spd2_3 <- ifelse(prices$pQ2=="NA", 0, prices$pQ3-prices$pQ2)
spd3_4 <- ifelse(prices$pQ3=="NA", 0, prices$pQ4-prices$pQ3)
spd4_5 <- ifelse(prices$pQ4=="NA", 0, prices$pQ5-prices$pQ4)
spd5_6 <- ifelse(prices$pQ5=="NA", 0, prices$pQ6-prices$pQ5)
spd6_7 <- ifelse(prices$pQ6=="NA", 0, prices$pQ7-prices$pQ6)
spd7_8 <- ifelse(prices$pQ7=="NA", 0, prices$pQ8-prices$pQ7)
 
spread <- cbind(spd1_2, spd2_3, spd3_4, spd4_5, spd5_6, spd6_7, spd7_8)

# bind spot prices to the futures prices in the simulated data frames (Q2,Q3, Q4 and Q5) 
spot2 <- priceSpot %>% select(spot1, spot2)
spot3 <- priceSpot %>% select(spot1, spot2, spot3)
spot4 <- priceSpot %>% select(spot1, spot2, spot3, spot4)
spot5 <- priceSpot %>% select(spot1, spot2, spot3, spot4, spot5)

futures_spot_2 <- cbind(price2,spot2, stocks_4)
futures_spot_3 <- cbind(price3,spot3, stocks_4)
futures_spot_4 <- cbind(price4,spot4, stocks_4)
futures_spot_5 <- cbind(price5,spot5, stocks_4)

# calculate the basis for the 5000 simulated prices; then add the carry over stocks.
basis2 <- futures_spot_2 %>% 
  mutate(b2_Q1 = spot1 - P_1_2,
         b2_Q2 = spot2 - P_2_2)  %>%
  select(b2_Q1,b2_Q2, stocks_4)

basis3 <- futures_spot_3 %>% 
  mutate(b3_Q1 = spot1 - P_1_3,
         b3_Q2 = spot2 - P_2_3,
         b3_Q3 = spot3 - P_3_3)  %>%
  select(b3_Q1,b3_Q2,b3_Q3, stocks_4)

basis4 <- futures_spot_4 %>% 
  mutate(b4_Q1 = spot1 - P_1_4,
         b4_Q2 = spot2 - P_2_4,
         b4_Q3 = spot3 - P_3_4,
         b4_Q4 = spot4 - P_4_4)  %>%
  select(b4_Q1,b4_Q2,b4_Q3,b4_Q4, stocks_4)

basis5 <- futures_spot_5 %>% 
  mutate(b5_Q1 = spot1 - P_1_5,
         b5_Q2 = spot2 - P_2_5,
         b5_Q3 = spot3 - P_3_5,
         b5_Q4 = spot4 - P_4_5,
         b5_Q5 = spot5 - P_5_5)  %>%
  select(b5_Q1,b5_Q2,b5_Q3,b5_Q4, b5_Q5, stocks_4)

# Calculate the mean and standard deviation of the four columns in basis4
basis_mean_2 <- basis2 %>% summarise_if(is.numeric, mean)
basis_mean_3 <- basis3 %>% summarise_if(is.numeric, mean)
basis_mean_4 <- basis4 %>% summarise_if(is.numeric, mean)
basis_mean_5 <- basis5 %>% summarise_if(is.numeric, mean)
basis_sd_2 <- basis2 %>% summarise_if(is.numeric, sd)
basis_sd_3 <- basis3 %>% summarise_if(is.numeric, sd)
basis_sd_4 <- basis4 %>% summarise_if(is.numeric, sd)
basis_sd_5 <- basis5 %>% summarise_if(is.numeric, sd)

# bind the results together
basis_mean_all <- bind_rows(basis_mean_2, basis_mean_3, basis_mean_4, basis_mean_5)
basis_mean_all <- basis_mean_all %>% select(b2_Q1,b3_Q1,b3_Q2,b4_Q1,b4_Q2,b4_Q3,b5_Q1,b5_Q2,b5_Q3,b5_Q4)
basis_sd_all <- bind_rows(basis_sd_2, basis_sd_3, basis_sd_4, basis_sd_5)
basis_sd_all <- basis_sd_all %>% select(b2_Q1,b3_Q1,b3_Q2,b4_Q1,b4_Q2,b4_Q3,b5_Q1,b5_Q2,b5_Q3,b5_Q4)
 
# bind together first three quarters of Q3 and Q5 futures prices
futures_5 <- price5 %>% select(P_1_5, P_2_5, P_3_5)
futures_3_5 <- cbind(price3,futures_5)

# calculate Q3 to Q5 spreads for the first three quarters
spread_3_5 <- futures_3_5 %>% 
  mutate(spd_Q1 = P_1_5 - P_1_3,
         spd_Q2 = P_2_5 - P_2_3,
         spd_Q3 = P_3_5 - P_3_3)  %>%
  select(spd_Q1,spd_Q2,spd_Q3)

# calculate the mean spread and the standard deviation of the spread
spread_3_5 %>% summarise_if(is.numeric, mean)
spread_3_5 %>% summarise_if(is.numeric, sd)
 


