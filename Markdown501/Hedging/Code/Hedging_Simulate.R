rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2)
data <- read.csv(here("Data", "corn_ethanol_data.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 

data$Week <- as.Date(data$Week, format = c("%m/%d/%Y"))

#data$Date <- as.Date(data$Date, origin = "1899-12-30") # https://stackoverflow.com/questions/47093228/convert-excel-numeric-to-date
#data$Week <- as.Date(data$Week, format = c("%m/%d/%y")) I CAN'T GET THIS METHOD TO WORK!
head(data)
tail(data)

# There are six years and five contracts per year for a total of 30 contracts (2 - 3 months per contract).
# index: n = 30
# We want to randomly sample three weeks from each of the 30 data sub divisions.
# We order the data from earliest (date 1) to middle (date 2) to latest (date 3)
# We choose various combinations of prices from the three dates to calculate hedging profits.
# Doing this aggregates each set of three rows into a single row.
# After looping through the date once (the i loop) we have 30 rows with various measures of hedging profits.
# We repeat the process 300 times to ensure the law of large numbers is working ==> 3000 random measures of hedging profits.
# All 3000 rows are aggregated into a list (this was recommended in a forum).
# The list is then saved as an rds file, an used in the "hedging_profits.R" program.

# setting the seed will ensure we can reproduce the random result
set.seed(2021) 

compare_repeat = list() # create the list which will hold the results to be saved

for (j in 1:300) {
  
  for (i in 1:30) {
    sample <- data[sample(which(data$Index == i ), 3),] # random select three rows for each of 30 index values
    sample <- sample[order(sample$Week),] # order from oldest to newest
    sample <- as.data.frame(sample)
    # construct single row which combines dates and prices from three rows.
    # DATES APPEAR AS SERIAL NUMBERS - I CAN'T GET DATE FORMAT TO WORK PROPERLY.
    Date1 <- sample$Week[1]  
    Date2 <- sample$Week[2]
    Date3 <- sample$Week[3]
    DDG1 <- sample$DDG[1] # choose this and next four prices from first row
    Corn1 <- sample$CornSpot[1]
    Ethanol1 <- sample$Ethanol[1]
    FutCorn1 <- sample$CornFut[1]
    FutEthan1 <- sample$EthanFut[1]
    Corn2 <- sample$CornSpot[2] # choose this and next two prices from second row
    FutCorn2 <- sample$CornFut[2]
    FutEthan2 <- sample$EthanFut[2]
    Corn3 <- sample$CornSpot[3] # choose this and next three prices from third row
    FutCorn3 <- sample$CornFut[3]
    Ethanol3 <- sample$Ethanol[3]
    FutEthan3 <- sample$EthanFut[3]
    # bind previous variables into a single row
    # USE DATA.FRAME() INSTEAD OF CBIND() - date formats are preserved 
    # cbind() forces the variables to have the same data class, that's why the Dates get converted
    sample3 <- data.frame(Date1, Date2, Date3, DDG1,Corn1,Ethanol1,FutCorn1,FutEthan1,Corn2,FutCorn2,Corn3,FutCorn3,Ethanol3,FutEthan3)
    
    # Construct the specific measures of hedging profits
    # Long in corn with no hedge: long_corn <- Buy corn[1] and sell corn[3]
    # Short in corn with no hedge: short_corn <- Sell corn[1] and buy corn[3]
    # Crush margin: crushNH <- Sell DDG[1], buy corn[2] and sell ethanol[3]
    # Short hedge: short_hedge <- Buy corn[1], short corn futures[1], sell corn[3] and long corn futures[3]
    # Long hedge: long_hedge <- Sell corn[1], long corn futures[1], buy corn[3] and short corn futures[3]
    # Crush hedge: crushH <- Sell DDG[1], long corn futures[1], short ethanol futures[1], buy corn[2], sell corn futures[2], sell ethanol[3] and long ethanol futures[3]
    
    compare <- sample3 %>% mutate(
      long_corn = -Corn1 + Corn3,
      short_corn = Corn1 - Corn3,
      crush1 = 0.0085*DDG1+2.8*Ethanol1-Corn1,
      crushNH = 0.0085*DDG1+2.8*Ethanol3-Corn2 - crush1,
      short_hedge = -Corn1 + Corn3 + FutCorn1/100 - FutCorn3/100,
      long_hedge = Corn1 - Corn3 - FutCorn1/100 + FutCorn3/100,
      crushH =  0.0085*DDG1+2.8*Ethanol3-Corn2 + 2.8*FutEthan1 - FutCorn1/100 + FutCorn2/100 - 2.8*FutEthan3 - crush1,
      cornBasis1 = Corn1 - FutCorn1/100,
      cornBasis2 = Corn2 - FutCorn2/100,
      cornBasis3 = Corn3 - FutCorn3/100,
      ethanolBasis1 = Ethanol1 - FutEthan1,
      ethanolBasis3 = Ethanol3 - FutEthan3,
      BasisChange = cornBasis3 - cornBasis1) %>%
      select(Date1, Date2, Date3, long_corn,short_corn,crushNH,short_hedge,long_hedge,crushH,cornBasis1,cornBasis2,cornBasis3,ethanolBasis1,ethanolBasis3, BasisChange)
    
    compare_repeat[[(j-1)*35+i]] <- compare  
  }
}

compare_full <- dplyr::bind_rows(compare_repeat) # bind together into a large list and save.

# to export
saveRDS(compare_full, file = here("Data", "compare.rds"))


