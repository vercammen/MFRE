# if error occurs concerning "rland" use install.packages("rlang")
rm(list=ls())
graphics.off() # close all plots

setwd("C:/Users/Jim/Dropbox/Git/Markdown501/Bananas")

library(readxl)
library(countrycode)

# read in FAO production, export and import data, rename value column
FAO_Q <-read_excel("./Data/Bananas.xlsx", sheet = "FAOSTAT_Production")
FAO_Q <-FAO_Q[c(4,6,12)]
names(FAO_Q)[names(FAO_Q) == "Value"] <- "production"

FAO_X <-read_excel("./Data/Bananas.xlsx", sheet = "FAOSTAT_Exports")
FAO_X <-FAO_X[c(4,6,12)]
names(FAO_X)[names(FAO_X) == "Value"] <- "exports"

FAO_M <-read_excel("./Data/Bananas.xlsx", sheet = "FAOSTAT_Imports")
FAO_M <-FAO_M[c(4,6,12)]
names(FAO_M)[names(FAO_M) == "Value"] <- "imports"

# merge dataframes with a full join
merge1 <- merge(x = FAO_Q, y = FAO_X, by = "Area", all = TRUE)
merge2 <- merge(x = merge1, y = FAO_M, by = "Area", all = TRUE)
merge2 <- merge2[-c(2,4,6)]

# rename Area column to Country and then generate a country code
# eliminate countries which do not have a iso3c code (i.e., Kosovo)
names(merge2)[names(merge2) == "Area"] <- "Country"
merge2[,"Country"]=countrycode(merge2$Country,"country.name", "iso3c")


# import P_real dataframe, eliminate rows with non iso3c code, merge with merge2 
P_real <- readRDS("./Convert/P_real.RDS")
trade_all <- merge(merge2, P_real[complete.cases(P_real), ], by="Country")

# convert all blank cells to 0
trade_all[is.na(trade_all)] = 0

# subset data according to imports > 50,000, exports < 500 and production < 500; eliminate unused columns
importer_pure <- trade_all[(trade_all$imports > 50000) & (trade_all$exports < 500) & (trade_all$production < 500), ]
importer_pure <- importer_pure[-c(6,7,8)]

# subset data according to exports > 50,000 and imports < 500; eliminate unused columns
exporter_pure <- trade_all[(trade_all$exports > 50000) & (trade_all$imports < 500), ]
exporter_pure <- exporter_pure[-c(6,7,8)]

# calculate average price for pure importers and pure exporters
mean(importer_pure$real_price)
mean(exporter_pure$real_price)

# import trade data and add a trade_id column
#bilateral <- read.csv(file="./Trade/Bilateral_trade.xlsx", header=TRUE, sep=",")
bilateral <-read_excel("./Data/Bilateral_trade.xlsx", sheet = "bilateral")
bilateral$trade_id <- seq.int(nrow(bilateral))

# merge bilateral with the P_real dataframe using exporter from bilateral and Country from p_real
# rename real_price to exp_price; eliminate unused columns
exp_price <- merge(bilateral, P_real, by.x="exporter", by.y="Country")
names(exp_price)[names(exp_price) == 'real_price'] <- 'exp_price'
exp_price <- exp_price[-c(2,3,8,9,10,11)]

# repeat for the importer

# merge bilateral with the P_real dataframe using exporter from bilateral and Country from p_real
# rename real_price to exp_price; eliminate unused columns
imp_price <- merge(bilateral, P_real, by.x="importer", by.y="Country")
names(imp_price)[names(imp_price) == 'real_price'] <- 'imp_price'
imp_price <- imp_price[-c(3,4,8,9,10,11)]

#merge the exp_price and imp_price data sets together using the trade_id variable
bilateral_final <- merge(exp_price[-c(3,4)], imp_price[-c(1)], by = "trade_id")
#bilateral_final <- merge(exp_price, imp_price, by = "trade_id")

# eliminate duplicate rows
#distinct(bilateral_final)

# calculate import price minus export price, and show summary statistics
bilateral_final$price_gap <- bilateral_final$imp_price - bilateral_final$exp_price

# calculate average price for pure importers and pure exporters
mean(bilateral_final$exp_price)
mean(bilateral_final$imp_price)

# calculate the average export price for each importer, then rename the aggregate column
exp_price_avg <- aggregate(bilateral_final$exp_price, by=list(imp_name=bilateral_final$imp_name), FUN=mean)
names(exp_price_avg)[names(exp_price_avg) == 'x'] <- 'mean_exp_price'

# identify the importer's unique price by calculating the average import price 
imp_price_avg <- aggregate(bilateral_final$imp_price, by=list(imp_name=bilateral_final$imp_name), FUN=mean)
names(imp_price_avg)[names(imp_price_avg) == 'x'] <- 'mean_imp_price'

# merge the average export and import price dataframes and calculate the price gap
avg_price <- merge(exp_price_avg, imp_price_avg, by="imp_name")
avg_price$gap <- avg_price$mean_imp_price - avg_price$mean_exp_price

# calculate the average price gap
mean(avg_price$gap)

