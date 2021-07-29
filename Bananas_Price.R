# use <ctrk><shift><c> to comment/uncomment
# Be sure there is no Excel formatting such as commas or it will come in as a string,
# or eliminate comma using N$population <= gsub(",","",N$population) then run N$population <= as.numeric(as.character(N$population))
# recommended to convert blanks to NA when importing data: https://stackoverflow.com/questions/12763890/exclude-blank-and-na-in-r
rm(list=ls())
#setwd("C:/Users/Jim/Dropbox/FRE501")
setwd("C:/Users/vercamme/Dropbox/FRE501")

#options(stringsAsFactors = FALSE)

# Set Convert indicator to TRUE (run conversion code) or FALSE (don't run)
conversion <- FALSE

# code which uses the countrycode() function to convert country codes
if(conversion == TRUE){
  
library(mapdata)
library(ggplot2)
library(countrycode)
library(dplyr)
  
# load the world map data, rename key variable and convert to standard country names
w=map_data(map="world")
names(w)[which(names(w)=="region")]="Country" # rename "region" to "Country"
w[,"Country"]=countrycode(w$Country,"country.name", "country.name")
saveRDS(w, file = "./Convert/w.RDS")
  
# convert country name to 3 character iso code
GI <- read.csv(file="./Convert/Groceries Index.csv", header=TRUE, sep=",")
GI[,"Country"]=countrycode(GI$Country,"country.name", "iso3c")
saveRDS(GI, file = "./Convert/GI.RDS")
P <- read.csv(file="./Convert/Banana Price.csv", header=TRUE, sep=",")
P[,"Country"]=countrycode(P$Country,"country.name", "iso3c")
saveRDS(P, file = "./Convert/P.RDS")
Q <- read.csv(file="./Convert/Banana Production.csv", header=TRUE, sep=",")
Q[,"Country"]=countrycode(Q$Country,"country.name", "iso3c")
saveRDS(Q, file = "./Convert/Q.RDS")
N <- read.csv(file="./Convert/Population.csv", header=TRUE, sep=",")
N[,"Country"]=countrycode(N$Country,"country.name", "iso3c")
saveRDS(N, file = "./Convert/N.RDS")
# convert country name in data to standarized name used by R (required for graphing)
Qm <- read.csv(file="./Convert/Banana Production.csv", header=TRUE, sep=",")
Qm[,"Country"]=countrycode(Qm$Country,"country.name", "country.name")
saveRDS(Qm, file = "./Convert/Qm.RDS")
Pm <- read.csv(file="./Convert/Banana Price.csv", header=TRUE, sep=",")
Pm[,"Country"]=countrycode(Pm$Country,"country.name", "country.name")
saveRDS(Pm, file = "./Convert/Pm.RDS")
}
# end of code which creates map data and converts country codes

# Begin code here when not running the above conversion script

# PART A: Plot world map of banana production and price

# clear environment and load packages
rm(list=ls())
library(mapdata)
library(ggplot2)
library(dplyr)

# load world map and banana data with iso code conversions
rm(list=ls())
#load("./Convert/Bananas.RData")
Qm <- readRDS("./Convert/Qm.RDS")
Pm <- readRDS("./Convert/Pm.RDS")
w <- readRDS("./Convert/w.RDS")
 
#merge world map data with production and price data
worldQ =full_join(w,Qm)
worldP =full_join(w,Pm)

# removes Antarctica from plot
worldQ=worldQ[which(worldQ$Country!="Antarctica"),]
worldP=worldP[which(worldP$Country!="Antarctica"),]

# calculate mean value of data to be plotted for the scaling procedure
mmQ=mean(Qm$production,na.rm=T)
mmP=mean(Pm$price,na.rm=T)

#plot production data using longitude and latitude as axis markers
banana_Q <- qplot(long, lat, data = worldQ, group = group,fill=production,geom ="polygon",ylab="",xlab="")+
  scale_fill_gradient2(high="green",mid="yellow",low="red",midpoint =mmQ )+ 
  labs(fill=" ")+
  theme_minimal()+
  theme(text = element_text(size=25), legend.position="bottom")+
  guides(fill = guide_colorbar(barwidth = 30, barheight = 0.5))

#plot price data using longitude and latitude as axis markers
banana_P <-qplot(long, lat, data = worldP, group = group,fill=price,geom ="polygon",ylab="",xlab="")+
  scale_fill_gradient2(high="green",mid="yellow",low="red",midpoint =mmP )+ 
  labs(fill="($/kg) ")+
  theme_minimal()+
  theme(text = element_text(size=25), legend.position="bottom")+
  guides(fill = guide_colorbar(barwidth = 30, barheight = 0.5))

# Part B: Calculate Correlation between Real Price and Per Capita Production

# read in saved data with updated country codes
P <- readRDS("./Convert/P.RDS")
GI <- readRDS("./Convert/GI.RDS")
Q <- readRDS("./Convert/Q.RDS")
N <- readRDS("./Convert/N.RDS")
 
# merge the grocery index (GI) and banana price (P) datasets
real <- merge(P , GI[ -c(2) ], by = "Country")
# calculate real price and add as a new column, then save dataframe
real$real_price <- 100*real$price/real$index_adjust
saveRDS(real, file = "./Convert/P_real.RDS")

# merge the production (Q) and population (N) datasets
capita <- merge(Q , N[ -c(2) ], by = "Country")
# calculate production per capita
capita$production_capita <- capita$production/capita$population

# create dataframe with production_capita and real price
real_capita <- merge(real[ -c(2,3,4,5) ] , capita[ -c(3,4,5) ], by = "Country")

# filter data for positive production and calculate real price and production correlation 
final <- filter(real_capita, real_capita$production_capita >0)
final <- na.omit(final) # eliminate rows with blank values
cor.test(final$real_price, final$production_capita, method = "pearson", use = "complete.obs")

# repeat correlation test but this time with logged data
final$logprice <- log(final$real_price)
final$logproduction <- log(final$production_capita)
cor.test(final$logprice, final$logproduction, method = "pearson", use = "complete.obs")

# generate a scatterplot of logged real price and per capita production
# plot(final$logprice, final$logproduction, main = "Plot of Logged Real Banana Price and Production",
#     xlab = "Log of Banana Production per Capita", ylab = "Log of Real Banana Price",
#     pch = 19, frame = FALSE)

# import trade data and add a trade_id column
bilateral <- read.csv(file="./Trade/trade.csv", header=TRUE, sep=",")
bilateral$trade_id <- seq.int(nrow(bilateral))

# merge trade with the real dataframe using exp_code from bilateral and Country from real
# rename real_price to exp_price
exp_price <- merge(bilateral, real[ -c(2,3,4,5) ], by.x="exp_code", by.y="Country")
names(exp_price)[names(exp_price) == 'real_price'] <- 'exp_price'

#repeat on the importer side
imp_price <- merge(bilateral, real[ -c(2,3,4,5) ], by.x="imp_code", by.y="Country")
names(imp_price)[names(imp_price) == 'real_price'] <- 'imp_price'

#merge the exp_price and imp_price data sets together using the trade_id variable
bilateral_final <- merge(exp_price[-c(1,4,5)], imp_price[-c(1,2,3,4)], by = "trade_id")

# eliminate duplicate rows
distinct(bilateral_final)

# calculate import price minus export price, and show summary statistics
bilateral_final$price_gap <- bilateral_final$imp_price - bilateral_final$exp_price

# create a columm of trade share in data set
bilateral_final$trade_sum <- sum(bilateral_final$trade_value)
bilateral_final$trade_share <- bilateral_final$trade_value/bilateral_final$trade_sum

# calculate weighted average price gap
sum(bilateral_final$trade_share*bilateral_final$price_gap)

#write.table(q, "./Convert/Groceries_Convert", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = NA)
