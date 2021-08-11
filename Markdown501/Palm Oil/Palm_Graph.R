rm(list = ls())
graphics.off()
#setwd("C:/Users/Jim/Dropbox/Git/Markdown501/Palm Oil")
setwd("C:/Users/vercamme/Dropbox/Git/Markdown501/Palm Oil")
library(ggplot2)
library(gridExtra)
data <- read.csv(file="./Data/palm_data.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
head(data)
data$date <- seq(as.Date("2012/1/1"), by = "month", length.out = 114)
 
plot_rotterdam <- ggplot(data, aes(x = date, y = rotterdam)) +  
  geom_line() +ggtitle("Rotterdam Price of CPO")+labs(y= "US$/tonne")
plot_malaysia <- ggplot(data, aes(x = date, y = malaysia)) +  
  geom_line() +ggtitle("Malaysian Price of CPO")+labs(y= "RM/tonne")
grid.arrange(plot_rotterdam, plot_malaysia, ncol = 2)    

data$priceRatio <- data$malaysia/data$rotterdam
plot_PriceRatio <- ggplot(data, aes(x = date, y = priceRatio)) +  
  geom_line() +ggtitle("Malay-Rotterdam Price Ratio")+labs(y= "Ratio")
plot_Exchange <- ggplot(data, aes(x = date, y = exchange)) +  
  geom_line() +ggtitle("Malay-U.S. Exchange")+labs(y= "RM per One US$")
grid.arrange(plot_PriceRatio, plot_Exchange, ncol = 2)  

data$spread <- data$rotterdam -data$malaysia/data$exchange
plot_Spread <- ggplot(data, aes(x = date, y = spread)) +  
  geom_line() +ggtitle("Rotterdam - Malay Price Spread")+labs(y= "US$/tonne")+geom_hline(yintercept = mean(data$spread), color="blue")
plot_Spread

plot_Corn <- ggplot(data, aes(x = date, y = cornShip)) +  
  geom_line() +ggtitle("U.S. to Japan Corn Rate")+labs(y= "US$/tonne")+geom_hline(yintercept = mean(data$cornShip), color="blue")
 
grid.arrange(plot_Spread, plot_Corn, ncol = 2)  


lmCorn <- lm(data$spread~data$cornShip, data = data)
summary(lmCorn)

elasCorn <- lmCorn$coefficients[2]*mean(data$cornShip)/mean(data$spread)
elasCorn






