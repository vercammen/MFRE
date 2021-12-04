rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2)
data <- read.csv(here("Data", "corn_ethanol_data.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 

data$Date <- as.Date(data$Week, format = c("%m/%d/%Y")) 
head(data)

# plot the three spot price data series
plot_spot_corn <- ggplot(data, aes(x = Date, y = CornSpot)) +  
  geom_line() + 
  labs(title = "Weekly Corn Spot Price, Iowa", y= "$/bu", x="") + 
  theme(plot.title = element_text(size=10)) 
#plot_spot_corn

plot_spot_DDG <- ggplot(data, aes(x = Date, y = DDG)) +  
  geom_line() + 
  labs(title = "Weekly DDG Spot Price, Iowa", y= "$/Ton", x="") + 
  theme(plot.title = element_text(size=10)) 
#plot_spot_DDG

plot_spot_ethanol <- ggplot(data, aes(x = Date, y = Ethanol)) +  
  geom_line() + 
  labs(title = "Weekly Ethanol Spot Price, Iowa", y= "$/Gallon", x="") + 
  theme(plot.title = element_text(size=10)) 
#plot_spot_ethanol

# uncomment to overwrite files
# ggsave(plot_spot_corn, file = here("Images", "CornSpot.png"), dpi = 150)
# ggsave(plot_spot_DDG, file = here("Images", "DDG.png"), dpi = 150)
# ggsave(plot_spot_ethanol, file = here("Images", "Ethanol.png"), dpi = 150)

# plot the two basis series
plot_basis_corn <- ggplot(data, aes(x = Date, y = BasisCorn)) +  
  geom_line() + 
  labs(title = "Weekly Corn Basis, Iowa", y= "$/bu", x="") + 
  theme(plot.title = element_text(size=10)) 
#plot_basis_corn

plot_basis_ethan <- ggplot(data, aes(x = Date, y = BasisEthan)) +  
  geom_line() + 
  labs(title = "Weekly Ethanol Basis, Iowa", y= "$/gallon", x="") + 
  theme(plot.title = element_text(size=10)) 
#plot_basis_ethan

# uncomment to overwrite files
#ggsave(plot_basis_corn, file = here("Images", "BasisCorn.png"), dpi = 150)
#ggsave(plot_basis_ethan, file = here("Images", "BasisEthanol.png"), dpi = 150)

# construct an ethanol revenue and cost data series (standardized to $/bu) and then plot.
data <- mutate(data, 
               Revenue = 0.0085*DDG + 2.8*Ethanol,
               Cost = CornSpot)

plot_crush <- ggplot(data, aes(x = Date)) + 
  geom_line(aes(y = Revenue, color = "Revenue")) + 
  geom_line(aes(y = Cost, color = "Cost")) + 
  labs(title = "Revenue and Cost in Ethanol Productions", y = "$/bu", x="") +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(name = "Variable", values = c("Revenue" = "blue", "Cost" = "red"))
#plot_crush

# uncomment to overwrite files
# ggsave(plot_crush, file = here("Images", "Revenue_Cost.png"), dpi = 150)

# optimal hedge ratio (beta when regression log of spot price on log of futures price)
data <- data %>%
  mutate(log_spot = log(CornSpot),
         log_fut = log(CornFut/100))

hedge_ratio <- lm(log_spot ~ log_fut, data = data)
summary(hedge_ratio)

# Plot the wheat prices and basis for Kansas (end of module material)
 
# read in the wheat data
prices <- read.csv(here("Data", "wheat_covid.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE)
prices$Date <- as.Date(prices$Date, format = c("%m/%d/%Y"))
prices <- prices %>% mutate(BasisMay = Spot - May, BasisDec = Spot - Dec)

plot_prices <- ggplot(prices, aes(x = Date)) +  
  geom_line(aes(y = May, color = "May Futures")) + 
  geom_line(aes(y = Dec, color = "Dec Futures")) + 
  geom_line(aes(y = Spot, color = "Kansas Spot")) +
  labs(title = "Wheat Spot Price (Kansas) and Wheat Futures: Febuary to April, 2020", y= "$/bu", x="") + 
  theme(plot.title = element_text(size=10)) + 
  scale_color_manual(name = "Variable", values = c("May Futures" = "blue", "Dec Futures" = "red", "Kansas Spot" = "black"))
#plot_prices

plot_basis <- ggplot(prices, aes(x = Date)) +  
  geom_line(aes(y = BasisMay, color = "May Basis")) + 
  geom_line(aes(y = BasisDec, color = "Dec Basis")) + 
  labs(title = "Kansas Wheat Basis (May & Dec Futures): Febuary to April, 2020", y= "$/bu", x="") + 
  theme(plot.title = element_text(size=10)) + 
  scale_color_manual(name = "Variable", values = c("May Basis" = "blue", "Dec Basis" = "red"))
#plot_basis

# uncomment to overwrite files
# ggsave(plot_prices, file = here("Images", "WheatPrices.png"), dpi = 150)
# ggsave(plot_basis, file = here("Images", "WheatBasis.png"), dpi = 150)

# calculate hedging profits assuming hedge is created on March 18
# filter the data for prices beginning March 18
prices2 <- prices %>% filter(Date > "0020-03-15")
start_may <- prices2[1,"BasisMay"]
start_dec <- prices2[1,"BasisDec"]
prices2 <- prices2 %>% mutate(count = c(1:n()), BasisChMay = BasisMay - start_may, BasisChDec = BasisDec - start_dec)

# graph the change in basis for varying hedging periods ==> short hedging profits
plot_profits <- ggplot(prices2, aes(x = count)) +  
  geom_line(aes(y = BasisChMay, color = "May Basis Change")) + 
  geom_line(aes(y = BasisChDec, color = "Dec Basis Change")) + 
  labs(title = "Short Wheat Hedging Profits: Days Left in Hedge", y= "$/bu", x="Days Left in Hedge") + 
  theme(plot.title = element_text(size=10)) + 
  scale_color_manual(name = "Variable", values = c("May Basis Change" = "blue", "Dec Basis Change" = "red"))
plot_profits

# uncomment to overwrite files
#ggsave(plot_profits, file = here("Images", "WheatProfits.png"), dpi = 150)