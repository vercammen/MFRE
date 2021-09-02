# The codes in this script are equivalent to those in the Palm_Oil document file starting from `Running the code` section. Please refer to the lecture notes for details on the code 

# Load packages. This code assumes you have installed {pacman}
pacman::p_load(here, rjson, dplyr, ggplot2, gridExtra)

# If the get.Comtrade() function won't work, the following code will import in the list objects A and B based from the case study
# load(here("Data", "comtradedata.RData"))

# Loads the ComTrade_Function.R script. This code assumes that the function is saved in the "Data" folder. 
source(here("Code", "ComTrade_Function.R"))

# Create object A based on the defined parameters and converts the list object to a df
  # r = 528 (NetherlandS)
  # p = 0 (World), 170 (Colombia), 320 (Guatemala), 360 (Indonesia), 458 (Malaysia)
  # ps = year 2020
  # rg = 1 (import)
  # cc = 151110 (crude palm oil)
A <- get.Comtrade(r="528", p="0, 170, 320, 360, 458", ps="2020", 
                  rg=1, cc="151110")

dfA <- as.data.frame(A$data)

# Clean up dfA and call it A_final
A_final <- dfA %>%
  mutate(Weight.A = as.numeric(dfA$TradeQuantity)/1000,
         Unit.A = "tonnes") %>%
  select(rtTitle, ptTitle, cmdCode, yr, rgDesc, Unit.A, Weight.A) %>%
  rename(Imp.A = rtTitle,
         Exp.A = ptTitle,
         Comm.A = cmdCode, 
         Year.A = yr,
         Type.A = rgDesc) %>%
  arrange(Weight.A)

A_final

# Import Shares 
A_final <- A_final %>% 
  mutate(share = Weight.A / Weight.A[n()])

# Code for Imports and Exports
# Create object B based on the defined parameters
  # r = 528 (Netherlands)
  # p = 0 (World)
  # ps = year 2020
  # rg = 1 (imports), 2 (exports)
  # cc = 151110 (crude palm oil), refined palm oil (151190), biodiesel (3826)

B <- get.Comtrade(r="528", p="0", ps="2020", 
                  rg="1,2", cc="151110, 151190, 3826")
dfB <- as.data.frame(B$data)

dfBsum <- dfB %>%
  mutate(Weight.B = as.numeric(TradeQuantity)/1000,
         Unit.B = "tonnes") %>%
  select(rtTitle, ptTitle, cmdCode, yr, rgDesc, Unit.B, Weight.B) %>%
  rename(Imp.B = rtTitle,
         Exp.B = ptTitle,
         Comm.B = cmdCode, 
         Year.B = yr,
         Type.B = rgDesc)
dfBsum

# Exercise 2
dfBsum[2,c(3)]
dfBsum[2,c("Comm.B")]

# Exercise 2 Solution: Calculate weight of domestic consumption
import_tot <- dfBsum[1,c("Weight.B")] + dfBsum[3,c("Weight.B")] + dfBsum[5,c("Weight.B")]
export_tot <- dfBsum[2,c("Weight.B")] + dfBsum[4,c("Weight.B")] + dfBsum[6,c("Weight.B")]
import_tot
export_tot
domestic <- import_tot - export_tot
domestic

# Local Versus U.S. Dollars
graphics.off() # close all plots
data <- read.csv(here("Data", "palm_data.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE)
data$date <- seq(as.Date("2012/1/1"), by = "month", length.out = 114)
head(data)

# Plot the two price series side by side
plot_rotterdam <- ggplot(data, aes(x = date, y = rotterdam)) +  
  geom_line() + 
  labs(title = "Rotterdam Price", y= "CPO: US$/tonne", x = "Date") + 
  theme(plot.title = element_text(size=10))

plot_malaysia <- ggplot(data, aes(x = date, y = malaysia)) +  
  geom_line() + 
  labs(title = "Malaysian Price", y = "CPO: RM/tonne", x = "Date") + 
  theme(plot.title = element_text(size=10))

grid.arrange(plot_rotterdam, plot_malaysia, ncol = 2) 

# Isolating Exchange Rates Impacts
data <- mutate(data, priceRatio = malaysia/rotterdam)

plot_PriceRatio <- ggplot(data, aes(x = date, y = priceRatio)) +  
  geom_line() + 
  labs(title ="Malay-Rotterdam Price Ratio", y= "Ratio", x = "Date")+ theme(plot.title = element_text(size=10))

plot_Exchange <- ggplot(data, aes(x = date, y = exchange)) +  
  geom_line() + 
  labs(title = "Malay-U.S. Exchange", y= "RM per One US$", x = "Date") + 
  theme(plot.title = element_text(size=10))

grid.arrange(plot_PriceRatio, plot_Exchange, ncol = 2) 

# Analysis of Price Spread
data <- mutate(data, spread = rotterdam - (malaysia/exchange))

plot_Spread <- ggplot(data, aes(x = date, y = spread)) +  
  geom_line() + 
  labs(title = "Rotterdam - Malaysian Price Spread", y= "US$/tonne", x = "Date") + 
  theme(plot.title = element_text(size=10)) + 
  geom_hline(yintercept = mean(data$spread), color="blue")

# Analysis of Transportation Costs
plot_Corn <- ggplot(data, aes(x = date, y = cornShip)) +  
  geom_line() + 
  labs(title = "U.S. to Japan Corn Rate", y= "US$/tonne", x = "Date") +
  theme(plot.title = element_text(size=10)) + 
  geom_hline(yintercept = mean(data$cornShip), color="blue")

grid.arrange(plot_Spread, plot_Corn, ncol = 2)

# Regression of Spread on Corn Transport Cost
lmCorn <- lm(data$spread~data$cornShip, data = data)
summary(lmCorn)
plot_Spread
