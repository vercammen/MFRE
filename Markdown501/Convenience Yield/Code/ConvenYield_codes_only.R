rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2)
hay_data <- read.csv(here("Data", "usda_hay.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
head(hay_data)

# delete the month column and average each column
hay_data2 <- select(hay_data, -"month")

mean_data <- hay_data2 %>%
  group_by(year) %>%
  summarise_all(list(mean))

# plots
plot_price <- ggplot(mean_data, aes(x = year, y = price)) +  
  geom_line() + 
  labs(title = "Yearly Avg Hay Price", y= "$/ton") + 
  theme(plot.title = element_text(size=10))
#plot_price

plot_production <- ggplot(mean_data, aes(x = year, y = production)) +  
  geom_line() + 
  labs(title = "Yearly Hay Production", y= "tons ('000s)") + 
  theme(plot.title = element_text(size=10))
#plot_production

plot_stocks <- ggplot(mean_data, aes(x = year)) + 
  geom_line(aes(y = stckMay, color = "May")) + 
  geom_line(aes(y = stckDec, color = "Dec")) + 
  labs(title = "May and December Corn Stocks", y = "tons ('000s)", x = "Date") +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(name = "Month", values = c("May" = "blue", "Dec" = "red"))
# plot_stocks

# sumstats of May stocks
summary(mean_data$stckMay) 

# calculate the 25% quartile for May stocks - threshold for low stocks
quant_cut = quantile(mean_data$stckMay, 0.25)

# add the low stocks indicator variable (based on quartile value) to monthly data
hay_data <- hay_data %>%
  mutate(stckDum = ifelse(stckMay<quant_cut,1,0) )
head(hay_data)

# Add 11 monthly dummies to merged monthly data
hay_data <- hay_data %>% 
  mutate(d1 = ifelse(month=="Jan",1,0), 
         d2 = ifelse(month=="Feb",1,0),
         d3 = ifelse(month=="Mar",1,0),
         d4 = ifelse(month=="Apr",1,0),
         d5 = ifelse(month=="May",1,0),
         d6 = ifelse(month=="Jun",1,0),
         d7 = ifelse(month=="Jul",1,0),
         d8 = ifelse(month=="Aug",1,0),
         d9 = ifelse(month=="Sep",1,0),
         d10 = ifelse(month=="Oct",1,0),
         d11 = ifelse(month=="Nov",1,0),
         d12 = ifelse(month=="Dec",1,0))
head(hay_data, 15)

# add 12 interaction variables to monthly data
hay_data <- hay_data %>% 
  mutate(i0 = stckDum, 
         i1 = d1*stckDum,
         i2 = d2*stckDum,
         i3 = d3*stckDum,
         i4 = d4*stckDum,
         i5 = d5*stckDum,
         i6 = d6*stckDum,
         i7 = d7*stckDum,
         i8 = d8*stckDum,
         i9 = d9*stckDum,
         i10 = d10*stckDum,
         i11 = d11*stckDum,
         i12 = d12*stckDum)
head(hay_data, 15)

# add the first differences to the monthly data
diff_data <- hay_data %>%
  mutate(price_diff = price - lag(price), 
         d1Diff = d1 - lag(d1), 
         d2Diff = d2 - lag(d2), 
         d3Diff = d3 - lag(d3),
         d4Diff = d4 - lag(d4),
         d5Diff = d5 - lag(d5),
         d6Diff = d6 - lag(d6),
         d7Diff = d7 - lag(d7),
         d8Diff = d8 - lag(d8),
         d9Diff = d9 - lag(d9),
         d10Diff = d10 - lag(d10),
         d11Diff = d11 - lag(d11),
         d12Diff = d12 - lag(d12),
         i0Diff = i0 - lag(i0),
         i1Diff = i1 - lag(i1),
         i2Diff = i2 - lag(i2),
         i3Diff = i3 - lag(i3),
         i4Diff = i4 - lag(i4),
         i5Diff = i5 - lag(i5),
         i6Diff = i6 - lag(i6),
         i7Diff = i7 - lag(i7),
         i8Diff = i8 - lag(i8),
         i9Diff = i9 - lag(i9),
         i10Diff = i10 - lag(i10),
         i11Diff = i11 - lag(i11),
         i12Diff = i12 - lag(i12)) %>%
  slice(-1) #drops first row 

head(diff_data, 10)

# Estimate model without interaction vars
regP_diff <- lm(price_diff ~ d2Diff + d3Diff + d4Diff + d5Diff + d6Diff + d7Diff + d8Diff + d9Diff + d10Diff + d11Diff + d12Diff + 0, data = diff_data)
summary(regP_diff)
matrix_coef1 <- summary(regP_diff)$coefficients
coeff1 <- as.data.frame(matrix_coef1[,1])
colnames(coeff1) <- "dum"
coeff1

# Graph coefs of 11 dummy vars
label <- factor(c("Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                levels = c("Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

coeff1 <- cbind(coeff1, label)

plot1 <- ggplot(coeff1, aes(x=label, y=dum)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Seasonal Price Estimates for Hay (Restricted Model)",
       x = "Current Month - December",
       y = "$ per ton")
plot1

# Estimating the model with the interaction variables
regP_diff2 <- lm(price_diff ~ d2Diff + d3Diff + d4Diff + d5Diff + d6Diff + d7Diff + d8Diff + d9Diff + d10Diff + d11Diff + d12Diff
                  + i0Diff  + i2Diff + i3Diff + i4Diff + i5Diff + i6Diff + i7Diff + i8Diff + i9Diff + i10Diff + i11Diff +  i12Diff + 0, data = diff_data)
summary(regP_diff2)
matrix_coef2 <- summary(regP_diff2)$coefficients
coeff2 <- as.data.frame(matrix_coef2[,1])
coeff2

coeff2_A <- coeff2 %>% slice(1:11)
coeff2_A <- coeff2_A 
colnames(coeff2_A) <- "dum"

coeff2_B <- coeff2 %>% slice(12:22)
colnames(coeff2_B) <- "inter"

coeff2 <-  cbind(coeff2_A,coeff2_B)

coeff2  <- coeff2 %>% 
  mutate(dum_plus_inter = dum + inter )   
coeff2

plot2A <- ggplot(coeff2, aes(x=label, y=dum)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Seasonal Price Estimates for Hay: Normal Stock Years",
       x = "Current Month - December",
       y = "$ per ton")

plot2A

plot2B <- ggplot(coeff2, aes(x=label, y=dum_plus_inter)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Seasonal Price Estimates for Hay: Low Stock Years",
       x = "Current Month - December",
       y = "$ per ton")

plot2B

# Simulation parameters

a <- 16.21
b <- 3.50
m0 <- -0.22
m1 <- 0.03
S0 <- 2.015
H1 <- 14.38
S_bar <- 2.015
v <- c(a, b, m0, m1, S0, H1, S_bar)

# Source price_function.R (saved inside "Code" folder)
source(here("Code", "price_function.R"))
del <- get_delta(v)
del

get_price <- function(H5,D) {
  Price <- del[,1] + del[,2]*H5 + del[,3]*D 
}  

H5 <- 14.38
D <- 0
P <- get_price(H5,D)
P

# Compare simulation with real world data
historic <- c(2.866, 2.993, 3.123, 3.105, 2.866, 2.993, 3.123, 3.105)
barplot(historic,names.arg = c(1,2,3,4,5,6,7,8), main="Average Quarterly Corn Price: 1980-2019.",
        xlab="Quarter: Sept-Nov = 1, Dec-Feb = 2, etc", ylab="$/bu",
        beside=TRUE, ylim=c(2, 4), xpd = FALSE)

historic_4 <- historic[1:4]
simulated <- P[1:4]
all_price <- cbind(historic_4,simulated)
rownames(all_price) <- c("Q1","Q2","Q3","Q4")
all_price

barplot(all_price, main="Quarterly Corn Price: Historic vs Simulated",ylab="$/bu",
        col=c("darkblue","red", "green", "brown"),
        legend = rownames(all_price), args.legend = list(x = "topleft", cex = .7), beside=TRUE, ylim=c(2, 4), xpd = FALSE)

# What If Analysis (H5)
H5 <- 13
D <- 0
P_rev <- get_price(H5,D)
P_rev

all_price2 <- cbind(P,P_rev)
all_price2

barplot(all_price2, main="Low H5 Prices versus Base Prices",ylab="$/bu", sub="Left Plot is Base Prices, Right Plot is Low H5 Prices", names.arg=c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8),
        col=c("darkblue","red", "green", "brown"),
        beside=TRUE, ylim=c(3, 5), xpd = FALSE)

impacts <- P_rev - P
impacts

# What if analysis (storage costs)

m0 <- -0.15 # base value = -0.22
v <- c(a, b, m0, m1, S0, H1, S_bar)
H5 <- 14.38 # base value = 14.38
D <- 0
del <- get_delta(v)
P_rev2 <- get_price(H5,D)
P_rev2

all_price3 <- cbind(P,P_rev2)

barplot(all_price3, main="High Storage Cost Prices vs. Base Prices",ylab="$/bu", sub="Left is base; right is high strg cost Prices", names.arg=c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8),
        col=c("darkblue","red", "green", "brown"),
        beside=TRUE, ylim=c(3, 5), xpd = FALSE)

# Appendix
m0 <- -0.22 # base value = -0.22
v <- c(a, b, m0, m1, S0, H1, S_bar)
H5 <- 14.38 # base value = 14.38
D <- 0
del <- get_delta(v)
P_chck <- get_price(H5,D)

X <- a/b - 1/b*P_chck
S <- rep(0, times = 8)
S[1] <- S0 + H1 - X[1]
S[2] <- S[1] - X[2]
S[3] <- S[2] - X[3]
S[4] <- S[3] - X[4]
S[5] <- S[4] + H5 - X[5]
S[6] <- S[5] - X[6]
S[7] <- S[6] - X[7]
S[8] <- S[7] - X[8]
S

lop <- rep(0, times = 8)
for(i in 1:7)
{
  lop[i+1] <- P[i+1]-P[i]-(m0+m1*S[i])
}
lop



