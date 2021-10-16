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
plot_price

plot_production <- ggplot(mean_data, aes(x = year, y = production)) +  
  geom_line() + 
  labs(title = "Yearly Hay Production", y= "tons ('000s)") + 
  theme(plot.title = element_text(size=10))
plot_production

plot_stocks <- ggplot(mean_data, aes(x = year)) + 
  geom_line(aes(y = stckMay, color = "May")) + 
  geom_line(aes(y = stckDec, color = "Dec")) + 
  labs(title = "May and December Corn Stocks", y = "tons ('000s)", x = "Date") +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(name = "Month", values = c("May" = "blue", "Dec" = "red"))
plot_stocks

# sumstats of May stocks
summary(mean_data$stckMay) 

# dummy var based on quartile value
mean_data <- mean_data %>%
  mutate(quant_cut = quantile(mean_data$stckMay, 0.25),
         stckDum = ifelse(stckMay<quant_cut,1,0))

# remove the first row
diff_data <- select(hay_data, c("year","month"))
diff_data <- diff_data[-1,]

# Take the first difference
diff_data <- diff_data %>% 
  mutate(price_diff = diff(hay_data$price))
head(diff_data)

# Convert to monthly format
full_data <- merge(x = diff_data, y = mean_data[,c("year","stckDum")], by = "year", all.x = TRUE)
# alternative function
# full_data <- left_join(x = diff_data, y = mean_data[,c("year","stckDum")], by = "year")

head(full_data)

# Create 11 monthly dummy vars
D <- hay_data$month
dummies <- model.matrix(~D+0)

col_order <- c("DJan","DFeb","DMar","DApr","DMay","DJun","DJul","DAug","DSep","DOct","DNov")
dummies <- dummies[, col_order]

# Add dummy vars to df
dummies_diff <- diff(dummies)
full_data <- cbind(full_data,dummies_diff)
head(full_data, 4)

# Create interaction terms
full_data <- full_data %>% 
  mutate(IJan = DJan*stckDum,
         IFeb = DFeb*stckDum,
         IMar = DMar*stckDum,
         IApr = DApr*stckDum,
         IMay = DMay*stckDum,
         IJun = DJun*stckDum,
         IJul = DJul*stckDum,
         IAug = DAug*stckDum,
         ISep = DSep*stckDum,
         IOct = DOct*stckDum,
         INov = DNov*stckDum)

head(full_data, 4)

# Estimate model without interaction vars
regP_diff <- lm(price_diff ~ DJan + DFeb + DMar + DApr + DMay + DJun + DJul + DAug + DSep + DOct + DNov + 0, data = full_data)
summary(regP_diff)
matrix_coef1 <- summary(regP_diff)$coefficients
coeff1 <- as.data.frame(matrix_coef1[,1])
colnames(coeff1) <- "dum"
coeff1

# Graph coefs of 11 dummy vars
label <- factor(c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"),
                levels = c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"))

coeff1 <- cbind(coeff1, label)

plot1 <- ggplot(coeff1, aes(x=label, y=dum)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Seasonal Price Estimates for Hay (Restricted Model)",
       x = "Current Month - December",
       y = "$ per ton")

plot1

# Estimating the unrestricted model 
regP_diff2 <- lm(price_diff ~ DJan + DFeb + DMar + DApr + DMay + DJun + DJul + DAug + DSep + DOct + DNov + IJan + IFeb + IMar + IApr + IMay + IJun + IJul + IAug + ISep + IOct + INov + 0, data = full_data)
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



