pacman::p_load(dplyr, here)


# read in price and stocks data
price_data <- read.csv(here("data", "price.csv"))
stock_data <- read.csv(here("data", "stocks.csv"))

# merge data
all_data <- left_join(price_data, stock_data, by = c("year"))
head(all_data)

# Next, I create the following variables and drop the first row:
      # quarter dummies
      # high stock dummy (= 1 if stocks > 108)
      # interaction term
      # price difference 
      # "dummy differences"
      # "stationarity differences" 
      # "interaction differences" 

all_data <- all_data %>%
  mutate(q1 = ifelse(quarter == 1, 1, 0),     # quarter dummies
         q2 = ifelse(quarter == 2, 1, 0),
         q3 = ifelse(quarter == 3, 1, 0),
         highstock = ifelse(stocks > 108, 1, 0), # high stock dummy 
         highstock_q1 = highstock * q1,      # interaction
         highstock_q2 = highstock * q2,
         highstock_q3 = highstock * q3, 
         d_price = price - lag(price),        # stationarity diff
         d_q1 = q1 - lag(q1),                 # dummy diff
         d_q2 = q2 - lag(q2),
         d_q3 = q3 - lag(q3),
         d_highstock = highstock - lag(highstock),  # stock diff 
         d_highstock_q1 = highstock_q1 - lag(highstock_q1), # interaction diff
         d_highstock_q2 = highstock_q2 - lag(highstock_q2),
         d_highstock_q3 = highstock_q3 - lag(highstock_q3)) %>% 
  slice(-1)                                  # drop first row

head(all_data)

# Finally, I estimate the differenced model
model <- lm(d_price ~ d_q1 + d_q2 + d_q3 + d_highstock + d_highstock_q1 + 
              d_highstock_q2 + d_highstock_q3 + 0, data = all_data)
summary(model)