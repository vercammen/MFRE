rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2, gridExtra, lubridate)
beta_data <- read.csv(here("Data", "corn_beta.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
head(beta_data)

# convert trade_day from character to date
# beta_data$trade_day <- as.Date(beta_data$trade_day, format = c("%d-%b-%y"))
# %b means the month is spelled in text (3 character, jan/feb/mar)

beta_data <- mutate(beta_data, trade_day = as.Date(trade_day, format = c("%d-%b-%y")))

# create 2 side by side graph
plot_sandp <- ggplot(beta_data, aes(x = trade_day, y = sandp)) +
  geom_line() +
  labs(title = "S&P", y = "US$", x = "Date") +
  theme(plot.title = element_text(size = 10))

plot_corn <- ggplot(beta_data, aes(x = trade_day, y = corn)) +
  geom_line() +
  labs(title = "Corn", y = "US$", x = "Date") +
  theme(plot.title = element_text(size = 10))

## grid.arrange(plot_sandp, plot_corn, ncol = 2)

# optional --> calculate monthly average

# I first created the year and month variables. Then calculated the average S&P and corn prices, respectively, by year and month. 

# To plot dates in R, ggplot likes the date format to have an actual date in it (i.e., cannot just be year-month, it has to be some combination of year-month-date.) So when we create this date variable, we will paste/concatenate year-month-"01" to indicate the 1st of every month. It could be any number from 01-30 too. The sep = "-" means that we the include a "-" in between year, month, and 01 (year-month-01). If we put sep = "/", then it will look like year/month/01. 
# since month is in numbers, it is indicated as %m in the format

monthly_avg <- beta_data %>% 
  mutate(year = year(trade_day), 
         month = month(trade_day)) %>%
  group_by(year, month) %>%
  summarize(avg_sandp = mean(sandp),
            avg_corn = mean(corn)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))

head(monthly_avg)

# plot
plot_sandp_avg <- ggplot(monthly_avg, aes(x = date, y = avg_sandp)) +
  geom_line() +
  labs(title = "S&P (monthly average)", y = "US$", x = "Date") +
  theme(plot.title = element_text(size = 10)) 
# + scale_y_continuous(limits = c(0, 4600))

plot_corn_avg <- ggplot(monthly_avg, aes(x = date, y = avg_corn)) +
  geom_line() +
  labs(title = "Corn (monthly average)", y = "US$", x = "Date") +
  theme(plot.title = element_text(size = 10)) 
# + scale_y_continuous(limits = c(0, 800))

## grid.arrange(plot_sandp_avg, plot_corn_avg, ncol = 2)

# calculate the correlation for daily S&P index and corn price.
cor(beta_data[,c("sandp")],beta_data[,c("corn")])

# add the log of the first difference for S&P and corn price, eliminate first row
beta_data <- beta_data %>% 
  mutate(sandp_rtn = log(sandp) - log(lag(sandp))) %>%
  mutate(corn_rtn = log(corn) - log(lag(corn))) %>%
  slice(-1) # drop first row

head(beta_data)

# calculate the correlation for S&P and corn daily return.
cor(beta_data[,c("sandp_rtn")],beta_data[,c("corn_rtn")])

# regress corn_rtn on S&P_rtn
model_lm <- lm(corn_rtn ~ sandp_rtn, data = beta_data)
summary(model_lm)