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

grid.arrange(plot_sandp, plot_corn, ncol = 2)


