rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2)
profits <- readRDS(file = "compare.rds")
head(profits)

profits <- profits %>% mutate(BasisChange = cornBasis3 - cornBasis1) # this could have been added in hedging_simulate.R rather than here.


# Currently I manually change the X variable to different names to generate a new histogram.
# The better alternative would be to have separate code for each histogram.
# The variables in question are the names of the five histograms in the Images folder.
# I CAN'T FIX WARNING MESSAGE ABOUT SELECTING BIN SIZE

ggplot(profits, aes(x=crushH)) + geom_histogram()
# Change the width of bins
ggplot(profits, aes(x=crushH)) + 
  geom_histogram(binwidth=0.1)
# Change colors
p<-ggplot(profits, aes(x=crushH)) +  xlab("Deviation from Target Crush Margin, Hedged ($/bu)") +
  geom_histogram(color="black", fill="white")
p

# Calculate the percentiles which are associated with each histogram.
# There should also be one of these for each of the five variables 
# I am currently manually changing the names to generate a new set of percentiles
# I pasted the results directly into my slides. Better to have a copy of all results here.
crushH_sorted <- sort(profits$crushH)
crushH_percentile <- quantile(crushH_sorted, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))
crushH_percentile

 

