# load packages
pacman::p_load(here, dplyr, ggplot2, janitor, tidyr)

# optional: creating the rds file 
# load in the wasde files
dataFirst <- read.csv(here("Data", "oce-wasde-report-data-2010-04-to-2015-12.csv"))
dataSecond <- read.csv(here("Data", "oce-wasde-report-data-2016-01-to-2020-12.csv"))

# combine these two files using the rbind() function (rbind = rowbind)
data <- rbind(dataFirst, dataSecond)

# create new dataframe called wasde that contains only the corn commodity from the data dataframe
wasde <- filter(data, Commodity == "Corn", ProjEstFlag == "Proj.")

# save this dataframe as an RDS file and call it wasde_corn_proj.RDS
saveRDS(wasde, here("Data", "wasde_corn_proj.rds"))

# begin data cleaning
wasdeAll <- readRDS(here("Data", "wasde_corn_proj.rds"))

# delete unwanted columns
wasde <- select(wasdeAll, -c("ReportDate", "ReportTitle","ReliabilityProjection", "Region",
                             "AnnualQuarterFlag","ReleaseTime", "Unit", "ProjEstFlag"))

# print 15 rows of the wasde dataframe
head(wasde, 15)

# drop wasdenumber = 481 because of capitalization issues
wasde <- filter(wasde, WasdeNumber != 481)

# rename columns
wasde <- wasde %>% rename(Date = ReleaseDate,
                          Year = ForecastYear,
                          Month = ForecastMonth)

# filter observations
wasde <- wasde %>%
  filter(Attribute %in% c("Area Harvested", "Yield per Harvested Acre", "Use, Total"))

table(wasde$Attribute)

# convert long to wide dataframe
wasde_wide <- wasde %>%
  pivot_wider(names_from = Attribute,
              values_from = Value) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  rename(Acres = `Area Harvested`,
         Use = `Use, Total`,
         Yield = `Yield per Harvested Acre`)

# begin Q3 - analysis

# load corn
corn <- read.csv(here("Data", "corn_price.csv"))
head(corn)

# clean corn
corn <- corn %>%
  rename(P_current = corn_price) %>%
  mutate(Date = as.Date(Date, format = c("%m/%d/%Y")),
         P_diff = P_current - lag(P_current))

# merge data
all_data <- left_join(wasde_wide, corn, by = c("Date"))
head(all_data)
dim(all_data)

# create lag vars
all_data <- all_data %>%
  mutate(A_diff = Acres - lag(Acres),
         U_diff = Use - lag(Use),
         Y_diff = Yield - lag(Yield)) %>%
  slice(-1)

# filter april-may diff because of change in marketing year
noMay <- filter(all_data, Month != 5)

# run monthly regression
model_lm <- lm(P_diff ~ A_diff + U_diff + Y_diff, data = noMay)
summary(model_lm)

# create vars for event study
wasde_wide <- wasde_wide %>%
  mutate(d_acres = ifelse(Acres - lag(Acres)>0, 1,
                          ifelse(Acres - lag(Acres)==0, 0,
                                 ifelse(Acres-lag(Acres)<0, -1, NA))),
         d_use = ifelse(Use - lag(Use)>0, 1,
                        ifelse(Use - lag(Use)==0, 0, 
                               ifelse(Use-lag(Use)<0, -1, NA))),
         d_yield = ifelse(Yield - lag(Yield)>0, 1,
                          ifelse(Yield - lag(Yield)==0, 0, 
                                 ifelse(Yield-lag(Yield)<0, -1, NA))))

# merge corn data and wasde data
corn_wasde <- left_join(corn, wasde_wide, by = c("Date"))
head(corn_wasde)

# estimate event study
eventstudy <- lm(P_current ~ lag(P_current) + d_acres + d_use + d_yield, data = corn_wasde)
summary(eventstudy)
