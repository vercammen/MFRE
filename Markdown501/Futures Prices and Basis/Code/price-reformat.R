
# load packages and data
#pacman::p_load(here, dplyr, ggplot2)

# load data
price1 <- read.csv(here("Data", "price1.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price2 <- read.csv(here("Data", "price2.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price3 <- read.csv(here("Data", "price3.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price4 <- read.csv(here("Data", "price4.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price5 <- read.csv(here("Data", "price5.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price6 <- read.csv(here("Data", "price6.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price7 <- read.csv(here("Data", "price7.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
price8 <- read.csv(here("Data", "price8.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 
priceSpot <- read.csv(here("Data", "priceSpot.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 

# create the function which retrieves a row of the simulated price data

get_simulated <- function(row_select) {

# create null vector for each of 8 contracts and spot price.
pQ1 <- c()
pQ2 <- c()
pQ3 <- c()
pQ4 <- c()
pQ5 <- c()
pQ6 <- c()
pQ7 <- c()
pQ8 <- c()
pSpot <- c()

# transpose the rows from imported data into columns
pQ1[1] <- price1[row_select,"P_1_1"]
length(pQ1) <- 8

pQ2[1] <- price2[row_select,"P_1_2"]
pQ2[2] <- price2[row_select,"P_2_2"]
length(pQ2) <- 8

pQ3[1] <- price3[row_select,"P_1_3"]
pQ3[2] <- price3[row_select,"P_2_3"]
pQ3[3] <- price3[row_select,"P_3_3"]
length(pQ3) <- 8

pQ4[1] <- price4[row_select,"P_1_4"]
pQ4[2] <- price4[row_select,"P_2_4"]
pQ4[3] <- price4[row_select,"P_3_4"]
pQ4[4] <- price4[row_select,"P_4_4"]
length(pQ4) <- 8

pQ5[1] <- price5[row_select,"P_1_5"]
pQ5[2] <- price5[row_select,"P_2_5"]
pQ5[3] <- price5[row_select,"P_3_5"]
pQ5[4] <- price5[row_select,"P_4_5"]
pQ5[5] <- price5[row_select,"P_5_5"]
length(pQ5) <- 8

pQ6[1] <- price6[row_select,"P_1_6"]
pQ6[2] <- price6[row_select,"P_2_6"]
pQ6[3] <- price6[row_select,"P_3_6"]
pQ6[4] <- price6[row_select,"P_4_6"]
pQ6[5] <- price6[row_select,"P_5_6"]
pQ6[6] <- price6[row_select,"P_6_6"]
length(pQ6) <- 8

pQ7[1] <- price7[row_select,"P_1_7"]
pQ7[2] <- price7[row_select,"P_2_7"]
pQ7[3] <- price7[row_select,"P_3_7"]
pQ7[4] <- price7[row_select,"P_4_7"]
pQ7[5] <- price7[row_select,"P_5_7"]
pQ7[6] <- price7[row_select,"P_6_7"]
pQ7[7] <- price7[row_select,"P_7_7"]
length(pQ7) <- 8

pQ8[1] <- price8[row_select,"P_1_8"]
pQ8[2] <- price8[row_select,"P_2_8"]
pQ8[3] <- price8[row_select,"P_3_8"]
pQ8[4] <- price8[row_select,"P_4_8"]
pQ8[5] <- price8[row_select,"P_5_8"]
pQ8[6] <- price8[row_select,"P_6_8"]
pQ8[7] <- price8[row_select,"P_7_8"]
pQ8[8] <- price8[row_select,"P_8_8"]
length(pQ8) <- 8

# bind the columns of data together
price_sim <- cbind(pQ1,pQ2,pQ3,pQ4,pQ5,pQ6,pQ7,pQ8)

}

