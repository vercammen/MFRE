# this program provides the solution to the assignment 1 question on honey trade

# <CTRL> L to clear screen
rm(list = ls())
# index the three regions as follows: Canada = 1, U.S. = 2, Japan = 3
# attach observed values to total supply (Qs) and demand (Qd) 
Qs_1 <- 16515
Qs_2 <- 265322
Qs_3 <- 44495
Qd_1 <- 6465 
Qd_2 <- 272232
Qd_3 <- 47635
# attach observed prices to the pricing variable, P
P_1 <- 3855
P_2 <- 4271.28
P_3 <- 5002.52
# specify transportation costs
T_12 <- P_2 - P_1
T_13 <- P_3 - P_1
T_23 <- 800
T_21 <- T_12
T_31 <- T_13
T_32 <- T_23
# place transportation costs in a symetric matrix
T <- matrix(c(0, T_12, T_13, T_21, 0, T_23, T_31, T_32, 0 ), nrow=3, ncol=3) 
# specify demand and supply elasticities
ED <- -0.5
ES <- 1.5
# derive the slopes of the three inverse demand schedules
b_1 <- -(1/ED)*P_1/Qd_1
b_2 <- -(1/ED)*P_2/Qd_2
b_3 <- -(1/ED)*P_3/Qd_3
# derive the slopes of the three inverse supply schedules
beta_1 = (1/ES)*P_1/Qs_1
beta_2 = (1/ES)*P_2/Qs_2
beta_3 = (1/ES)*P_3/Qs_3
# derive the intercepts of the three inverse demand schedules
a_1 <- P_1*(1-1/ED)
a_2 <- P_2*(1-1/ED)
a_3 <- P_3*(1-1/ED)
# derive the intercepts of the three inverse supply schedules
alpha_1 <- P_1*(1-1/ES)
alpha_2 <- P_2*(1-1/ES)
alpha_3 <- P_3*(1-1/ES)

# create parameter vectors and matrices
Bvec <- c(b_1, b_2, b_3)
B <-diag(Bvec)
betavec <- c(beta_1, beta_2, beta_3) 
beta <-diag(betavec)
A <- c(a_1, a_2, a_3)
alpha <- c(alpha_1, alpha_2, alpha_3)

# specify the starting values for the shipment matrix (as a 9 by 1 column vector)
start <- c(6465, 6910, 3140, 0, 265322, 0,  0, 0, 44495)

# create negative of the welfare function with 9x1 vector of shipment variables as inputs
# this is done to accommodate the optim() functions which uses vectors and not matrices
# use the standard formula because the supply schedules intersect the vertical axis
welfare <- function(Xsh) {
  X = matrix(Xsh, 3, 3) # Reshape 9x1 vector into 3 x 3 matrix.
  Qs <- rowSums(X)
  Qd <- colSums(X)
  net_surplus <- -(t(A) %*% Qd - 0.5*t(Qd) %*% B %*% Qd - (t(alpha) %*% Qs + 0.5*t(Qs) %*% beta %*% Qs) - sum(T*X))
}

# test the welfare function by supplying the 9x1 vector of observed shipment values
test <- welfare(start)
 
# use the optim() function to choose shipment matrix (output is transposed)
solution <- optim( par=start, fn=welfare, lower=matrix(0,9,1), method="L-BFGS-B" )$par

trade <-matrix(solution, 3, 3)

# recover equilibrium quantity consumed in each country
Qd_e <- colSums(trade)

# generate prices by substituting demand quantities into the inverse demand functions
(price_e <- A - Bvec*Qd_e)

# compare with transportation costs

(price_e[2]-price_e[1]-T[1,2]) # Canada and the U.S.
(price_e[3]-price_e[1]-T[1,3]) # Canada and Japan