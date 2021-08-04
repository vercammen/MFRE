# this program replicates the case study in Chapter 2 of my textbook

# <CTRL> L to clear screen
rm(list = ls())
# specify intercept and slopes of supply and demand schedules for five countries
a1 <- 1.746  # Mexico
a2 <- 0.443  # The U.S.
a3 <- 1.026  # Canada
a4 <- 0.852  # The EU
a5 <- 0.561  # Latin America
alpha1 <- -0.506
alpha2 <- -0.256
alpha3 <- -0.426
alpha4 <- -1.067  # base case value = -1.067; What-if value = -0.5
alpha5 <- -0.661
b1 <- 1.156
b2 <- 0.022
b3 <- 1.162
b4 <- 0.044
b5 <- 0.072
beta1 <- 0.292
beta2 <- 0.042
beta3 <- 1.180
beta4 <- 0.086
beta5 <- 0.118

# create parameter vectors and matrices
Bvec <- c(b1, b2, b3, b4, b5)
B <-diag(Bvec)
betavec <- c(beta1, beta2, beta3, beta4, beta5) 
beta <-diag(betavec)
A <- c(a1, a2, a3, a4, a5)
alpha <- c(alpha1, alpha2, alpha3, alpha4, alpha5)

# create transportation cost matrix
T <- matrix(c(0, 0.01170, 0.01933, 0.03111, 0.03238, 0.01170, 0, 0.00844, 0.02140, 0.02841, 0.01933, 0.00844, 0, 0.02129, 0.03289, 0.03111, 0.02140, 0.02129, 0, 0.02747, 0.03238, 0.02841, 0.03289, 0.02747, 0), nrow=5, ncol=5)

# create shipment matrix
#X <- matrix(c(1.3622, 0, 0, 0, 0, 0.9604, 10.4596, 0, 0, 0.3952, 0, 0, 0.52, 0, 0.2014, 0, 0, 0, 14.5298, 0.6788, 0, 0, 0, 0, 5.6422), nrow = 5, ncol = 5)
 
# create calculate welfare function with 15x1 vector of shipment variables as inputs
# this is done to accommodate the optim() functions which uses vectors and not matrices
welfare <- function(Xsh) {
  X = matrix(Xsh, 5, 5) # Reshape 15x1 vector into 5 x 5 matrix.
  Qs <- rowSums(X)
  Qd <- colSums(X)
  unit <- rep(1,5)
  cons <- c(alpha1*alpha1/beta1, alpha2*alpha2/beta2, alpha3*alpha3/beta3, alpha4*alpha4/beta4, alpha5*alpha5/beta5)
  net_surplus <- -(t(A) %*% Qd - 0.5*t(Qd) %*% B %*% Qd - (t(alpha) %*% Qs + 0.5*t(Qs) %*% beta %*% Qs  + 0.5*t(cons) %*% unit ) - sum(T*X))
}

# use the starting values from chapter 2 of the textbook.

start <- c(1.357, 0,  0, 0,  0, 0,  10.331, 0,  0, 0,  0, 0,  0.521, 0,  0, 0,  0, 0,  14.478, 0,  0, 0,  0, 0,  5.322)

# test the welfare function by supplying a 25x1 vector of shipment values
test <- welfare(start)
 
# use the optim() function to choose shipment matrix (output is transposed)
solution <- optim( par=start, fn=welfare, lower=matrix(0,25,1), method="L-BFGS-B" )$par

trade <-matrix(solution, 5, 5)

# recover equilibrium quantity consumed in each country
Qd_e <- colSums(trade)

# generate prices by substituting demand quantities into the inverse demand functions
(price_e <- 5000*(A - Bvec*Qd_e))

# generate non scaled transportation costs
(Tns <- 5000*T)