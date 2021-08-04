# this program illustrates how R can be used to maximize a quadratic function

a11 <- -0.5
a12 <- 0.4
a21 <- 0
a22 <- -0.75
b1 <- 2
b2 <- 3
cons <- 0

# the optim tool minimizes so we want to minimize the negative of this bivariate quadratic function
quadfunc <-function(x) { 
  y <- -1*(a11*x[1]*x[1] + a12*x[1]*x[2] + a21*x[2]*x[1] + a22*x[2]*x[2] + b1*x[1] + b2*x[2] + cons)
  return(y)
}
 
quadfunc(c(2,2))

# Choose the pair of x values to maximize the function: $par extracts only the argmax and nothing else
optim( c(2,2), quadfunc, lower=-Inf, upper=Inf)

# Repeat but now save the optimized values of x1 and x2.
optim_x <- optim( c(2,2), quadfunc, lower=-Inf, upper=Inf)$par

# repeat but use matrix multiplication:  %*%. t()
# xx <- matrix(c(1, 2, 3, 4, 5, 6), nrow=2, ncol=3, byrow=T)
A <- matrix(c(a11,a12,a21,a22), nrow=2, ncol=2)
B <- matrix(c(b1,b2), nrow=2, ncol=1)
quadfunc2 <-function(x) { 
  y2 <- -t(x) %*% A %*% x-t(B) %*% x+cons
  return(y2)
}
quadfunc2(c(2,2))

# Repeat but now add two bivariates together and enter four x variables as a matrix
quadfunc3 <-function(x) { 
  y3 <- -1*(a11*x[1,1]*x[1,1] + a12*x[1,1]*x[1,2] + a21*x[1,2]*x[1,1] + a22*x[1,2]*x[1,2] + b1*x[1,1] + b2*x[1,2] + cons)-1*(a11*x[2,1]*x[2,1] + a12*x[2,1]*x[2,2] + a21*x[2,2]*x[2,1] + a22*x[2,2]*x[2,2] + b1*x[2,1] + b2*x[2,2] + cons)
  return(y3)
}

quadfunc3(cbind(c(2,2),c(2,2)))


 