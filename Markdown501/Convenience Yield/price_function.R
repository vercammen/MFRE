# this code specifies the function which delivers the 24 delta values to the main program.
 
get_delta <- function(v) {
  a <- v[1]
  b <- v[2]
  m0 <- v[3]
  m1 <- v[4]
  S0 <- v[5]
  H1 <- v[6]
  S8 <- v[7]
  
  Z <- m0/b
  m <- m1/b
  
  omeg0 <- rep(0, times = 8)
  omeg1 <- rep(0, times = 8)
  omeg2 <- rep(0, times = 8)
  gam0 <- rep(0, times = 8)
  gam1 <- rep(0, times = 8)
  gam2 <- rep(0, times = 8)
  Omeg0 <- rep(0, times = 8)
  Omeg1 <- rep(0, times = 8)
  Omeg2 <- rep(0, times = 8)
  Gam0 <- rep(0, times = 8)
  Gam1 <- rep(0, times = 8)
  Gam2 <- rep(0, times = 8)
  del0 <- rep(0, times = 8)
  del1 <- rep(0, times = 8)
  del2 <- rep(0, times = 8)
     
gam0[1] <- S0+H1
omeg0[2] <- -(Z+m*(S0+H1))
gam0[2] <- S0+H1-omeg0[2]
omeg0[3] <- omeg0[2]-Z-m*gam0[2]
gam0[3] <- gam0[2]-omeg0[3]
omeg0[4] <- omeg0[3]-Z-m*gam0[3] 
gam0[4] <- gam0[3]-omeg0[4]
omeg0[6] <- -Z
gam0[6] <- -omeg0[6]
omeg0[7] <- omeg0[6]-Z-m*gam0[6]
gam0[7] <- gam0[6]-omeg0[7]
omeg0[8] <- omeg0[7]-Z-m*gam0[7]
gam0[8] <- gam0[7]-omeg0[8]

gam1[1] <- -1
omeg1[2] <- 1+m
gam1[2] <- -(1+omeg1[2])
omeg1[3] <- omeg1[2]-m*gam1[2]
gam1[3] <- gam1[2]-omeg1[3]
omeg1[4] <- omeg1[3]-m*gam1[3]
gam1[4] <- gam1[3]-omeg1[4]
gam1[5] <- -1
omeg1[6] <- 1+m
gam1[6] <- -(1+omeg1[6])
omeg1[7] <- omeg1[6]-m*gam1[6]
gam1[7] <- gam1[6]-omeg1[7]
omeg1[8] <- omeg1[7]-m*gam1[7]
gam1[8] <- gam1[7]-omeg1[8]

gam2[5] <- 1
omeg2[6] <- -m
gam2[6] <- 1-omeg2[6]
omeg2[7] <- omeg2[6]-m*gam2[6]
gam2[7] <- gam2[6]-omeg2[7]
omeg2[8] <- omeg2[7]-m*gam2[7]
gam2[8] <- gam2[7]-omeg2[8]

Gam04_num <- omeg0[4]*gam1[4]-omeg1[4]*gam0[4]-Z*gam1[4]+gam1[4]*gam0[8]/gam1[8]
Gam04_den <- omeg1[4]-m*gam1[4]+gam1[4]*gam2[8]/gam1[8]
Gam0[4] <- -Gam04_num/Gam04_den
Gam14_num <- gam1[4]*gam2[8]
Gam14_den <- gam1[4]*gam2[8]+gam1[8]*(omeg1[4]-m*gam1[4])
Gam1[4] <- -Gam14_num/Gam14_den
Gam2[4] <- gam1[4]/Gam14_den

Omeg0[1] <- -gam0[4]/gam1[4]+Gam0[4]/gam1[4]
Omeg0[2] <- omeg0[2]+omeg1[2]*Omeg0[1]
Omeg0[3] <- omeg0[3]+omeg1[3]*Omeg0[1]
Omeg0[4] <- omeg0[4]+omeg1[4]*Omeg0[1]
Omeg0[5] <- (omeg0[4]*gam1[4]-omeg1[4]*gam0[4]-Z*gam1[4])/gam1[4]+(omeg1[4]-m*gam1[4])*Gam0[4]/gam1[4]
Omeg0[6] <- omeg0[6]+omeg1[6]*Omeg0[5]+omeg2[6]*Gam0[4]
Omeg0[7] <- omeg0[7]+omeg1[7]*Omeg0[5]+omeg2[7]*Gam0[4]
Omeg0[8] <- omeg0[8]+omeg1[8]*Omeg0[5]+omeg2[8]*Gam0[4]
  
Gam0[1] <- S0+H1-Omeg0[1]
Gam0[2] <- gam0[2]+gam1[2]*Omeg0[1]
Gam0[3] <- gam0[3]+gam1[3]*Omeg0[1]
Gam0[5] <- Gam0[4]-Omeg0[5]
Gam0[6] <- gam0[6]+gam1[6]*Omeg0[5]+gam2[6]*Gam0[4]
Gam0[7] <- gam0[7]+gam1[7]*Omeg0[5]+gam2[7]*Gam0[4]
Gam0[8] <- gam0[8]+gam1[8]*Omeg0[5]+gam2[8]*Gam0[4]

Omeg1[1] <- Gam1[4]/gam1[4] 
Omeg1[2] <-omeg1[2]*Omeg1[1]
Omeg1[3] <-omeg1[3]*Omeg1[1]
Omeg1[4] <-omeg1[4]*Omeg1[1]
Omeg1[5] <-(omeg1[4]-m*gam1[4])*Gam1[4]/gam1[4]
Omeg1[6] <- omeg1[6]*Omeg1[5]+omeg2[6]*(Gam1[4]+1)
Omeg1[7] <- omeg1[7]*Omeg1[5]+omeg2[7]*(Gam1[4]+1)
Omeg1[8] <- omeg1[8]*Omeg1[5]+omeg2[8]*(Gam1[4]+1)

Gam1[1] <- -Omeg1[1]
Gam1[2] <- gam1[2]*Omeg1[1]
Gam1[3] <- gam1[3]*Omeg1[1]
Gam1[5] <- Gam1[4]+1-Omeg1[5]
Gam1[6] <- gam1[6]*Omeg1[5]+gam2[6]*(Gam1[4]+1)
Gam1[7] <- gam1[7]*Omeg1[5]+gam2[7]*(Gam1[4]+1)
Gam1[8] <- gam1[8]*Omeg1[5]+gam2[8]*(Gam1[4]+1)

Omeg2[1] <- Gam2[4]/gam1[4]
Omeg2[2] <- omeg1[2]*Omeg2[1]
Omeg2[3] <- omeg1[3]*Omeg2[1]
Omeg2[4] <- omeg1[4]*Omeg2[1]
Omeg2[5] <- (omeg1[4]-m*gam1[4])*Gam2[4]/gam1[4]
Omeg2[6] <- omeg1[6]*Omeg2[5]+omeg2[6]*Gam2[4]
Omeg2[7] <- omeg1[7]*Omeg2[5]+omeg2[7]*Gam2[4]
Omeg2[8] <- omeg1[8]*Omeg2[5]+omeg2[8]*Gam2[4]

Gam2[1] <- -Omeg2[1]
Gam2[2] <- gam1[2]*Omeg2[1]
Gam2[3] <- gam1[3]*Omeg2[1]
Gam2[5] <- Gam2[4]-Omeg2[5]
Gam2[6] <- gam1[6]*Omeg2[5]+gam2[6]*Gam2[4]
Gam2[7] <- gam1[7]*Omeg2[5]+gam2[7]*Gam2[4]
Gam2[8] <- gam1[8]*Omeg2[5]+gam2[8]*Gam2[4]

del0[1] <- a-b*Omeg0[1]-b*Omeg2[1]*S0
del0[2] <- a-b*Omeg0[2]-b*Omeg2[2]*S0
del0[3] <- a-b*Omeg0[3]-b*Omeg2[3]*S0
del0[4] <- a-b*Omeg0[4]-b*Omeg2[4]*S0
del0[5] <- a-b*Omeg0[5]-b*Omeg2[5]*S0
del0[6] <- a-b*Omeg0[6]-b*Omeg2[6]*S0
del0[7] <- a-b*Omeg0[7]-b*Omeg2[7]*S0
del0[8] <- a-b*Omeg0[8]-b*Omeg2[8]*S0

del1[1] <- -b*Omeg1[1]
del1[2] <- -b*Omeg1[2]
del1[3] <- -b*Omeg1[3]
del1[4] <- -b*Omeg1[4]
del1[5] <- -b*Omeg1[5]
del1[6] <- -b*Omeg1[6]
del1[7] <- -b*Omeg1[7]
del1[8] <- -b*Omeg1[8]

del2[1] <- -b*Omeg2[1]
del2[2] <- -b*Omeg2[2]
del2[3] <- -b*Omeg2[3]
del2[4] <- -b*Omeg2[4]
del2[5] <- -b*Omeg2[5]
del2[6] <- -b*Omeg2[6]
del2[7] <- -b*Omeg2[7]
del2[8] <- -b*Omeg2[8]

del <- cbind(del0,del1,del2)
del

}

 
 
 


