# https://comtrade.un.org/data/Doc/api/ex/r
# https://data.library.virginia.edu/getting-un-comtrade-data-with-r/

rm(list = ls())
#setwd("C:/Users/Jim/Dropbox/Git/Markdown501/Palm Oil")
setwd("C:/Users/vercamme/Dropbox/Git/Markdown501/Palm Oil") 
source("ComTrade_Function.R")
library(rjson)

# Extract annual imports (rg=1) of palm oil (cc = 151110) in .csv format for most recent year (ps=2020)
# for reporter (r) (Netherlands=528) and partners (p):
# Columbia=170,  Guatemala=320, Indonesia=360, Malaysia=458, World = 0

A <- get.Comtrade(r="528", p="0, 170, 320, 360, 458", ps="2020", 
                    rg=1, cc="151110")
dfA <- as.data.frame(A$data)
 
Imp.A <- dfA$rtTitle
Exp.A <- dfA$ptTitle
Comm.A <- dfA$cmdCode
Year.A <- dfA$yr
Type.A <- dfA$rgDesc
Unit.A <- "tonnes"
Weight.A <- as.numeric(dfA$TradeQuantity)/1000

dfAsum <- cbind(Imp.A, Exp.A, Comm.A, Year.A, Type.A, Unit.A, Weight.A)
dfAsum

# Extract imports (rg=1) and exports (rg=2) of palm oil (cc = 151110), refined palm oil (cc = 151190) in .csv format for most recent year (ps=2020)
# and biodiesel (cc = 3826) for reporter (r) (Netherlands=528) and partners (p) World = 0


B <- get.Comtrade(r="528", p="0", ps="2020", 
                  rg="1,2", cc="151110,151190,3826")
dfB <- as.data.frame(B$data)

Imp.B <- dfB$rtTitle
Exp.B <- dfB$ptTitle
Comm.B <- dfB$cmdCode
Year.B <- dfB$yr
Type.B <- dfB$rgDesc
Unit.B <- "tonnes"
Weight.B <- as.numeric(dfB$TradeQuantity)/1000

dfBsum <- cbind(Imp.B, Exp.B, Comm.B, Year.B, Type.B, Unit.B, Weight.B)
dfBsum









B <- get.Comtrade(r="528", p="0", ps="2020", 
                   rg="1,2", cc="151110,151190")
BJ <- as.data.frame(B$data)
BJ

dfB <- as.data.frame(do.call(rbind, B))


Imp.B <- dfB$Reporter
Exp.B <- dfB$Partner
Comm.B <- dfB$Commodity
Type.B <- dfB$Trade.Flow
Unit.B <- "Tonnes"
Weight.B <- dfB$Qty/1000

Comm.B[1] <- "Crude Palm"
Comm.B[2] <- "Refined Palm"
Comm.B[3] <- "Biodiesel"

dfBsum <- cbind(Imp.B, Exp.B, Comm.B, Type.B, Unit.B, Weight.B)
dfBsum

 

