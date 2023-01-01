rm(list=ls())
library(quantmod)
STK=get(getSymbols("MU"))
STK=na.omit(STK["2010-01-01::2022-05-31"])
STK=as.matrix(STK)
m=6
PL=setNames(numeric(nrow(STK)),rownames(STK))
while ( m < nrow(STK)){
  if (Cl(STK)[m]>= max(Hi(STK)[(m-5):(m-1)])){
    long=as.numeric(Cl(STK)[m])
    while( Cl(STK)[m]>min(Lo(STK)[(m-3):(m-1)]) 
           && m < nrow(STK)-1){m=m+1}
    PL[m]=as.numeric(Op(STK)[m+1])-long
  }
  m=m+1  
}
plot(cumsum(PL),type="l",col="red",lwd=2)

#######################################################
# 
# PL2=setNames(numeric(nrow(STK)),rownames(STK))
# pz=0
# for ( m in 4:nrow(STK)){
#   if (Cl(STK)[m] >= max(Hi(STK)[(m - 3):(m - 1)]) &&
#       m < nrow(STK) - 1) {
#     long = long + as.numeric(Cl(STK)[m])
#     pz = pz + 1
#   } else{
#     
#       ( Cl(STK)[m]>min(Lo(STK)[(m-3):(m-1)]) 
#            && m < nrow(STK)-1){m=m+1}
#     PL2[m]=as.numeric(Op(STK)[m+1])-long
#     }
# }
# plot(cumsum(PL2),type="l",col="blue",lwd=2)

