rm(list=ls())
library(quantmod)
STK=get(getSymbols("NVDA"))
STK=as.matrix(na.omit(STK))
m=4
PL1=setNames(numeric(nrow(STK)),rownames(STK))
while ( m < nrow(STK)){
  if (Cl(STK)[m]>= max(Hi(STK)[(m-3):(m-1)])){
    long=as.numeric(Cl(STK)[m])
    while( Cl(STK)[m]>min(Lo(STK)[(m-3):(m-1)])
           && m < nrow(STK)){m=m+1}
    PL1[m]=as.numeric(Cl(STK)[m])-long
  }
  m=m+1
}
plot(cumsum(PL1),type="l",col="red",lwd=2)
##################加碼
PL2=setNames(numeric(nrow(STK)),rownames(STK))
PZ=0
long = 0
for ( m in  4:nrow(STK)){
  if (Cl(STK)[m]>= max(Hi(STK)[(m-3):(m-1)])  && m < nrow(STK)){
    long=long+as.numeric(Cl(STK)[m])
    PZ=PZ+1
    m=m+1}else{
      if (Cl(STK)[m]<= min(Lo(STK)[(m-3):(m-1)]) &&  m < nrow(STK)){
        PL2[m]=as.numeric(Cl(STK)[m])*PZ-long
        PZ=0
        long=0}
    }
}
yRange=range(cumsum(PL1),cumsum(PL2))
plot(cumsum(PL1),type="l",col="red",lwd=2,ylim = yRange)
par(new=T)
plot(cumsum(PL2),type="l",col="blue",lwd=2,ylim = yRange)