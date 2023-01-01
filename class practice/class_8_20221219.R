rm(list=ls())
library(quantmod)
STK=get(getSymbols("NVDA"))
STK=as.matrix(na.omit(STK)["2015-01-01::2021-12-31"])
# chartSeries(STK)
#############
PL=Cl(STK)-Op(STK)
PL=as.numeric(PL)
# plot(cumsum(PL),type="l")
PZ=rep(0,length(PL))
for (m in 2:length(PL)){
  if (PL[m-1]<0){PZ[m]=2*PZ[m-1]}
  if (PL[m-1]>=0){PZ[m]=1}
}
# View(cbind(PL,PZ,PL*PZ))

########
m=6
PL1=setNames(numeric(nrow(STK)),rownames(STK))
while ( m < nrow(STK)){
  if (Cl(STK)[m]>= max(Hi(STK)[(m-5):(m-1)])){
    long=as.numeric(Cl(STK)[m])
    while( Cl(STK)[m]>min(Lo(STK)[(m-4):(m-1)])
           && m < nrow(STK)){m=m+1}
    PL1[m]=as.numeric(Cl(STK)[m])-long
  }
  m=m+1
}
###########################333
yRange=range(cumsum(PL1),cumsum(PZ*PL))
plot(cumsum(PL1),type="l",col="blue",ylim=yRange)
par(new=T)
plot(cumsum(PZ*PL),type="l",col="red",ylim=yRange)
# plot(PZ,type="h")
