rm(list=ls())
library(quantmod)
STK=get(getSymbols("NVDA"))
STK=as.matrix(na.omit(STK)["2015-01-01::2021-12-31"])
##################原策略
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
# plot(cumsum(PL1),type="l",col="red",lwd=2)
##################加濾網
PL2=setNames(numeric(nrow(STK)),rownames(STK))
long = 0
m=6
while ( m < nrow(STK)){
  if (Cl(STK)[m]>= max(Hi(STK)[(m-5):(m-1)])
      && sd(Cl(STK)[(m-4):(m-1)])<1.5){
    long=as.numeric(Cl(STK)[m])
    while( Cl(STK)[m]>min(Lo(STK)[(m-4):(m-1)])
           && m < nrow(STK)){m=m+1}
    PL2[m]=as.numeric(Cl(STK)[m])-long
  }
  m=m+1
}
# plot(cumsum(PL2),type="l",col="blue",lwd=2)
yRange=range(cumsum(PL1),cumsum(PL2))
plot(cumsum(PL1),type="l",col="red",lwd=2,ylim = yRange)
par(new=T)
plot(cumsum(PL2),type="l",col="blue",lwd=2,ylim = yRange)
#############
(wr=length(PL2[PL2>0])/length(PL2[PL2!=0]))
(odds=(mean(PL2[PL2>0]))/abs(mean(PL2[PL2<0])))
(EV=wr*odds+(1-wr)*(-1))
(PF=sum(PL2[PL2>0])/abs(sum(PL2[PL2<0])))

(DD=cumsum(PL2)-cummax(cumsum(PL2)))
(MDD=min(DD))

