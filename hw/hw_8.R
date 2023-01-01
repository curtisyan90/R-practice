rm(list = ls())
library(quantmod)
library(showtext)

STK="MU"
STK=get(getSymbols(STK))
# STK=to.weekly(na.omit(STK["2010-01-01::2022-05-31"]))
STK=na.omit(STK["2010-01-01::2022-05-31"])
chartSeries(STK)
STK=na.omit(STK)


(PL=setNames(rep(0,length(Cl(STK))),time(STK)))

m=6
count=0
while ( m < nrow(STK)){
  if (Cl(STK[m])>=max(Hi(STK[(m-5):(m-1)]))){
    long=as.numeric(Cl(STK)[m+1]) # 進場開盤價
    count=count+1
    while( (Cl(STK[m])>min(Lo(STK[(m-3):(m-1)]))) && m <nrow(STK)-1){m=m+1} # cl>ma | ma1>ma2
    PL[m]=as.numeric(Op(STK)[m+1])-long # 出場開盤價
  }
  m=m+1  
}
(PL)
plot(cumsum(PL),type="l",col="red",lwd=2)

# ***********************************

(wr=length(PL[PL>0])/length(PL[PL!=0]))
(odds=(mean(PL[PL>0]))/abs(mean(PL[PL<0])))
(EV=wr*odds+(1-wr)*(-1))
(PF=sum(PL[PL>0])/abs(sum(PL[PL<0])))

(DD=cumsum(PL)-cummax(cumsum(PL)))
(MDD=min(DD))
(Yrange=range(cumsum(PL), DD))

plot(cumsum(PL), type="l", lwd=2, col="red", ylim = Yrange)
# points(which(DD==0), cumsum(PL)[which(DD==0)], pch=10, col ="red")
# 
# par(new=T)
# plot(DD, type = "h", col="darkgreen", ylim = Yrange
#      , ylab = "", xlab = "", xaxt="n")

# axis(1, 1:length(PL), as.Date("2022-01-01")+1*(0:length(PL)-1))
# axis(1, 1:length(PL), as.Date("2010-01-01")+7*(1:length(PL)))
