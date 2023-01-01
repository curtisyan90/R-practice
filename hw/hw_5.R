rm(list = ls())
library(quantmod)
library(showtext)

STK="ORCL"
STK=get(getSymbols(STK))
STK=to.weekly(na.omit(STK["2010-01-01::2022-05-31"]))

STK=as.matrix(STK)
class(STK)
numeric(nrow(STK))
profit=setNames(numeric(nrow(STK)),rownames(STK))

for (m in rownames(STK)){
  profit[[m]]=STK[m,4]-STK[m,1]
}

plot(cumsum(profit), main="Yahoo 資料", type="l", col ="red", lwd=2)
abline(h=0, col="green")
cumsum(profit)[648]

# ***********************************

(wr=length(profit[profit>0])/length(profit[profit!=0]))
(odds=(mean(profit[profit>0]))/abs(mean(profit[profit<0])))
(EV=wr*odds+(1-wr)*(-1))
(PF=sum(profit[profit>0])/abs(sum(profit[profit<0])))

(DD=cumsum(profit)-cummax(cumsum(profit)))
(MDD=min(DD))
(Yrange=range(cumsum(profit), DD))

plot(cumsum(profit), type="l", lwd=2, col="blue", ylim = Yrange
     , ylab = "PL", xlab = "Date", xaxt="n")
points(which(DD==0), cumsum(profit)[which(DD==0)], pch=10, col ="red")

par(new=T)
plot(DD, type = "h", col="darkgreen", ylim = Yrange
     , ylab = "", xlab = "", xaxt="n")

# axis(1, 1:length(profit), as.Date("2022-01-01")+1*(0:length(profit)-1))
axis(1, 1:length(profit), as.Date("2010-01-01")+7*(1:length(profit)))

