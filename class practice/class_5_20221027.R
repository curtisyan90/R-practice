rm(list = ls())
library(quantmod)
library(showtext)
showtext.auto(enable = TRUE)

setwd("/Users/curtis/Downloads/202210/2303")
dir()

STKB<-""
for (m in dir()){
  STK=read.csv(m, skip=2, header=FALSE)
  STK=STK[(-nrow(STK)+4):-nrow(STK),]
  STKB=rbind(STKB,STK)
}
STK=STKB[-1,]

(time = gsub("111", "2022", STK[,1]))
(timeVector=strptime(time, "%Y/%m/%d", tz=Sys.timezone()))

STK[,2]=as.numeric(gsub(",", "", STK[,2]))

STK=cbind(as.numeric(STK[,4]), as.numeric(STK[,5])
          , as.numeric(STK[,6]), as.numeric(STK[,7])
          , as.numeric(STK[,2]))
STK=xts(STK, as.POSIXct(timeVector))
colnames(STK)=c("Open", "High", "Low", "Close", "Volume")

STK=as.matrix(STK)
class(STK)
numeric(nrow(STK))
profit=setNames(numeric(nrow(STK)),rownames(STK))

for (m in rownames(STK)){
  profit[[m]]=STK[m,4]-STK[m,1]
}

plot(cumsum(profit), main="證交所資料", type="l", col ="red", lwd=2)
abline(h=0, col="green")

cumsum(profit)[198]
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
axis(1, 1:length(profit), as.Date("2022-01-01")+1*(1:length(profit)))

