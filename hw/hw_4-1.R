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
