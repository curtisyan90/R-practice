rm(list = ls())
library(quantmod)

setwd("/Users/curtis/Downloads/202210")
dir()

STK=read.csv("STOCK_DAY_2330_202209.csv", skip=2, header=FALSE)

STK=STK[(-nrow(STK)+3):-nrow(STK),]

(time = gsub("111", "2022", STK[,1]))
(timeVector=strptime(time, "%Y/%m/%d", tz=Sys.timezone()))

STK[,2]=as.numeric(gsub(",", "", STK[,2]))

STK=cbind(as.numeric(STK[,4]), as.numeric(STK[,5])
          , as.numeric(STK[,6]), as.numeric(STK[,7])
          , as.numeric(STK[,2]))
STK=xts(STK, as.POSIXct(timeVector))
colnames(STK)=c("Open", "High", "Low", "Close", "Volume")
chartSeries(STK)

# View(STK)
# 
# d=as.Date("2022-10-01")
# dd=Sys.Date()  # current date
# dd-d
# as.Date(d)+5
#cbind->column ; rbind->row
