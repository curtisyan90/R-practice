# install.packages("readr")

rm(list = ls())
library(readr)
library(quantmod)

setwd("/Users/curtis/Downloads/202210")
dir()

TX=read_csv("Daily_2022_10_19.csv", col_names=FALSE, skip=1)

colnames(TX) = c("Date", "Symbol", "Delivery", "Time", "Price", "Volume", "", "", "")
TX=TX[c(-7,-8,-9)]
#View(TX)
Row = which(TX$Symbol=="TX") # find row where symbol==TX
TXData = TX[Row,]
#View(TXData)

TXHotData=TXData[TXData$Delivery=="202210",]
#View(TXHotData)

(timeChar=paste(TXHotData$Date,TXHotData$Time))
(timeVector=strptime(timeChar, "%Y%m%d %H%M%S", tz=Sys.timezone()))
#View(TXHotData)

TSdata=xts(TXHotData[,5:6], as.POSIXct(timeVector))
View(TSdata)

chartSeries(TSdata)

TX_10s=to.period(TSdata, "seconds", 10)
chartSeries(TX_10s)

TX_mins = to.period(TSdata,"minutes",10)
chartSeries(TX_mins)
