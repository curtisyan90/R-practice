rm(list = ls())
library(quantmod)
library(showtext)

STK="ORCL"
STK=get(getSymbols(STK))
STK=to.weekly(na.omit(STK["2010-01-01::2022-05-31"]))
chartSeries(STK)
STK=na.omit(STK)

ma=14
#View(cbind(Cl(STK),SMA(Cl(STK),ma),row))

plot(SMA(Cl(STK)), ma)
