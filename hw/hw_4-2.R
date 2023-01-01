rm(list = ls())
library(quantmod)
library(showtext)

STK="2303.TW"
STK=get(getSymbols(STK))
STK=na.omit(STK["2022-01-01::2022-10-26"])

STK=as.matrix(STK)
class(STK)
numeric(nrow(STK))
profit=setNames(numeric(nrow(STK)),rownames(STK))

for (m in rownames(STK)){
  profit[[m]]=STK[m,4]-STK[m,1]
}

plot(cumsum(profit), main="Yahoo 資料", type="l", col ="red", lwd=2)
abline(h=0, col="green")
cumsum(profit)[198]

