rm(list = ls())
library(quantmod)

STK="ORCL"
STK=get(getSymbols(STK))
#STK=na.omit(STK) # 忽略NA
STK=to.weekly(na.omit(STK["2010-01-01::2022-05-31"]))
#chartSeries(STK)
View(STK)

pl=Cl(STK)-Op(STK)
plot(cumsum(pl))

class(pl) # class to display type
class(STK)
View(pl)
STK=as.matrix(STK)
class(STK)
View(STK)
numeric(nrow(STK))
profit=setNames(numeric(nrow(STK)),rownames(STK))
# nrow(STK) get num of STK
# numeric(nrow(STK)) create numeric-> table?? 
# setNames(numeric(nrow(STK)),rownames(STK)) set col names of numeric by rownames

for (m in rownames(STK)){
  profit[[m]]=STK[m,4]-STK[m,1]
}
profit


plot(cumsum(profit), type="l", col ="red", lwd=2)
abline(h=0, col="green")
