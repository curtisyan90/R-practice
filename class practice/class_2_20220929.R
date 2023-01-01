#install.packages("quantmod")

library(quantmod)

getSymbols("NFLX")
View(NFLX)
chartSeries(NFLX)

STK="NVDA"
STK=get(getSymbols(STK))
chartSeries(STK["2021-12-31::2022-05-31"], up.col = "gray", dn.col = "black"
            ,theme = "white")

View(cbind(Op(STK),Cl(STK)))

View(lag(Cl(STK["2021-12-31::2022-05-31"]),1))
