library(quantmod)

STK="NVDA"
STK=get(getSymbols(STK))
chartSeries(STK["2022-01-01::2022-05-31"], up.col = "red", dn.col = "green"
            ,theme = "white")

View(cbind(Op(STK["2022-01-01::2022-05-31"]),Cl(STK["2022-01-01::2022-05-31"]),
           Cl(STK["2022-01-01::2022-05-31"])-Op(STK["2022-01-01::2022-05-31"]),Cl(STK["2022-01-01::2022-05-31"])-lag(Cl(STK["2022-01-01::2022-05-31"]),1)))
bar=Cl(STK["2022-01-01::2022-05-31"])-Op(STK["2022-01-01::2022-05-31"])
rr=Cl(STK["2022-01-01::2022-05-31"])-lag(Cl(STK["2022-01-01::2022-05-31"]),1)
redBar = length(bar[bar>0])
greenBar = length(bar[bar<0])
up = length(rr[rr>0])
down = length(rr[rr<0])

# 1. 紅K條件下，上漲&下跌的機率
cat("紅K條件下，上漲的機率:",round((length(rr[rr>0] & bar[bar>0])/redBar)*100,2),"%",
      "下跌的機率:",round((length(rr[rr<0] & bar[bar>0])/redBar)*100,2),"%\n",

# 2. 綠K條件下，上漲&下跌的機率
      "綠K條件下，上漲的機率:",round((length(rr[rr>0] & bar[bar<0])/greenBar)*100,2),"%",
      "下跌的機率:",round((length(rr[rr<0] & bar[bar<0])/greenBar)*100,2),"%\n",

# 3. 上漲條件下，紅K&綠K的機率
      "上漲條件下，紅K的機率:",round((length(rr[rr>0] & bar[bar>0])/up)*100,2),"%",
      "綠K的機率:",round((length(rr[rr>0] & bar[bar<0])/up)*100,2),"%\n",

# 4. 下跌條件下，紅K&綠K的機率
      "下跌條件下，紅K的機率:",round((length(rr[rr<0] & bar[bar>0])/down)*100,2),"%",
      "綠K的機率:",round((length(rr[rr<0] & bar[bar<0])/down)*100,2))

