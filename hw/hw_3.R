library(quantmod)
library(showtext)
rm(list = ls())

STK="ORCL"
STK=get(getSymbols(STK))
STK=to.weekly(na.omit(STK["2010-01-01::2022-05-31"]))
STK=cbind(STK, lag(Op(STK),1), lag(Cl(STK),1))
STK=as.matrix(STK)
class(STK)
profit=profit1=profit2=profit3=profit4=profit5=setNames(numeric(nrow(STK)),rownames(STK))
STK<-STK[-1,]

for (m in rownames(STK)){
  profit[[m]]=STK[m,4]-STK[m,1]
  
  if ((STK[m,1] / STK[m,8])>=1.01 ){ # op > lag cl >= 1%
    profit1[[m]]=STK[m,4]-STK[m,1]
  }
  
  if (STK[m,1] > STK[m,8] ){ # op > lag cl
    profit2[[m]]=STK[m,4]-STK[m,1]
  }
  
  if (STK[m,1] < STK[m,8] ){ # op < lag cl
    profit3[[m]]=STK[m,1]-STK[m,4]
  }

  if ((STK[m,8] - STK[m,7])>0 ){ # lag k = red
    profit4[[m]]=STK[m,4]-STK[m,1]
  }

  if ((STK[m,8] - STK[m,7])<0 ){ # lag k = green
    profit5[[m]]=STK[m,1]-STK[m,4]
  }
}
plot(cumsum(profit), main="原始策略", type="l", col ="red", lwd=2)
abline(h=0, col="green")
plot(cumsum(profit1), main="1.開高幅度>1%，開盤買進 收盤賣出", type="l", col ="red", lwd=2)
abline(h=0, col="green")
plot(cumsum(profit2), main="2.開高，開盤買進 收盤賣出", type="l", col ="red", lwd=2)
abline(h=0, col="green")
plot(cumsum(profit3), main="3.開低，開盤賣出 收盤買進", type="l", col ="red", lwd=2)
abline(h=0, col="green")
plot(cumsum(profit4), main="4.前一根為紅K，開盤買進 收盤賣出", type="l", col ="red", lwd=2)
abline(h=0, col="green")
plot(cumsum(profit5), main="5.前一根為綠K，開盤賣出 收盤買進", type="l", col ="red", lwd=2)
abline(h=0, col="green")
print(cumsum(profit)[648])

