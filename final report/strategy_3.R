rm(list = ls())
library(quantmod)
STK = get(getSymbols("2330.TW"))
# STK = as.matrix(na.omit(STK))
STK = as.matrix(na.omit(STK)["2012-01-01::2022-12-30"])
#Draw MA on K chart
ma1_len = 50
ma2_len = 100
ma3_len = 150
chartSeries(STK)
par(new = T)
addSMA(ma1_len)
addSMA(ma2_len)
addSMA(ma3_len)
##################Strategy
colnames(STK) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
ma1 <- SMA(Cl(STK), ma1_len)
ma2 <- SMA(Cl(STK), ma2_len)
ma3 <- SMA(Cl(STK), ma3_len)
ATR<-ATR(STK[,c("High","Low","Close")], 14)
atr <- ATR[,2]
tr <- ATR[,1]
fee <- 0.006
longStop <- (1-0.03)
longDone <- (1+0.03)

m = 101  # start K
c <- 0
PL = setNames(numeric(nrow(STK)), rownames(STK))
while (m < nrow(STK)) {
  if (# tr<atr
    (tr[m]<atr[m] && tr[m-1]<atr[m-1] && tr[m-2]<atr[m-2]) &&
    # ((Cl(STK)[m]>Op(STK)[m] && Cl(STK)[m-1]>Op(STK)[m-1]) || 
    #  (Cl(STK)[m]>Op(STK)[m] && Cl(STK)[m-2]>Op(STK)[m-2]) ||
    #  (Cl(STK)[m-1]>Op(STK)[m-1] && Cl(STK)[m-2]>Op(STK)[m-2])) &&
    (ma2[m]>ma2[m-1])
    ) {
    long = as.numeric(Cl(STK)[m])
    highest <- Hi(STK)[m]
    stoppiont <- min(Lo(STK)[m], Lo(STK)[m-1], Lo(STK)[m-2])*longStop
    c <- c+1
    while ((Cl(STK)[m]<highest*longStop || Cl(STK)[m]<stoppiont) == FALSE && 
           m < nrow(STK)-2) {
      if (Hi(STK)[m] > highest){
        highest <- Hi(STK)[m]
      }
      m = m + 1
    }
    PL[m] = (as.numeric(Cl(STK)[m]) - long)*(1-fee)
    
  }
  m = m + 1
}

################################################################################
(wr = length(PL[PL > 0]) / length(PL[PL != 0]))  # 勝率
(odds = (mean(PL[PL > 0])) / abs(mean(PL[PL < 0])))  # 賺賠比
(EV = wr * odds + (1 - wr) * (-1))  # 期望值
(PF = sum(PL[PL > 0]) / abs(sum(PL[PL < 0])))  # 獲利因子
(DD = cumsum(PL) - cummax(cumsum(PL)))  # 回撤
(MDD = min(DD))  # 最大回撤
(DDper=DD/cummax(cumsum(PL)))
which(DDper==min(DDper))
tail(sort(diff(which(DD==0))),5)
(income <- sum(PL))  # 總報酬
################################################################################
yRange = range(cumsum(PL), DD)
plot(cumsum(PL), type = "l", col = "red", lwd = 2, ylim = yRange
     , ylab = "PL", xlab = "Date", xaxt="n")
par(new = T)
plot(DD, type = "h", col = "blue", lwd = 2, ylim = yRange
     , ylab = "", xlab = "", xaxt="n")
par(new = T)
plot(DD, type = "h", col = "blue", lwd = 2, ylim = yRange
     , ylab = "", xlab = "", xaxt="n")

points(which(DD==0), cumsum(PL)[which(DD==0)], pch=10, col ="red")

axis(1, 1:length(PL), as.Date("1980-12-12")+1*(1:length(PL)))
# axis(1, 1:(length(PL)/30),format(as.Date("1980-12", "%Y-%m")+1*(1:length(PL)/30), format = "%Y-%m"), "%Y-%m")

