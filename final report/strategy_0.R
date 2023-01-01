rm(list = ls())
library(quantmod)
STK = get(getSymbols("AAPL"))
# STK = as.matrix(na.omit(STK))
STK = as.matrix(na.omit(STK)["1980-12-12::2022-12-30"])
fee <- 0.006
m = 1  # start K


######## buy open sell close
PL = setNames(numeric(nrow(STK)), rownames(STK))
while (m < nrow(STK)) {
  long = as.numeric(Op(STK)[m])
  PL[m] = (as.numeric(Cl(STK)[m]) - long) * (1 - fee)
  m = m + 1
}
# plot(cumsum(PL),type="l",col="red",lwd=2)

################## buy and hold
long = as.numeric(Op(STK)[1])
bnh = (as.numeric(Cl(STK)[nrow(STK)]) - long) * (1 - fee)

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

