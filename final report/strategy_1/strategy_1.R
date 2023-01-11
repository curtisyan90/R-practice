rm(list = ls())
library(quantmod)
STK <- get(getSymbols("AAPL"))
# STK = as.matrix(na.omit(STK))
STK <- as.matrix(na.omit(STK)["2012-01-01::2022-12-30"])
# Draw MA on K chart
ma1_len <- 50
ma2_len <- 100
chartSeries(STK)
par(new = T)
addSMA(ma1_len)
addSMA(ma2_len)
################## Strategy
colnames(STK) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
ma1 <- SMA(Cl(STK), ma1_len)
ma2 <- SMA(Cl(STK), ma2_len)
fee <- 0.0000229
longStop <- (1 - 0.03)

m <- 101 # start K
c <- 0
PL <- setNames(numeric(nrow(STK)), rownames(STK)) # position long
# OHLC
op <- Op(STK)
hi <- Hi(STK)
lo <- Lo(STK)
cl <- Cl(STK)

while (m < nrow(STK)) {
     if ( # close crossover ma1 & ma1>ma2
          ((cl[m - 1] <= ma1[m - 1] && cl[m] > ma1[m] && ma1[m] > ma2[m]) ||
               # close crossover ma1 & ma2
               (cl[m - 1] <= ma1[m - 1] && cl[m] > ma1[m] &&
                    cl[m - 1] <= ma2[m - 1] && cl[m] > ma2[m])) &&
                    # ma2 growth
                    ma2[m] > ma2[m - 1]) {
          long <- as.numeric(Cl(STK)[m]) # buy price
          c <- c + 1
          while (# 兩種寫法 same logic
                ## close < ma1
               (Cl(STK)[m] < ma1[m] ||
                 ## ma1 < ma2
                ma1[m] < ma2[m]) == FALSE &&
               
                # (Cl(STK)[m] >= ma1[m] || tr[m] <= atr[m]) &&
                # (Cl(STK)[m - 1] >= ma1[m-1] || Cl(STK)[m] >= Cl(STK)[m - 1]) && # nolint
                # (Cl(STK)[m] >= long * longStop)&&

                    m < nrow(STK) - 1) {
               m <- m + 1
          }
          PL[m] <- (as.numeric(Cl(STK)[m]) - long) * (1 - fee)
     }
     m <- m + 1
}

################################################################################
wr <- length(PL[PL > 0]) / length(PL[PL != 0]) # 勝率
odds <- (mean(PL[PL > 0])) / abs(mean(PL[PL < 0])) # 賺賠比
EV <- wr * odds + (1 - wr) * (-1) # 期望值
PF <- sum(PL[PL > 0]) / abs(sum(PL[PL < 0])) # 獲利因子
DD <- cumsum(PL) - cummax(cumsum(PL)) # 回撤
MDD <- min(DD) # 最大回撤
DDper <- DD / cummax(cumsum(PL))
# which(DDper==min(DDper))
tail(sort(diff(which(DD == 0))), 5)
income <- sum(PL) # 總報酬
################################################################################
yRange <- range(cumsum(PL), DD)
plot(cumsum(PL),
     type = "l", col = "red", lwd = 2, ylim = yRange, las = 1,
     ylab = "PL", xlab = "Date", xaxt = "n"
)
par(new = TRUE)
plot(DD,
     type = "h", col = "blue", lwd = 2, ylim = yRange, axes = FALSE,
     ylab = "", xlab = "", xaxt = "n"
)

points(which(DD == 0), cumsum(PL)[which(DD == 0)], pch = 10, col = "red")


## Add Legend
legend("topleft",
       legend = c("PL"),
       text.col = c("red"), lwd = 3, col = c("red"), seg.len = 0.5,
       cex = 0.75
)

# axis(1,1:length(PL),as.Date("2012-01-01")+0.005*(0:(length(PL)-1)))

# axis(1, 1:length(profit), as.Date("2010-01-01")+7*(1:length(profit)))
