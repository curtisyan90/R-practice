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
ATR <- ATR(STK[, c("High", "Low", "Close")], 14)
atr <- ATR[, 2]
tr <- ATR[, 1]
fee <- 0.006
longStop <- (1 - 0.03)
longDone <- (1 + 0.03)
pos_per <- 0.05

m <- 101 # start K
c <- 0
PL <- setNames(numeric(nrow(STK)), rownames(STK)) # position long
A <- setNames(numeric(nrow(STK)), rownames(STK)) # asset
A[1:m] <- 1
# OHLC
op <- Op(STK)
hi <- Hi(STK)
lo <- Lo(STK)
cl <- Cl(STK)

while (m < nrow(STK)) {
     if ( # close crossover ma1 & ma1>ma2
          (cl[m - 1] <= ma1[m - 1] && cl[m] > ma1[m] && ma1[m] > ma2[m]) ||
               # close crossover ma1 & ma2
               (cl[m - 1] <= ma1[m - 1] && cl[m] > ma1[m] &&
                    cl[m - 1] <= ma2[m - 1] && cl[m] > ma2[m]) &&
                    # ma2 growth
                    ma2[m] > ma2[m - 1]) {
          long <- as.numeric(Cl(STK)[m]) # buy price
          # share <- (as.numeric(A[m - 1]) * pos_per) / long # share amount
          # A[m] <- as.numeric(A[m - 1]) * (1 - pos_per) # asset after buy
          c <- c + 1
          while (

               # 兩種寫法
               ( # (Cl(STK)[m] < ma1[m] && tr[m] > atr[m])||
                    # 
                    # (cl[m - 1] < ma1[m - 1] && cl[m] < cl[m - 1]) ||
                         (cl[m] < long * longStop)) == FALSE &&
                    # (Cl(STK)[m] >= ma1[m] || tr[m] <= atr[m]) &&
                    # (Cl(STK)[m - 1] >= ma1[m-1] || Cl(STK)[m] >= Cl(STK)[m - 1]) && # nolint
                    # (Cl(STK)[m] >= long * longStop)&&

                    m < nrow(STK) - 1) {
               # A[m] <- as.numeric(A[m - 1])
               m <- m + 1
          }
          PL[m] <- (as.numeric(Cl(STK)[m]) - long)
          # A[m] <- as.numeric(A[m - 1]) + (as.numeriÇc(Cl(STK)[m]) - long) * share * (1 - fee)
     }
     # A[m] <- as.numeric(A[m - 1])
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
asset <- A
################################################################################
yRange <- range(cumsum(PL), DD, A)
plot(cumsum(PL),
     type = "l", col = "red", lwd = 2, ylim = yRange,
     ylab = "PL", xlab = "Date", xaxt = "n"
)
par(new = TRUE)
plot(A,
     type = "l", col = "yellow", lwd = 2, ylim = yRange,
     ylab = "", xlab = "", xaxt = "n"
)
par(new = TRUE)
plot(DD,
     type = "h", col = "blue", lwd = 2, ylim = yRange,
     ylab = "", xlab = "", xaxt = "n"
)
par(new = TRUE)
plot(DD,
     type = "h", col = "blue", lwd = 2, ylim = yRange,
     ylab = "", xlab = "", xaxt = "n"
)

points(which(DD == 0), cumsum(PL)[which(DD == 0)], pch = 10, col = "red")

axis(1, 1:length(PL), as.Date("2012-01-01") + 1 * (1:length(PL)))
# axis(1, 1:(length(PL)/30),format(as.Date("1980-12", "%Y-%m")+1*(1:length(PL)/30), format = "%Y-%m"), "%Y-%m")
