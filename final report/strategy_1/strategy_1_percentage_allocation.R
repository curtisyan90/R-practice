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
fee <- 0.0000229
longStop <- (1 - 0.02)
std_filter <- 2.8 # when std < filter
d <- 4 # std previous days
pos_per <- 0.1 # percentage of asset used per position

m <- 101 # start K
c <- 0 # count trade
PL <- setNames(numeric(nrow(STK)), rownames(STK)) # position long
A <- setNames(numeric(nrow(STK)), rownames(STK)) # asset
A[1:m - 1] <- 1000 # set asset before start
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
      ma2[m] > ma2[m - 1] &&
      # filter
      sd(cl[(m - d):(m - 1)]) < std_filter &&
      # check asset
      A[m - 1] > 0
  ) {
    ## set long position
    long <- as.numeric(Cl(STK)[m]) # buy price
    share <- (as.numeric(A[m - 1]) * pos_per) / long # share amount
    A[m] <- as.numeric(A[m - 1]) * (1 - pos_per) # asset after buy
    c <- c + 1

    while (
      ## close < ma1
      (Cl(STK)[m] < ma1[m] ||
        ## ma1 < ma2
        ma1[m] < ma2[m] ||
        ## close < stop
        cl[m] < long * longStop) == FALSE &&
        m < nrow(STK) - 1) {
      m <- m + 1
      A[m] <- as.numeric(A[m - 1])
    }
    ## sell
    PL[m] <- (as.numeric(cl[m]) - long) * (1 - fee)
    A[m - 1] <- as.numeric(A[m - 2]) + long * share +
      (as.numeric(cl[m]) - long) * share * (1 - fee)
  }
  A[m] <- as.numeric(A[m - 1])
  m <- m + 1
}
A[m] <- as.numeric(A[m - 1])
################################################################################
wr <- length(PL[PL > 0]) / length(PL[PL != 0]) # ??????
odds <- (mean(PL[PL > 0])) / abs(mean(PL[PL < 0])) # ?????????
EV <- wr * odds + (1 - wr) * (-1) # ?????????
PF <- sum(PL[PL > 0]) / abs(sum(PL[PL < 0])) # ????????????
DD <- cumsum(PL) - cummax(cumsum(PL)) # ??????
MDD <- min(DD) # ????????????
DD_A <- A - max(A) ## ????????????
MDD_A <- min(DD_A) ## ??????????????????
DDper <- DD / cummax(cumsum(PL))
# which(DDper==min(DDper))
tail(sort(diff(which(DD == 0))), 5)
income <- sum(PL) # ?????????
asset <- A[length(A)] ## ??????
################################################################################
par(mar = c(5, 4, 2, 4) + 0.1)
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
par(new = TRUE)
plot(A,
  type = "l", col = "orange", lwd = 2, ylim = range(A), axes = FALSE,
  ylab = "", xlab = "", xaxt = "n"
)
points(which(DD_A == 0), A[which(DD_A == 0)], pch = 10, col = "orange")

axis(side = 4, at = pretty(range(A)))
mtext("Asset", side = 4, line = 3)
#
## Add Legend
legend("topleft",
  legend = c("PL", "Asset"),
  text.col = c("red", "orange"), lwd = 3, col = c("red", "orange"), seg.len = 0.5,
  cex = 0.75
)

# axis(1, 1:length(PL), as.Date("2012-01-01") + 1 * (1:length(PL)))
# axis(1, 1:(length(PL)/30),format(as.Date("1980-12", "%Y-%m")+1*(1:length(PL)/30), format = "%Y-%m"), "%Y-%m")
