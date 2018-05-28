### R Taiwan 2017 ###

# install.packages("quantmod")
library(quantmod)

setwd("Desktop/R Taiwan 2017")
getwd()
dir()

profit = sample(-20:25, 200, replace = TRUE)
plot(cumsum(profit), type = "h")

getSymbols("^VIX")
chartSeries(VIX)
chartSeries(VIX["2017-01::2017-12"], theme = "white")

STK = as.xts(read.zoo(read.csv("2330.txt")))

dim(STK)
head(STK)
View(STK)
class(STK)
time(STK)

duration = "2016-01-01::2017-07-01"

# STK = get(getSymbols("GOOG"))
# STK = get(getSymbols("2330.tw"))
chartSeries(STK, subset = duration, theme = "white", up.col = "red", dn.col = "green")

chartSeries(to.monthly(STK), subset = duration, theme = "white", up.col = "red", dn.col = "green")
chartSeries(to.weekly(STK), subset = duration, theme = "white", up.col = "red", dn.col = "green")
chartSeries(STK, subset = duration, up.col = "red", dn.col = "green")

addSMA()
addSMA(20, col = "yellow")

Cl(STK) # close price
SMA(Cl(STK)) # close price of SMA
SMA(Op(STK)) # open price
SMA(Lo(STK)) # the lowest price

cond = Cl(STK) > SMA(Cl(STK))
head(cbind(Cl(STK), SMA(Cl(STK)), cond, PL), 100)

STK = as.matrix(STK)
PL = setNames(rep(0, nrow(STK)), rownames(STK))
m = 10 + 1

while (m < nrow(STK)) {
  if (cond[m-1] == 0 && cond[m] == 1) {
    long = Cl(STK)[m]
    while (cond[m] == 1 && m < nrow(STK)) {
      m = m + 1
    } 
    PL[m] = Cl(STK)[m] - long
  }
  m = m + 1
}

plot(PL, type = "h", col = "red", lwd = 2)
plot(cumsum(PL), type = "h", col = "red", lwd = 2)
sum(PL)

cond = Cl(STK) > SMA(Cl(STK))

while (m < nrow(STK)) {
  if (cond[m-1] == 0 && cond[m] == 1) {
    short = Cl(STK)[m]
    while (cond[m] == 1 && m < nrow(STK)) {
      m = m + 1
    }
    PL[m] = Cl(STK)[m] - short
  }
  m = m + 1
}

plot(cumsum(PL), type = "h", col = "red", lwd = 2)

(WinRate = length((PL[PL > 0])) / length(PL[PL != 0]))

mean(PL[PL > 0]) / abs(mean(PL[PL < 0]))

# importance: profit factor > 
# profit factor = sum(win) / sum(lose), if profit factor > 2 that is an excellent strategy, most of profit factor is among 1.5 to 1.6

sum(PL[PL > 0]) / abs(sum(PL[PL < 0]))

# if higher than the highest price of three days, long,

STK = as.matrix(STK)
PL = setNames(rep(0, nrow(STK)), rownames(STK))
m = 3 + 1

while (m < nrow(STK)) {
  if (Cl(STK)[m] > max(Hi(STK)[(m-3):(m-1)])) {
    short = Cl(STK)[m]
    while (Cl(STK)[m] > min(Lo(STK)[(m-3):(m-1)]) && m < nrow(STK)) {
      m = m +1
    } 
    PL[m] = short - Cl(STK)[m]
  } 
  m = m + 1
}

topPL = setNames(rep(max(0, PL[1]), length(PL)) ,names(PL))

for (m in 2:length(PL)) {
  if (topPL[m-1] < sum(PL[1:m])) {
    topPL[m] = sum(PL[1:m])
  } else (topPL[m] = topPL[m-1])
}

DD = cumsum(PL) - topPL
head(cbind(PL,topPL), 20)
plot(cumsum(PL), type = "h", col= "red", lwd = 3, ylim = range(cumsum(PL), DD))
par(new = T)
plot(DD, type = "h", lwd = 3, col = "darkgreen", ylim = range(cumsum(PL), DD))
points(which(DD == 0), topPL[which(DD == 0)], pch = 4, col = "purple")

which(DD == 0)
diff(which(DD == 0))
tail(sort(diff(which(DD == 0))), 5)

# management of asset
sample(1:48, 6) # lottery

dice = sample(c(-1, 2), 1)
init = 1 # initial money
pwin = 0.5 
odds = 2
f = 0.25 # 下注比例
nbet = 1000 # 賭局次數

cap = c(init, rep(0, nbet))

for (m in 2:(nbet+1)) {
  dice = sample(c(-1, 2), 1, prob = c((1-pwin), pwin))
  cap[m] = cap[m-1]*(1+f*dice) # cap[m] = cap[m-1]*f*dice + cap[m-1]
}

plot(cap, type = "l", lwd = 2)
