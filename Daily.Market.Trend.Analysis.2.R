setwd("F:/")
getwd()
# dir()

# TX.18.02 <- read.csv("TX_2018_02.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
TX.18.02 <- read.csv("TX_2018_02.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# TX.18.02 <- read.csv("TX_2018_02.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
head(TX.18.02, 15)
# TX.18.02 <- TX.18.02[-c(1:2), ]
colnames(TX.18.02) <- c("Date", "Time", "Open", "Close", "High", "Low", "Volume")
TX.18.02$Date <- ymd(TX.18.02$Date)
TX.18.02$Time <- hms(TX.18.02$Time)
TX.18.02$Time <- ymd_hms(paste(TX.18.02$Date, TX.18.02$Time, sep = " "))
TX.18.02 <- arrange(TX.18.02, Time)
TX.18.02 <- select(TX.18.02, Time, Open, High, Low, Close, Volume)
TX.18.02 <- TX.18.02 %>% na.omit()
# TX.18.02 <- TX.18.02 %>% na.omit() %>% read.zoo %>% as.xts()

TX.18.02.xts <- xts(TX.18.02[, c("Open", "Close", "High", "Low")], order.by = TX.18.02[, 1])
# TX.18.02.xts <- xts(TX.18.02[, c("Open", "Close", "High", "Low", "Volume")], order.by = TX.18.02[, 1])
# TX.18.02.xts <- xts(TX.18.02[, -1], order.by = TX.18.02[, 1])

# c(1:nrow(TX.18.02)) %>% as.character()
# rownames(TX.18.02.xts) <- TX.18.02[[1]]
# as.xts(TX.18.02.xts)
head(TX.18.02.xts, 20); first(TX.18.02.xts, "10 hours"); glimpse(TX.18.02.xts)

# plot(TX.18.02.xts[["Time"]], TX.18.02.xts[["Close"]], type = "l")
# TX.18.02.xts %>% 
#   ggplot(aes(x = Time, y = Close)) + geom_line()

chartSeries(TX.18.02.xts)
TX.18.02.xts %>% to.daily() %>% chartSeries()
TX.18.02.xts %>% to.hourly() %>% chartSeries()
TX.18.02.xts %>% to.minutes30() %>% chartSeries()
TX.18.02.xts %>% to.minutes10() %>% chartSeries()
TX.18.02.xts %>% to.minutes5() %>% chartSeries()
TX.18.02.xts %>% to.minutes3() %>% chartSeries()
lineChart(TX.18.02.xts, line.type = "h")
addMACD()
addBBands()
Cl(TX.18.02.xts)
dailyReturn(TX.18.02.xts)
dailyReturn(TX.18.02.xts) %>% VaR(p = 0.95, method = "historical")
MA5 <- SMA(TX.18.02.xts$Close, n = 5)
MA20 <- SMA(TX.18.02.xts$Close, n = 20)
CCI20 <- CCI(TX.18.02.xts$Close, 20)
chartSeries(TX.18.02.xts, TA = "addSMA(5);addSMA(20);addCCI(20)")
TX.18.02.xts[MA5 > MA20] %>% index()

subset = "2018-02-09 10:00:00::2018-02-09 12:00:00"
subset = "10:00:00::12:00:00"
candleChart(TX.18.02.xts, up.col = "red", dn.col = "green", theme = "white", TA = "addBBands()", subset = "2018-02-09 10:00:00::2018-02-09 12:00:00")
max(TX.18.02.xts[subset])
min(TX.18.02.xts[subset])
seriesHi(TX.18.02.xts[subset])
seriesLo(TX.18.02.xts)
last(first(TX.18.02.xts, "5 hours"), "30 minutes")
