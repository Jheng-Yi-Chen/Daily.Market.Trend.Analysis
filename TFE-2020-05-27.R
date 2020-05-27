

library(readr)
library(quantmod)





TX <- read.csv("C:\\Users\\User\\Desktop\\Data_from_TFE\\Daily_2020_05_27.csv")
colnames(TX) <- c("Date", "Symbol", "Delivery", "Time", "Price", "Volume", "", "", "")
TX <- TX[c(-7, -8, -9)]
Row = which(TX$Symbol == "TX")
TX1 = TX[Row, ]






View(TX1)
