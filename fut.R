
setwd("2016_fut")
getwd()

library(quantmod)
library(zoo)

futures2016 <- read.csv("2016_fut.csv", header = TRUE)
keep <- seq(2, nrow(futures2016), by = 5)
futures2016 <- futures2016[keep, ]
futures2016 <- futures2016[, c(1, 4, 5, 6, 7, 10)]
colnames(futures2016) <- c("Date", "Open", "High", "Low", "Close", "Volume")
head(futures2016, 10)

futures2016$Date <- gsub("/", "-", futures2016$Date)
futures2016$Date <- as.Date(futures2016$Date, format = "%Y/%m/%d/")

