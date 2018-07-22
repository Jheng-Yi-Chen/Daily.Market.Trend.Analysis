
# install.packages("Quandl")
# install.packages("quantmod")
# install.packages("dygraph")
# install.packages("tidyverse")

library(Quandl)
library(quantmod)
library(tidyverse)
library(dygraphs)

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2018-07-22")
getSymbols("^AXJO", src = "yahoo", from = start_date, to = end_date)
summary(AXJO) 
plot(AXJO[, "AXJO.Close"], main = "ASX200")
candleChart(AXJO, up.col = "red", dn.col = "green", theme = "black")


AXJO <- AXJO[, 1:4] %>% tail(100)
dygraph(AXJO) %>%
  dyCandlestick() %>% 
  dyEvent("2018-07-03", "Keep Rate at 1.5%", labelLoc = "bottom", col='blue') %>% 
  dyLegend(show = 'follow')


