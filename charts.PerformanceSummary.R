signal <- ifelse(fu_tmp1$Short > 0, 1, 0) %>% Lag()
roc <- ROC(type = "discrete", as.numeric(fu_tmp1$Close[-1]))
ret <- roc*signal
initialsize = 100000
ret %>% na.omit() %>% PerformanceAnalytics::charts.PerformanceSummary()
