##########上櫃鋼鐵工業##########

sop <- tw_all_company %>% filter(tse_otc == "OTC" & industry_cha == "鋼鐵工業") %>%
  group_by(year) %>% 
  summarise(sum_operating_profit = sum(operating_profit))

ggplot(sop, aes(x = year, y = sum_operating_profit)) + 
  geom_col(colour = "black") +
  ggtitle("上櫃鋼鐵工業營業利益總和") + xlab("年度") + ylab("營業利益總和") +
  scale_y_continuous(labels = comma) +
  theme_wsj() + scale_colour_wsj("colors6")

#------------------------------

sop <- tw_all_company %>% filter(tse_otc == "OTC" & industry_cha == "鋼鐵工業") %>%
  group_by(year) %>% 
  summarise(sum_operating_profit = sum(operating_profit))

sop$margin_sum_operating_profit[2:7] <- diff(sop$sum_operating_profit)/sop$sum_operating_profit[1:6]
sop <- sop[-1, ]

ggplot(sop, aes(x = year, y = margin_sum_operating_profit)) + 
  geom_col(colour = "black") +
  ggtitle("上櫃鋼鐵工業營業利益成長率") + xlab("年度") + ylab("營業利益成長率") +
  theme_wsj() + scale_colour_wsj("colors6")

#------------------------------

sni <- tw_all_company %>% filter(tse_otc == "OTC" & industry_cha == "鋼鐵工業") %>%
  group_by(year) %>% 
  summarise(sum_net_income = sum(net_income))

ggplot(sni, aes(x = year, y = sum_net_income)) + 
  geom_col(colour = "black") +
  ggtitle("上櫃鋼鐵工業稅後淨利總和") + xlab("年度") + ylab("稅後淨利總和") +
  scale_y_continuous(labels = comma) +
  theme_wsj() + scale_colour_wsj("colors6")

#------------------------------

sni <- tw_all_company %>% filter(tse_otc == "OTC" & industry_cha == "鋼鐵工業") %>%
  group_by(year) %>% 
  summarise(sum_net_income = sum(net_income))

sni$margin_sum_net_income[2:7] <- diff(sni$sum_net_income)/sni$sum_net_income[1:6]
sni <- sni[-1, ]

ggplot(sni, aes(x = year, y = margin_sum_net_income)) + 
  geom_col(colour = "black") +
  ggtitle("上櫃鋼鐵工業稅後淨利成長率") + xlab("年度") + ylab("稅後淨利成長率") +
  theme_wsj() + scale_colour_wsj("colors6")

#------------------------------

sr <- tw_all_company %>% filter(tse_otc == "OTC" & industry_cha == "鋼鐵工業") %>%
  group_by(year) %>% 
  summarise(sum_revenue = sum(revenue))

ggplot(sr, aes(x = year, y = sum_revenue)) + 
  geom_col(colour = "black") +
  ggtitle("上櫃鋼鐵工業營收總和") + xlab("年度") + ylab("營收總和") +
  scale_y_continuous(labels = comma) +
  theme_wsj() + scale_colour_wsj("colors6")

#------------------------------

sr <- tw_all_company %>% filter(tse_otc == "OTC" & industry_cha == "鋼鐵工業") %>%
  group_by(year) %>% 
  summarise(sum_revenue = sum(revenue))

sr$margin_sum_revenue[2:7] <- diff(sr$sum_revenue)/sr$sum_revenue[1:6]
sr <- sr[-1, ]

ggplot(sr, aes(x = year, y = margin_sum_revenue)) + 
  geom_col(colour = "black") +
  ggtitle("上櫃鋼鐵工業營收成長率") + xlab("年度") + ylab("營收成長率") +
  theme_wsj() + scale_colour_wsj("colors6")

#------------------------------
