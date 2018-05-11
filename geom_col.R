#------------------------------

sop <- tw_all_company %>% filter(tse_otc == "TSE" & industry_cha == "水泥工業") %>%
  group_by(year) %>% 
  summarise(sum_operating_profit = sum(operating_profit))

ggplot(sop, aes(x = year, y = sum_operating_profit)) + 
  geom_col(colour = "black") +
  ggtitle("上市水泥營業利益總和") + xlab("年度") + ylab("營業利益總和") +
  # theme(plot.title = element_text(color = "darkred", size = 26, hjust = .2),
  #       axis.title.x = element_text(size = 14),
  #       axis.title.y = element_text(size = 14),
  #       axis.text.x = element_text(size = 12, hjust = .2),
  #       axis.text.y = element_text(size = 12)) +
  scale_y_continuous(labels = comma) +
  theme_wsj() + scale_colour_wsj("colors6")
  # theme_igray() + scale_colour_tableau()
  # theme_economist()

#------------------------------

sop <- tw_all_company %>% filter(tse_otc == "TSE" & industry_cha == "水泥工業") %>%
  group_by(year) %>% 
  summarise(sum_operating_profit = sum(operating_profit))

sop$margin_sum_operating_profit[2:7] <- diff(sop$sum_operating_profit)/sop$sum_operating_profit[1:6]
sop <- sop[-1, ]

ggplot(sop, aes(x = year, y = margin_sum_operating_profit)) + 
  geom_col(colour = "black") +
  ggtitle("上市水泥營業利益成長率") + xlab("年度") + ylab("營業利益成長率") +
  theme_wsj() + scale_colour_wsj("colors6")
  # theme_economist()
