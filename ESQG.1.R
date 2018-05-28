
library(foreign)
library(readstata13)

es1 <- read.dta("ES-Indicators-Database-.dta")
qog1 <- read.dta("qog_std_ts_jan17.dta", convert.factors = FALSE)

# tfp <- read.dta("Firm Level TFP Estimates and Factor Ratios_February_21_2017.dta")

####################################################################################################

qog1$cy <- qog1$ccode * qog1$year
es1$cy <- es1$ccode * es1$year
mr1 <- merge(x = es1, y = qog1, by = "cy")

####################################################################################################

qog_var <- names(qog1) %in% c("ccode", "year", "fh_rol", "fh_polity2", "pwt_hci", "dpi_system", "ht_regtype", "p_polity", "p_polity2", "undp_hdi") 
qog2 <- qog1[qog_var]

qog2$cy <- qog2$ccode * qog2$year
es1$cy <- es1$ccode * es1$year

mr1 <- merge(x = es1, y = qog2, by = "cy")

####################################################################################################

library(lattice)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)

table(mr1$car4)
mr1 %>% filter(!is.na(soe)) -> mr1soe # filter rows

# mr1 %>% select(c(1,2,3)) -> test1 # select columns

# na.fail(mr1$soe)
# is.null(mr1$soe) ; is.na(mr1$soe) ; any(is.na(mr1$soe))
# mr1$soe1 <- na.exclude(mr1$soe)
# mr1$soe1 <- mr1[na.exclude(mr1$soe), ]
# table(mr1$soe1)
# summary(mr1$soe1)

count_soe <- ggplot(mr1soe, aes(x = mr1soe$soe)) + 
  geom_bar() + 
  xlab("Numbers of Soes and Non Soes") +
  ylab("Count")
ggsave("count_soe.png")

es2 <- mr1[ which(mr1$car4 >= 1), ]

####################################################################################################

library(reshape)
# mdata <- melt(mydata, id=c("id","time"))

table(es2$year.x)
sum(table(es2$year.x))
table(es2$year.y)
sum(table(es2$year.x))

table(es2$country2)
table(es2$country2, es2$year.x)
countryyear <- as.data.frame(table(es2$country2, es2$year.x))
colnames(countryyear) <- c("Country", "Year", "Count")
# countryyear1 <- melt(countryyear, Count = c("Country"))
# group_by(countryyear, )
write.csv(countryyear, file = "countryyear.csv")

####################################################################################################

# es2$size1 <- NA # ctrl + shift + c 
# es2$size1[es2$size == "small(<20)" ] <- 1
# es2$size1[es2$size == "medium(20-99)" ] <- 2
# es2$size1[es2$size == "large(100 and over)" ] <- 3
# table(es2$size1) # company size, 1 is small, 2 is medium, 3 is large

table(es2$size)
barplot(table(es2$size))

# es2$size1 = es2$size[!is.na(es2$size),]

es2 %>%
  filter(!is.na(size)) -> es2size

company_size <- ggplot(es2size, aes(x = size)) + 
  geom_bar() + 
  xlab("Compnay Size") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 10, hjust = 1))
ggsave("company_size.png")

# subset(es2, size != NA, select = c(car1))
tab1 <- table(es2$size, es2$exporter)
tab1

# si <- ddply(es2, "size", transform, Percent_Size = as.numeric(size) / sum(as.numeric(size)) * 100)
# ggplot(si, aes(x = size, y = Percent_Size, fill = exporter)) + 
#   geom_bar(stat = "identity")

# comapnysize <- ggplot(es2, aes(es2$size)) +
#   geom_bar(aes(color = es2$size)) +
#   coord_flip() +
#   scale_fill_manual(values = c("es2$size")) +
#   labs(x = "company size")
# ggsave("company_size.png")

####################################################################################################

es2 %>% filter(!is.na(car1)) -> es2age

summary(es2age$car1)

comapny_age <- ggplot(es2age, aes(car1)) +
  geom_bar(aes(color = car1)) +
  coord_flip() +
  scale_fill_manual(values = c("car1")) +
  labs(x = "Company Age", y = "Count")
ggsave("company_age.png")

####################################################################################################

library(vcd)
library(mosaic)

es2$perf11 <- NA
es2$perf11[es2$perf1 >= 0] <- "increase"
es2$perf11[es2$perf1 < 0] <- "decrease"
table(es2$perf11); es2$perf11

es2 %>% select(c(perf11, exporter, obst1)) -> es2peo1

colnames(es2peo1) <- c("Performance", "Exporter", "Access_to_Finance")

es2peo1$Access_to_Finance[es2peo1$Access_to_Finance == 100] <- "Yes"
es2peo1$Access_to_Finance[es2peo1$Access_to_Finance == 0] <- "No"

es2peo1 %>% 
  mosaic_es2peo1 <- mosaic( ~ Performance + Exporter + Access_to_Finance, data = es2peo1)

####################################################################################################

table(es2$exporter)

experfobst1 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst1, data = es2)
experfobst2 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst2, data = es2)
experfobst3 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst3, data = es2)
experfobst4 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst4, data = es2)
experfobst5 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst5, data = es2)
experfobst6 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst6, data = es2)
experfobst7 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst7, data = es2)
experfobst8 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst8, data = es2)
experfobst9 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst9, data = es2)
experfobst10 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst10, data = es2)
experfobst11 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst11, data = es2)
experfobst12 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst12, data = es2)
experfobst13 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst13, data = es2)
experfobst14 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst14, data = es2)
experfobst15 <- mosaic( ~ es2$perf11 + es2$exporter + es2$obst15, data = es2)

expper <- qplot(es2$perf1, es2$car6, data = es2, colour = factor(es2$exporter)) + 
  scale_colour_manual(values = c("red","green"))
expper

####################################################################################################

# mr1 %>% filter(!is.na(soe)) -> mr1soe
# es2 %>% filter(!is.na(size)) -> es2size
# es2 %>% filter(!is.na(car1)) -> es2age
# es2 %>% filter(!is.na(exporter)) -> es2exporter
# es2 %>% filter(!is.na(perf1)) -> es2perf1
# es2 %>% filter(!is.na(perf2)) -> es2perf2
# es2 %>% filter(!is.na(obst1)) -> es2obst1
# es2 %>% filter(!is.na(obst2)) -> es2obst2
# es2 %>% filter(!is.na(obst3)) -> es2obst3
# es2 %>% filter(!is.na(obst4)) -> es2obst4
# es2 %>% filter(!is.na(obst5)) -> es2obst5
# es2 %>% filter(!is.na(obst6)) -> es2obst6
# es2 %>% filter(!is.na(obst7)) -> es2obst7
# es2 %>% filter(!is.na(obst8)) -> es2obst8
# es2 %>% filter(!is.na(obst9)) -> es2obst9
# es2 %>% filter(!is.na(obst10)) -> es2obst10
# es2 %>% filter(!is.na(obst11)) -> es2obst11
# es2 %>% filter(!is.na(obst12)) -> es2obst12
# es2 %>% filter(!is.na(obst13)) -> es2obst13
# es2 %>% filter(!is.na(obst14)) -> es2obst14
es2 %>% filter(!is.na(obst15)) -> es2obst15
summarise(es2obst15, mean = mean(as.numeric(es2obst15)), min = min(as.numeric(es2obst15)), max = max(as.numeric(es2obst15)))

####################################################################################################

es2manufacturing <- es2[ which(es2$stra_sector == "Manufacturing"), ]
es3 <- mr1[ which(mr1$region == "EAP"), ]
# write.csv(es2, file = "es2.csv")
# write.dta(es2, file = "es2.dta")
# write.dta(mr1, file = "mr1.dta")

####################################################################################################

table(es2$idstd)
table(es2$year)
table(es2$country2)
table(es2$ownership)
table(es2$secfpct1)
table(es2$car1)
table(es2$car2)
table(es2$car3)
table(es2$car4)
table(es2$car6)
filter(es2, es2$car6 > 50)
filter(es2, es2$car6 > 90)
arrange(es2, desc(car1)) %>% head

####################################################################################################

table(es2$stra_sector)
es2 %>% filter(!is.na(stra_sector)) -> es2sector

sector_amount <- as.data.frame(table(es2sector$stra_sector))
colnames(sector_amount) <- c("Sector", "Count")
sector_amount5 <- sector_amount[ which(sector_amount$Count > 9), ]
ggplot(sector_amount, aes(x = Count, y = Sector)) +
  geom_point()
company_sector <- ggplot(sector_amount5, aes(x = Count, y = Sector)) + 
  geom_point()
ggsave("company_sector.png")

####################################################################################################

table(es2$perf1)
summary(es2$perf1)
table(es2$perf2)
summary(es2$perf2)

####################################################################################################

t(as.matrix(table(es2$obst1)))
t(as.matrix(table(es2$obst2)))
t(as.matrix(table(es2$obst3)))
t(as.matrix(table(es2$obst4)))
t(as.matrix(table(es2$obst5)))
t(as.matrix(table(es2$obst6)))
t(as.matrix(table(es2$obst7)))
t(as.matrix(table(es2$obst8)))
t(as.matrix(table(es2$obst9)))
t(as.matrix(table(es2$obst10)))
t(as.matrix(table(es2$obst11)))
t(as.matrix(table(es2$obst12)))
t(as.matrix(table(es2$obst13)))
t(as.matrix(table(es2$obst14)))
t(as.matrix(table(es2$obst15)))

obsttable <- rbind(t(as.matrix(table(es2$obst1))), t(as.matrix(table(es2$obst2))), t(as.matrix(table(es2$obst3))), t(as.matrix(table(es2$obst4))), t(as.matrix(table(es2$obst5))), t(as.matrix(table(es2$obst6))),
                   t(as.matrix(table(es2$obst7))), t(as.matrix(table(es2$obst8))), t(as.matrix(table(es2$obst9))), t(as.matrix(table(es2$obst10))), t(as.matrix(table(es2$obst11))), t(as.matrix(table(es2$obst12))),
                   t(as.matrix(table(es2$obst13))), t(as.matrix(table(es2$obst14))), t(as.matrix(table(es2$obst15))))

colnames(obsttable) <- c("NO", "YES")
rownames(obsttable) <- c("access to finance", "access to land", "business licensing and permits", "corruption", "courts", "crime, theft and disorder", "customs and trade regulations", "electricity", "inadequately educated workforce", "labor regulations", "political instability", "practices of competitors in the informal sector", "tax administration", "tax rates", "transport")
obsttable
write.csv(obsttable, file = "obsttable.csv")

####################################################################################################

table(es2$obst1); table(es2$obst2); table(es2$obst3); table(es2$obst4); table(es2$obst5)
table(es2$obst6); table(es2$obst7); table(es2$obst8); table(es2$obst9); table(es2$obst10)
table(es2$obst11); table(es2$obst12); table(es2$obst13); table(es2$obst14); table(es2$obst15)

####################################################################################################

table(es2$fin2)
table(es2$stra_sector)
table(es2$fh_rol)
es2$fh_rol2 <- es2$fh_rol + 1
table(es2$fh_rol2)
table(es2$pwt_hci)
table(es2$dpi_system)
table(es2$fh_polity2)
table(es2$ht_regtype)
table(es2$p_polity)
table(es2$p_polity2)
table(es2$undp_hdi)

summary(es2$pwt_hci)
summary(es2$fh_rol2)
summary(es2$p_polity2)

t(as.matrix(summary(es2$pwt_hci)))
t(as.matrix(summary(es2$fh_rol2)))
t(as.matrix(summary(es2$p_polity2)))
statetable <- rbind(t(as.matrix(summary(es2$pwt_hci))), t(as.matrix(summary(es2$fh_rol2))), t(as.matrix(summary(es2$p_polity2))))
colnames(statetable) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's")
rownames(statetable) <- c("Human Capital", "Rule of Law", "Polity")
write.csv(statetable, file = "statetable.csv")

####################################################################################################

table1 <- table(es2$stra_sector, es2$obst8)
table1
table2 <- table(es2$stra_sector, es2$obst1)
table2

####################################################################################################

boxplot(es2$perf1 ~ es2$country2)

boxplot(es3$perf1 ~ es3$country2)
barchart(es3$country2 ~ es3$perf1, data = es3)
plot(es3$perf1, es3$car6)
xyplot(es3$perf1 ~ es3$car6 | es3$country2, data = es3)

perff1 <- ggplot(es3, aes(x = car6, y = perf1)) +
  geom_point(aes(color = country2))
perff1

perff2 <- ggplot(es3, aes(x = country2, y = perf1)) +
  geom_boxplot()
perff2

####################################################################################################

ggplot(es2, aes(x=es2$obst1)) + 
  geom_bar()

ggplot(es2, aes(x = factor(es2$obst1), y=es2$perf1)) +  
  geom_boxplot()

# ggplot(es2$soe, aes(x=factor(es2$soe), y=es2$per)) +  
#   geom_boxplot() +
#   scale_fill_manual(values = c("yellow", "orange")) + 
#   ggtitle("SOE and Performance") + 
#   theme(axis.text.x = element_text(angle=90, face="bold", colour="black"))
# 
# mosaicplot(~obst1 + car1, data = es2, main = "Survival on The Titanic", color= T)

####################################################################################################

HCASG <- ggplot(es2, aes(x = es2$pwt_hci, y = es2$perf1)) +
  geom_point() +
  labs(x = "Human Capital", y = "Annual Sales Growth") +
  stat_smooth(method = lm, level = 0.95) +
  ggtitle("Human Capital and Annual Sales Growth")
HCASG  
ggsave("HCASG.png")

####################################################################################################

PASG <- ggplot(es2, aes(x = es2$p_polity2, y = es2$perf1)) +
  geom_point() +
  labs(x = "Polity", y = "Annual Sales Growth")
#  ggtitle("Polity and Annual Sales Growth")
ggsave("PFLOASG.png")

####################################################################################################

# es2$car6[es2$car6 == 0] <- NA

PFLOASG <- ggplot(es2, aes(x = es2$car6, y = es2$perf1)) +
  geom_point() +
  labs(x = "Proportion of a Firm held by the Largest Owner", y = "Annual Sales Growth") +
  stat_smooth(method = lm, level = 0.95)
#  ggtitle("Proportion of a Firm held by the Largest Owner and Annual Sales Growth")
ggsave("PFLOASG.png")

####################################################################################################

library(lme4)
library(xtable)

model0_1 <- lm(perf1 ~ exporter + size + car1 + car6 +
                 factor(obst1) + factor(obst2) + factor(obst3) + factor(obst4) + factor(obst5) + factor(obst6) + factor(obst7) + factor(obst8) + factor(obst9) + factor(obst10) + factor(obst11) + factor(obst12) + factor(obst13) + factor(obst14) + factor(obst15), data = es2)
summary(model0_1)
xtable(model0_1)

model0_2 <- lm(perf1 ~ pwt_hci + fh_rol2 + p_polity2, data = es2)
summary(model0_2)

# options(digits = 4)
model0_3 <- lmer(perf1 ~ exporter + size + car1 + car6 + 
                   factor(obst1) + factor(obst2) + factor(obst3) + factor(obst4) + factor(obst5) + factor(obst6) + factor(obst7) + factor(obst8) + factor(obst9) + factor(obst10) + factor(obst11) + factor(obst12) + factor(obst13) + factor(obst14) + factor(obst15) +
                   pwt_hci + fh_rol2 + p_polity2 +
                   pwt_hci:exporter + fh_rol2:exporter + p_polity2:exporter +
                   pwt_hci:size + fh_rol2:size + p_polity2:size +
                   pwt_hci:car1 + fh_rol2:car1 + p_polity2:car1 +
                   pwt_hci:car6 + fh_rol2:car6 + p_polity2:car6 +
                   pwt_hci:factor(obst1) + fh_rol2:factor(obst1) + p_polity2:factor(obst1) +
                   pwt_hci:factor(obst2) + fh_rol2:factor(obst2) + p_polity2:factor(obst2) +
                   pwt_hci:factor(obst3) + fh_rol2:factor(obst3) + p_polity2:factor(obst3) +
                   pwt_hci:factor(obst4) + fh_rol2:factor(obst4) + p_polity2:factor(obst4) +
                   pwt_hci:factor(obst5) + fh_rol2:factor(obst5) + p_polity2:factor(obst5) +
                   pwt_hci:factor(obst6) + fh_rol2:factor(obst6) + p_polity2:factor(obst6) +
                   pwt_hci:factor(obst7) + fh_rol2:factor(obst7) + p_polity2:factor(obst7) +
                   pwt_hci:factor(obst8) + fh_rol2:factor(obst8) + p_polity2:factor(obst8) +
                   pwt_hci:factor(obst9) + fh_rol2:factor(obst9) + p_polity2:factor(obst9) +
                   pwt_hci:factor(obst10) + fh_rol2:factor(obst10) + p_polity2:factor(obst10) +
                   pwt_hci:factor(obst11) + fh_rol2:factor(obst11) + p_polity2:factor(obst11) +
                   pwt_hci:factor(obst12) + fh_rol2:factor(obst12) + p_polity2:factor(obst12) +
                   pwt_hci:factor(obst13) + fh_rol2:factor(obst13) + p_polity2:factor(obst13) +
                   pwt_hci:factor(obst14) + fh_rol2:factor(obst14) + p_polity2:factor(obst14) +
                   pwt_hci:factor(obst15) + fh_rol2:factor(obst15) + p_polity2:factor(obst15) +
                   (1|country2), data = es2)
summary(model0_3)

####################################################################################################

model1 <- lmer(perf1 ~ exporter + size + car6 +
                 factor(obst1) + factor(obst11) +
                 (1|country2), data = es2)
summary(model1)
print(model1)
coef((model1))

model4 <- lmer(perf1 ~ stra_sector + exporter + size +
                 factor(obst1) + factor(obst11) +
                 (1|country2), data = es2)
summary(model4)
print(model4)
coef((model4))

model5 <- lmer(perf1 ~ exporter +
                 exporter:fh_rol2 + exporter:pwt_hci +
                 fh_rol2 + pwt_hci +
                 (1|country2), data = es2)
summary(model5)
print(model5)
coef((model5))

####################################################################################################

summary(es2$exporter)
summary(es2$size)
summary(es2$fh_rol2);length(es2$fh_rol2)
summary(es2$pwt_hci);length(es2$pwt_hci)
table(es2$obst1)
table(es2$obst11)
table(es2$country2)
