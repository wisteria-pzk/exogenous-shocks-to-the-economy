# open libraries
library(xts)
library(zoo)
library(astsa)
library(fpp2)
library(corrplot)
library(tidyverse)
library(forecast)
library(tidyr)
library(arulesViz)
library(ggfortify)
library(vars)

# read file
gdp <- read.csv("gdp_y.csv", 
                sep = ";", 
                dec = ",", 
                header = TRUE)

# convert GDP date column to time object (to "yearqtr" class)
gdp$date <- as.Date(gdp$date, format = "%d.%m.%Y")

# see data
glimpse(gdp)

# convert GDP data to xts
gdp_xts <- as.xts(gdp[,-1], 
                  order.by = gdp$date)

# plot GDP data over time
plot.xts(gdp_xts$ad, 
         xlab = "дата", 
         ylab = "млн драмов",
         main = "ВВП в млн драмов по годам")

# GDP aggregates
gdp_xts_aggr <- gdp_xts[, 1:8]

# plot GDP data over time
autoplot(gdp_xts_aggr)
autoplot(gdp_xts_aggr, facets = FALSE, 
         xlab = "год", 
         ylab = "млн драмов", 
         main = "ВВП в млн драмов по годам по агрегатам")

# corrplot of GDP data by aggregates
gdp_xts_cor <- cor(gdp_xts)
gdp_xts_cor_aggr <- gdp_xts_cor[1:8, 1:8]

corrplot(gdp_xts_cor_aggr, 
         method = "number", 
         type = "upper", 
         title = "Коррелограмма агрегатов ВВП")

corrplot(gdp_xts_cor_aggr, 
         method = "color", 
         type = "lower", 
         title = "Коррелограмма агрегатов ВВП")

# modeling GDP aggregates
lm_c <- lm(c ~ as, data = gdp_xts)
lm_i <- lm(i ~ as, data = gdp_xts)
lm_g <- lm(g ~ as, data = gdp_xts)
lm_xn <- lm(xn ~ as, data = gdp_xts)
lm_exp <- lm(exp ~ as, data = gdp_xts)
lm_imp <- lm(imp ~ as, data = gdp_xts)
lm_ad <- lm(ad ~ as, data = gdp_xts)

# summaries of models
summary(lm_c)
summary(lm_i)
summary(lm_g)
summary(lm_xn)
summary(lm_exp)
summary(lm_imp)
summary(lm_ad)

# plot GDP aggregates
par(mfrow = c(2, 3))

plot(ad ~ as, data = gdp_xts, main = "AD и AS")
abline(reg = lm_ad, 
       col = "red", 
       lwd = 3)

plot(c ~ as, data = gdp_xts, main = "Потребление и AS")
abline(reg = lm_c, 
       col = "red", 
       lwd = 3)

plot(i ~ as, data = gdp_xts, main = "Инвестиции и AS")
abline(reg = lm_i, 
       col = "red", 
       lwd = 3)

plot(g ~ as, data = gdp_xts, main = "Гос. закупки и AS")
abline(reg = lm_g, 
       col = "red", 
       lwd = 3)

plot(exp ~ as, data = gdp_xts, main = "Экспорт и AS")
abline(reg = lm_exp, 
       col = "red", 
       lwd = 3)

plot(imp ~ as, data = gdp_xts, main = "Импорт и AS")
abline(reg = lm_imp, 
       col = "red", 
       lwd = 3)

dev.off()


