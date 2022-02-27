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
library(plyr)

# read file
gdp <- read.csv("gdp_q.csv", 
                sep = ";", 
                dec = ",", 
                header = TRUE)

# convert GDP date column to time object (to "yearqtr" class)
gdp$date <- as.yearqtr(gdp$date)

# see data
glimpse(gdp)

# convert GDP data to xts
gdp_xts <- as.xts(gdp[,-1], 
                  order.by = gdp$date)

# plot GDP data over time
plot.xts(gdp_xts$ad, 
         xlab = "дата", 
         ylab = "млн драмов",
         main = "ВВП в млн драмов по кварталам")

# GDP aggregates
gdp_xts_aggr <- gdp_xts[, 1:8]

# plot GDP data over time 4 inch * 8 inch
autoplot(gdp_xts_aggr)
autoplot(gdp_xts_aggr, facets = FALSE)

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

# log of GDP aggregates and delete "xn"
gdp_xts_log <- log(gdp_xts_aggr)
gdp_xts_log <- gdp_xts_log[ , colSums(is.na(gdp_xts_log)) == 0]

# modeling GDP aggregates
lm_c <- lm(c ~ as, data = gdp_xts_log)
lm_i <- lm(i ~ as, data = gdp_xts_log)
lm_g <- lm(g ~ as, data = gdp_xts_log)
lm_exp <- lm(exp ~ as, data = gdp_xts_log)
lm_imp <- lm(imp ~ as, data = gdp_xts_log)
lm_ad <- lm(ad ~ as, data = gdp_xts_log)

# summaries of models
summary(lm_c)
summary(lm_i)
summary(lm_g)
summary(lm_exp)
summary(lm_imp)
summary(lm_ad)

# plot GDP aggregates
par(mfrow = c(2, 3))

plot(ad ~ as, data = gdp_xts_log, main = "AD and AS")
abline(reg = lm_ad, col = "red", lwd = 3)

plot(c ~ as, data = gdp_xts_log, main = "C and AS")
abline(reg = lm_c, col = "red", lwd = 3)

plot(i ~ as, data = gdp_xts_log, main = "I and AS")
abline(reg = lm_i, col = "red", lwd = 3)

plot(g ~ as, data = gdp_xts_log, main = "G and AS")
abline(reg = lm_g, col = "red", lwd = 3)

plot(exp ~ as, data = gdp_xts_log, main = "Exp and AS")
abline(reg = lm_exp, col = "red", lwd = 3)

plot(imp ~ as, data = gdp_xts_log, main = "Imp and AS")
abline(reg = lm_imp, col = "red", lwd = 3)

dev.off()

# functions
arima_forecast <- function (variable, period) {
  variable_fit <- auto.arima(variable)
  
  print(summary(variable_fit))
  
  sarima(variable,
         p = variable_fit$arma[1], 
         d = variable_fit$arma[6], 
         q = variable_fit$arma[2],
         P = variable_fit$arma[3], 
         D = variable_fit$arma[7], 
         Q = variable_fit$arma[4], 
         S = variable_fit$arma[5])
  
  variable_forecast <- sarima.for(variable, 
                                  n.ahead = period, 
                                  p = variable_fit$arma[1], 
                                  d = variable_fit$arma[6], 
                                  q = variable_fit$arma[2],
                                  P = variable_fit$arma[3], 
                                  D = variable_fit$arma[7], 
                                  Q = variable_fit$arma[4], 
                                  S = variable_fit$arma[5])
  
  print(variable_forecast$pred)
  
  return(variable_forecast$pred)
}

arima_forecast_plot <- function (variable, period) {
  variable_fit <- auto.arima(variable)
  
  sarima.for(variable, 
             n.ahead = period, 
             p = variable_fit$arma[1], 
             d = variable_fit$arma[6], 
             q = variable_fit$arma[2],
             P = variable_fit$arma[3], 
             D = variable_fit$arma[7], 
             Q = variable_fit$arma[4], 
             S = variable_fit$arma[5])
}

sngl_sarima <- function(variable) {
  variable_fit <- auto.arima(variable)
  sarima(variable,
         p = variable_fit$arma[1], 
         d = variable_fit$arma[6], 
         q = variable_fit$arma[2],
         P = variable_fit$arma[3], 
         D = variable_fit$arma[7], 
         Q = variable_fit$arma[4], 
         S = variable_fit$arma[5])
}

mlt_sarima <- function(variable_1, variable_2, variable_3, variable_4, variable_5, variable_6, variable_7, variable_8) {
  sngl_sarima (variable_1)
  sngl_sarima (variable_2)
  sngl_sarima (variable_3)
  sngl_sarima (variable_4)
  sngl_sarima (variable_5)
  sngl_sarima (variable_6)
  sngl_sarima (variable_7)
  sngl_sarima (variable_8)
}

# AS forecast
gdp_xts_aggr_as_forecast <- arima_forecast(gdp_xts_aggr$as, 20)
print(gdp_xts_aggr_as_forecast)

# AD forecast
gdp_xts_aggr_ad_forecast <- arima_forecast(gdp_xts_aggr$ad, 20)
print(gdp_xts_aggr_ad_forecast)

# C forecast
gdp_xts_aggr_c_forecast <- arima_forecast(gdp_xts_aggr$c, 20)
print(gdp_xts_aggr_c_forecast)

# I forecast
gdp_xts_aggr_i_forecast <- arima_forecast(gdp_xts_aggr$i, 20)
print(gdp_xts_aggr_i_forecast)

# G forecast
gdp_xts_aggr_g_forecast <- arima_forecast(gdp_xts_aggr$g, 20)
print(gdp_xts_aggr_g_forecast)

# XN forecast
gdp_xts_aggr_xn_forecast <- arima_forecast(gdp_xts_aggr$xn, 20)
print(gdp_xts_aggr_xn_forecast)

# EXP forecast
gdp_xts_aggr_exp_forecast <- arima_forecast(gdp_xts_aggr$exp, 20)
print(gdp_xts_aggr_exp_forecast)

# IMP forecast
gdp_xts_aggr_imp_forecast <- arima_forecast(gdp_xts_aggr$imp, 20)
print(gdp_xts_aggr_imp_forecast)

# 2026 all forecast to data frame
quarter_2026 <- as.yearqtr(c("2021 Q4", 
                             "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4", 
                             "2023 Q1", "2023 Q2", "2023 Q3", "2023 Q4",
                             "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4",
                             "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4",
                             "2026 Q1", "2026 Q2", "2026 Q3"))

gdp_xts_aggr_forecast_2026 <- data.frame("Index" = quarter_2026,
                                         "as" = gdp_xts_aggr_as_forecast,
                                         "ad" = gdp_xts_aggr_ad_forecast,
                                         "c" = gdp_xts_aggr_c_forecast,
                                         "i" = gdp_xts_aggr_i_forecast,
                                         "g" = gdp_xts_aggr_g_forecast,
                                         "xn" = gdp_xts_aggr_xn_forecast,
                                         "exp" = gdp_xts_aggr_exp_forecast,
                                         "imp" = gdp_xts_aggr_imp_forecast)

# GDP line from 2005 Q1 to 2021 Q3 in data frame
gdp_xts_aggr_df <- fortify.zoo(gdp_xts[, 1:8])

# merge 2 data frame in one
gdp_aggr_forecast <- rbind.fill(gdp_xts_aggr_df, gdp_xts_aggr_forecast_2026)

# GDP line from 2005 Q1 to 2026 Q3
gdp_xts_aggr_forecast <- as.xts(gdp_aggr_forecast[,-1], 
                                  order.by = gdp_aggr_forecast$Index)

# show residuals together 10 inch * 10 inch
mlt_sarima(gdp_xts_aggr$as, gdp_xts_aggr$ad, gdp_xts_aggr$c, gdp_xts_aggr$i, 
           gdp_xts_aggr$g, gdp_xts_aggr$xn, gdp_xts_aggr$exp, gdp_xts_aggr$imp)

dev.off()

# show forecast together 20 inch * 20 inch
par(mfrow = c(4, 2))

arima_forecast_plot(gdp_xts_aggr$as, 20)
arima_forecast_plot(gdp_xts_aggr$ad, 20)
arima_forecast_plot(gdp_xts_aggr$c, 20)
arima_forecast_plot(gdp_xts_aggr$i, 20)
arima_forecast_plot(gdp_xts_aggr$g, 20)
arima_forecast_plot(gdp_xts_aggr$xn, 20)
arima_forecast_plot(gdp_xts_aggr$exp, 20)
arima_forecast_plot(gdp_xts_aggr$imp, 20)

dev.off()

# add one year difference of GDP
gdp_xts$ad_yearlag <- diff(gdp_xts$ad, lag = 4, difference = 1)

# add growth of GDP
gdp_xts$ad_yearlag_growth <- gdp_xts$ad_yearlag / gdp_xts$ad * 100

# GDP line from 2005 Q1 to 2008 Q3 in xts
gdp_xts_aggr_clean_2005 <- gdp_xts[1:15, 1:8]

# 2008 AS forecast
gdp_xts_aggr_clean_as_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$as, 5)
print(gdp_xts_aggr_clean_as_forecast_2008)

# 2008 AD forecast
gdp_xts_aggr_clean_ad_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$ad, 5)
print(gdp_xts_aggr_clean_ad_forecast_2008)

# 2008 C forecast
gdp_xts_aggr_clean_c_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$c, 5)
print(gdp_xts_aggr_clean_c_forecast_2008)

# 2008 I forecast
gdp_xts_aggr_clean_i_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$i, 5)
print(gdp_xts_aggr_clean_i_forecast_2008)

# 2008 G forecast
gdp_xts_aggr_clean_g_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$g, 5)
print(gdp_xts_aggr_clean_g_forecast_2008)

# 2008 XN forecast
gdp_xts_aggr_clean_xn_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$xn, 5)
print(gdp_xts_aggr_clean_xn_forecast_2008)

# 2008 EXP forecast
gdp_xts_aggr_clean_exp_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$exp, 5)
print(gdp_xts_aggr_clean_exp_forecast_2008)

# 2008 IMP forecast
gdp_xts_aggr_clean_imp_forecast_2008 <- arima_forecast(gdp_xts_aggr_clean_2005$imp, 5)
print(gdp_xts_aggr_clean_imp_forecast_2008)

# 2008 all forecast to data frame
quarter_2008 <- as.yearqtr(c("2008 Q4", "2009 Q1", "2009 Q2", "2009 Q3", "2009 Q4"))

gdp_xts_aggr_clean_forecast_2008 <- data.frame("Index" = quarter_2008,
                                               "as" = gdp_xts_aggr_clean_as_forecast_2008, 
                                               "ad" = gdp_xts_aggr_clean_ad_forecast_2008,
                                               "c" = gdp_xts_aggr_clean_c_forecast_2008,
                                               "i" = gdp_xts_aggr_clean_i_forecast_2008,
                                               "g" = gdp_xts_aggr_clean_g_forecast_2008,
                                               "xn" = gdp_xts_aggr_clean_xn_forecast_2008,
                                               "exp" = gdp_xts_aggr_clean_exp_forecast_2008,
                                               "imp" = gdp_xts_aggr_clean_imp_forecast_2008)

# GDP line from 2005 Q1 to 2008 Q3 in data frame
gdp_xts_aggr_clean_2005_df <- fortify.zoo(gdp_xts_aggr_clean_2005)

# GDP line from 2010 Q1 to 2015 Q3 in data frame
gdp_xts_aggr_clean_2010_df <- fortify.zoo(gdp_xts[21:43, 1:8])

# merge 3 data frame in one
gdp_aggr_clean_2008 <- rbind.fill(gdp_xts_aggr_clean_2005_df, gdp_xts_aggr_clean_forecast_2008, gdp_xts_aggr_clean_2010_df)

# GDP line from 2005 Q1 to 2015 Q3
gdp_xts_aggr_clean_2008 <- as.xts(gdp_aggr_clean_2008[,-1], 
                                  order.by = gdp_aggr_clean_2008$Index)

# 2015 AS forecast
gdp_xts_aggr_clean_as_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$as, 5)
print(gdp_xts_aggr_clean_as_forecast_2015)

# 2015 AD forecast
gdp_xts_aggr_clean_ad_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$ad, 5)
print(gdp_xts_aggr_clean_ad_forecast_2015)

# 2015 C forecast
gdp_xts_aggr_clean_c_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$c, 5)
print(gdp_xts_aggr_clean_c_forecast_2015)

# 2015 I forecast
gdp_xts_aggr_clean_i_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$i, 5)
print(gdp_xts_aggr_clean_i_forecast_2015)

# 2015 G forecast
gdp_xts_aggr_clean_g_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$g, 5)
print(gdp_xts_aggr_clean_g_forecast_2015)

# 2015 XN forecast
gdp_xts_aggr_clean_xn_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$xn, 5)
print(gdp_xts_aggr_clean_xn_forecast_2015)

# 2015 EXP forecast
gdp_xts_aggr_clean_exp_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$exp, 5)
print(gdp_xts_aggr_clean_exp_forecast_2015)

# 2015 IMP forecast
gdp_xts_aggr_clean_imp_forecast_2015 <- arima_forecast(gdp_xts_aggr_clean_2008$imp, 5)
print(gdp_xts_aggr_clean_imp_forecast_2015)

# 2015 all forecast to data frame
quarter_2015 <- as.yearqtr(c("2015 Q4", "2016 Q1", "2016 Q2", "2016 Q3", "2016 Q4"))

gdp_xts_aggr_clean_forecast_2015 <- data.frame("Index" = quarter_2015,
                                               "as" = gdp_xts_aggr_clean_as_forecast_2015, 
                                               "ad" = gdp_xts_aggr_clean_ad_forecast_2015,
                                               "c" = gdp_xts_aggr_clean_c_forecast_2015,
                                               "i" = gdp_xts_aggr_clean_i_forecast_2015,
                                               "g" = gdp_xts_aggr_clean_g_forecast_2015,
                                               "xn" = gdp_xts_aggr_clean_xn_forecast_2015,
                                               "exp" = gdp_xts_aggr_clean_exp_forecast_2015,
                                               "imp" = gdp_xts_aggr_clean_imp_forecast_2015)

# GDP line from 2017 Q1 to 2020 Q1 in data frame
gdp_xts_aggr_clean_2017_df <- fortify.zoo(gdp_xts[49:61, 1:8])

# merge 3 data frame in one
gdp_aggr_clean_2015 <- rbind.fill(gdp_aggr_clean_2008, gdp_xts_aggr_clean_forecast_2015, gdp_xts_aggr_clean_2017_df)

# GDP line from 2005 Q1 to 2020 Q1
gdp_xts_aggr_clean_2015 <- as.xts(gdp_aggr_clean_2015[,-1], 
                                  order.by = gdp_aggr_clean_2015$Index)

# 2020 AS forecast
gdp_xts_aggr_clean_as_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$as, 6)
print(gdp_xts_aggr_clean_as_forecast_2020)

# 2020 AD forecast
gdp_xts_aggr_clean_ad_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$ad, 6)
print(gdp_xts_aggr_clean_ad_forecast_2020)

# 2020 C forecast
gdp_xts_aggr_clean_c_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$c, 6)
print(gdp_xts_aggr_clean_c_forecast_2020)

# 2020 I forecast
gdp_xts_aggr_clean_i_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$i, 6)
print(gdp_xts_aggr_clean_i_forecast_2020)

# 2020 G forecast
gdp_xts_aggr_clean_g_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$g, 6)
print(gdp_xts_aggr_clean_g_forecast_2020)

# 2020 XN forecast
gdp_xts_aggr_clean_xn_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$xn, 6)
print(gdp_xts_aggr_clean_xn_forecast_2020)

# 2020 EXP forecast
gdp_xts_aggr_clean_exp_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$exp, 6)
print(gdp_xts_aggr_clean_exp_forecast_2020)

# 2020 IMP forecast
gdp_xts_aggr_clean_imp_forecast_2020 <- arima_forecast(gdp_xts_aggr_clean_2015$imp, 6)
print(gdp_xts_aggr_clean_imp_forecast_2020)

# 2020 all forecast to data frame
quarter_2020 <- as.yearqtr(c("2020 Q2", "2020 Q3", "2020 Q4", "2021 Q1", "2021 Q2", "2021 Q3"))

gdp_xts_aggr_clean_forecast_2020 <- data.frame("Index" = quarter_2020,
                                               "as" = gdp_xts_aggr_clean_as_forecast_2020, 
                                               "ad" = gdp_xts_aggr_clean_ad_forecast_2020,
                                               "c" = gdp_xts_aggr_clean_c_forecast_2020,
                                               "i" = gdp_xts_aggr_clean_i_forecast_2020,
                                               "g" = gdp_xts_aggr_clean_g_forecast_2020,
                                               "xn" = gdp_xts_aggr_clean_xn_forecast_2020,
                                               "exp" = gdp_xts_aggr_clean_exp_forecast_2020,
                                               "imp" = gdp_xts_aggr_clean_imp_forecast_2020)

# merge 2 data frame in one
gdp_aggr_clean_2020 <- rbind.fill(gdp_aggr_clean_2015, gdp_xts_aggr_clean_forecast_2020)

# GDP line from 2005 Q1 to 2021 Q3
gdp_xts_aggr_clean_2020 <- as.xts(gdp_aggr_clean_2020[,-1], 
                                  order.by = gdp_aggr_clean_2020$Index)

# 2026 AS forecast
gdp_xts_aggr_clean_as_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$as, 20)
print(gdp_xts_aggr_clean_as_forecast_2026)

# 2026 AD forecast
gdp_xts_aggr_clean_ad_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$ad, 20)
print(gdp_xts_aggr_clean_ad_forecast_2026)

# 2026 C forecast
gdp_xts_aggr_clean_c_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$c, 20)
print(gdp_xts_aggr_clean_c_forecast_2026)

# 2026 I forecast
gdp_xts_aggr_clean_i_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$i, 20)
print(gdp_xts_aggr_clean_i_forecast_2026)

# 2026 G forecast
gdp_xts_aggr_clean_g_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$g, 20)
print(gdp_xts_aggr_clean_g_forecast_2026)

# 2026 XN forecast
gdp_xts_aggr_clean_xn_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$xn, 20)
print(gdp_xts_aggr_clean_xn_forecast_2026)

# 2026 EXP forecast
gdp_xts_aggr_clean_exp_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$exp, 20)
print(gdp_xts_aggr_clean_exp_forecast_2026)

# 2026 IMP forecast
gdp_xts_aggr_clean_imp_forecast_2026 <- arima_forecast(gdp_xts_aggr_clean_2020$imp, 20)
print(gdp_xts_aggr_clean_imp_forecast_2026)

# show forecast together 20 inch * 20 inch 
par(mfrow = c(4, 2))

arima_forecast_plot(gdp_xts_aggr_clean_2020$as, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$ad, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$c, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$i, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$g, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$xn, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$exp, 20)
arima_forecast_plot(gdp_xts_aggr_clean_2020$imp, 20)

dev.off()

# show residuals together 10 inch * 10 inch
mlt_sarima(gdp_xts_aggr_clean_2020$as, gdp_xts_aggr_clean_2020$ad, gdp_xts_aggr_clean_2020$c, gdp_xts_aggr_clean_2020$i, 
           gdp_xts_aggr_clean_2020$g, gdp_xts_aggr_clean_2020$xn, gdp_xts_aggr_clean_2020$exp, gdp_xts_aggr_clean_2020$imp)

dev.off()

# 2026 all forecast to data frame
quarter_2026 <- as.yearqtr(c("2021 Q4", 
                             "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4", 
                             "2023 Q1", "2023 Q2", "2023 Q3", "2023 Q4",
                             "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4",
                             "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4",
                             "2026 Q1", "2026 Q2", "2026 Q3"))

gdp_xts_aggr_clean_forecast_2026 <- data.frame("Index" = quarter_2026,
                                         "as" = gdp_xts_aggr_clean_as_forecast_2026,
                                         "ad" = gdp_xts_aggr_clean_ad_forecast_2026,
                                         "c" = gdp_xts_aggr_clean_c_forecast_2026,
                                         "i" = gdp_xts_aggr_clean_i_forecast_2026,
                                         "g" = gdp_xts_aggr_clean_g_forecast_2026,
                                         "xn" = gdp_xts_aggr_clean_xn_forecast_2026,
                                         "exp" = gdp_xts_aggr_clean_exp_forecast_2026,
                                         "imp" = gdp_xts_aggr_clean_imp_forecast_2026)

# merge 2 data frame in one
gdp_aggr_clean_2026 <- rbind.fill(gdp_aggr_clean_2020, gdp_xts_aggr_clean_forecast_2026)

# GDP line from 2005 Q1 to 2021 Q3
gdp_xts_aggr_clean_2026 <- as.xts(gdp_aggr_clean_2026[,-1], 
                                order.by = gdp_aggr_clean_2026$Index)

# GDP aggregates actual -- gdp_xts_aggr_forecast 4 inch * 8 inch
autoplot(gdp_xts_aggr_forecast, facets = FALSE)

# GDP aggregates hypothetical -- gdp_xts_aggr_clean_2026 4 inch * 8 inch
autoplot(gdp_xts_aggr_clean_2026, facets = FALSE)

# GDP loses
gdp_xts_aggr_loses <- (gdp_xts_aggr_forecast - gdp_xts_aggr_clean_2026)
gdp_xts_aggr_loses

autoplot(gdp_xts_aggr_loses, facets = FALSE)


