# Copyright (c) 2015 M. Rizky Luthfianto, Rochana Prih Hastuti
# This code is protected under Creative Commons License:
# http://creativecommons.org/licenses/by-nc-nd/4.0/

# References:
# - Argawal, S., 'CASH DEMAND FORECASTING FOR ATMS', Master Thesis, Indian Institute of Technology Kanpur, 2013.
#   Available from http://www.idrbt.ac.in/PDFs/PT%20Reports/2013/Shanu%20Agrawal_Cash%20demand%20forecasting%20for%20ATMs_2013.pdf
# - Ekinci, Y. et al., 'Optimization of ATM cash replenishment with group-demand forecasts', Expert Systems with Applications, vol. 42, no. 7, 2015, pp. 3480-3490.
#   Available from http://www.researchgate.net/publication/270969041_Optimization_of_ATM_cash_replenishment_with_group-demand_forecasts
# - r-snippets https://github.com/rilut/r-snippets

library(tseries)
library(forecast)

holiday_vector <- function(nth_day, days_in_year=365) {
  zeroes = rep(0,365)
  zeroes[nth_day] = 1
  return(zeroes)
}

get_holiday_xreg <- function(holiday_yday) {
  Map(holiday_vector, holiday_yday)
}

holidays = c("1-1-2014", "14-01-2014", "17-01-2014", "31-01-2014", "01-02-2014", "01-05-2014", "13-05-2014", "15-07-2014", "28-07-2014", "29-07-2014", "31-08-2014", "01-09-2014", "16-09-2014", "05-10-2014", "06-10-2014", "22-10-2014", "25-10-2014", "25-12-2014")
holiday_dates = as.Date(holidays, "%d-%m-%Y")
holiday_yday = yday(holiday_dates)

holiday_xreg = get_holiday_xreg(holiday_yday)

data = read.table("D:\\cash-forecast\\BRANCH DATA HISTORY.txt", sep="|", header=T)

branch132 <- data[which(data$BRCODE==132),]
branch132 <- branch132[order(branch132$EFFDATE),] 

hist(branch132$DEPOSIT)
library(tseries)

for (i in 1:length(branch132$DEPOSIT)) {
  branch132$flag250[i] =rangetag(      0,  250000, branch132$DEPOSIT[i])
  branch132$flag500[i] =rangetag( 250000,  500000, branch132$DEPOSIT[i])
  branch132$flag750[i] =rangetag( 500000,  750000, branch132$DEPOSIT[i])
  branch132$flag1000[i]=rangetag( 750000, 1000000, branch132$DEPOSIT[i])
  branch132$flag1250[i]=rangetag(1000000, 1250000, branch132$DEPOSIT[i])
  branch132$flag1500[i]=rangetag(1250000, 1500000, branch132$DEPOSIT[i])
  branch132$flag1750[i]=rangetag(1500000, 1750000, branch132$DEPOSIT[i])
  branch132$flag2000[i]=rangetag(1750000, 2000000, branch132$DEPOSIT[i])
  branch132$flag2250[i]=rangetag(2000000, 2250000, branch132$DEPOSIT[i])
  branch132$flag2500[i]=rangetag(2250000, 2500000, branch132$DEPOSIT[i])
}

branch132_bersih <- data.frame(CLOSINGBAL=branch132$CLOSINGBAL,f250=branch132$flag250,f500=branch132$flag500,f750=branch132$flag750,f1000=branch132$flag1000,f1250=branch132$flag1250,f1500=branch132$flag1500,f1750=branch132$flag1750,f2000=branch132$flag2000,f2250=branch132$flag2250,f2500=branch132$flag2500)
#xreg <- cbind(f250=branch132_bersih$f250,f500=branch132_bersih$f500,f750=branch132_bersih$f750,f1000=branch132_bersih$f1000,f1250=branch132_bersih$f1250,f1500=branch132_bersih$f1500,f1750=branch132_bersih$f1750,f2000=branch132_bersih$f2000,f2250=branch132_bersih$f2250,f2500=branch132_bersih$f2500)

# Variable to be modelled
CLOSE <- ts(branch132_bersih$CLOSINGBAL, frequency=1)
CLOSELOG <- log(CLOSE)
# CLOSE_DEP <- ts(branch132$CLOSINGBAL, frequency=1)
plot(CLOSE)


library(forecast)

xreg=cbind(branch132_bersih$f250,branch132_bersih$f1000,branch132_bersih$f1250,branch132_bersih$f1500,branch132_bersih$f1750,branch132_bersih$f2250)
#,branch132_bersih$f2500

library(Matrix)
rankMatrix(xreg)
ncol(xreg)

modArima <- auto.arima(CLOSE, xreg=xreg)
modArimaLOG <- auto.arima(CLOSELOG, xreg=xreg)
summary(modArimaLOG)
fore <- forecast.Arima(modArima, xreg=xreg)
foreLOG <- forecast.Arima(modArimaLOG, xreg=xreg)
forecasted=exp(foreLOG$x)

MAPE <- function(actual, forecast) {
  # Wrong
  if (length(actual) != length(forecast)) stop("actual and forecast have different lengths")
  
  res<-100 * mean( abs(forecast-actual) / abs(actual) )
  res
}

MAPE(CLOSE, forecasted)
MAPE(CLOSE, fore$x)
summary(fore)


















install.packages("ftsa")
library("ftsa")

summary(modArima)
#ME     RMSE      MAE       MPE     MAPE     MASE       ACF1
#%Training set -42261.67 600201.6 344640.4 -21.78284 34.82106 1.025143 0.02362197

# summary(branch132_bersih$f750)
# summary(branch132_bersih$f500)
# summary(branch132_bersih$f2000)
############################################################################################################################

#closingbalTS <- ts(branch132$CLOSINGBAL)
closingbalTS <- ts(branch132$CLOSINGBAL, frequency=7)
plot.ts(closingbalTS)
#CEK TREND U/ NON-SEASONAL DATA
library("TTR")
closingbalSMA3 <- SMA(closingbalTS, n=3)
plot.ts( closingbalSMA3)
closingbalSMA10 <- SMA(closingbalTS, n=10)
plot.ts( closingbalSMA10)
#SEASONAL asumsi
closingbalCOMP <- decompose(closingbalTS)
plot(closingbalCOMP)

#FORE Simple NON-SEASONAL
closingFORE <- HoltWinters(closingbalTS, beta=F, gamma=F)
#closingFORE$SSE 1.575058e+14
mean_SSE <- (closingFORE$SSE / 365.25)
RMSE_FORE <- sqrt(mean_SSE) #656679
#         closingFORE2 <- forecast.HoltWinters(closingFORE, h=365)
#         plot.forecast(closingFORE2)
#adjusted to NON SEASONAL
closingbalADJUSTED <- closingbalTS- closingCOMP$seasonal
closingFOREAD <- HoltWinters(closingbalADJUSTED, beta=F, gamma=F)
#closingFOREAD$SSE  1.476617e+14
mean_SSEAD <- (closingFOREAD$SSE / 365.25)
RMSE_FOREAD <- sqrt(mean_SSEAD) 635826.8
#         closingFOREAD2 <- forecast.HoltWinters(closingFOREAD, h=365)
#         plot.forecast(closingFOREAD2)

#FORE HOLT
closingHOLT <- HoltWinters(closingbalADJUSTED, gamma=F)
#closingHOLT$SSE 1.494821e+14

#FORE HOLT-WINTERS
closingLOG <- log(closingbalTS)
closingHW <- HoltWinters(closingLOG)
#closingHW$SSE 90.36052
mean_HWLOG <- (closingHW$SSE / 365.25)
RMSE_HWLOG <- sqrt(mean_HWLOG) #0.4973868
plot(closingHW)

closingHW2 <- HoltWinters(closingbalTS)
plot(closingHW2)
#closingHW2$SSE 1.846445e+14
mean_HW2 <- (closingHW2$SSE / 365.25)
RMSE_HW2 <- sqrt(mean_HW2) #711005.8
#FORE ARIMA
