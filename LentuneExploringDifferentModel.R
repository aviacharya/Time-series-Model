setwd("~/Desktop/DataScience/LentuneDataScienceProject")
library(dplyr)
library(dplyr)
library(forecast)
library(Metrics)
library(imputeTS)
library(TSA)
library(sandwich)
library(urca)
library(lmtest)
library(vars)
library(tseries)
FinalSalesDataFra2me <- read.csv("Top100stockdataforbranch2.csv",header = TRUE)
#explore the data
glimpse(FinalSalesDataFrame)


#To check number of rows in dataset
nrow(FinalSalesDataFrame)

BranchFilter <-c(2)


FinalSalesDataFramefilter <-FinalSalesDataFrame %>% # use 32779
  filter(Branch %in% BranchFilter,Stock==435) %>%
  arrange(Date)
nrow(FinalSalesDataFramefilter)

FinalSalesDataFramefilter <-na.omit(FinalSalesDataFramefilter)
#file = paste(code,'.csv',sep = "")
#write.csv(FinalSalesDataFramefilter, file)


#-----------------impute missing date---
glimpse(FinalSalesDataFramefilter)
# FinalSalesDataFramefilter$Date <- as.Date(FinalSalesDataFramefilter$Date,format="%Y-%m-%d")
FinalSalesDataFramefilter$Date <- as.Date(FinalSalesDataFramefilter$Date,format ="%d/%m/%Y")
glimpse(FinalSalesDataFramefilter)

sorted.data <- FinalSalesDataFramefilter[order(FinalSalesDataFramefilter$Date),]

#View(sorted.data)
data.length <- length(sorted.data$Date)
data.length

time.min <- as.Date("2012-05-01")
time.max <- as.Date("2019-04-30")
all.dates <- seq(time.min, time.max, by="month")

all.dates

all.dates.frame <- data.frame(list(Date=all.dates))


merged.data <- merge(all.dates.frame, sorted.data,all=T)


dat_ts <- ts(merged.data$TotalQuantity, start=c(2012,5), end= c(2019,10), frequency = 12)

# na interpolation
dat_impute <- na.interpolation(dat_ts)

#---timeseries plot with our data
autoplot(dat_impute, main = "Sales of Stock 435  made by Branch 2",ylab = 
           "Total sales", xlab = "Year",col="blue",cex = .8)



#decomposition of stock sales
autoplot(decompose(dat_impute))

#seasonalplot
ggseasonplot(dat_impute) 
  #ggtitle("Seasonal plot: stock sales")


#forecast with various method
##################################
# use a benchmark method to forecast
fit <-snaive(dat_impute) #residual sd: 45.74 and RMSE : 45.489
print(summary(fit))
checkresiduals(fit)


#Naive Forecasting Method
NaiveModel <- naive(dat_impute, h = 3) #residual sd: 52.153 and RMSE : 51.853
summary(NaiveModel)
checkresiduals(fit)

#mean(fit$residuals)
#fit ETS method (Error,Trend,Seasonal)
fit_ets <-ets(dat_impute) #residual sd: 31.609 and RMSE : 31.248
print(summary(fit_ets))
checkresiduals(fit_ets)
f1 <- forecast(fit_ets,h=3)
print(summary(f1))


#Simple Exponential Smoothing
simpleExponentialModel <- ses(dat_impute, h=3) # residual sd:26.1322 and RMSE: 25.83694
summary(simpleExponentialModel)
checkresiduals(simpleExponentialModel)

#Holt's Trend Method
Holt_model <- holt(dat_impute, h = 3,seasonal="additive") # residual sd:25.7458 and RMSE: 25.160
summary(Holt_model)
checkresiduals(Holt_model)

#--------------TBATS model--------------
m_tbats = tbats(dat_impute)
summary(m_tbats)
f_tbats = forecast(m_tbats, h=3)
print(summary(f_tbats))
plot(f_tbats)
summary(tbats.components(m_tbats))

# ARIMA MODEL

boxplot(dat_ts ~cycle(dat_impute),xlab = "Month", ylab = "stock_sales",notch = TRUE)

ArimaModel <- auto.arima(dat_impute, d=1,D=1, stepwise = FALSE,approximation = FALSE,trace = TRUE,
                         seasonal=TRUE)#residual 22.16303 and RMSE : 21.39462



print(summary(ArimaModel))

forecast <-forecast(ArimaModel,h=3,level=c (80,90))
autoplot(forecast)

print((predict(forecast)))
# write.csv(data.frame(predict(yourGLM)), "file.csv")


diff_dat <-diff(dat_impute, differences = 1)

# To check stationary of data
par(mfrow = c(1,2))
acf(ts((diff_dat)),main='ACF Stock Sales') # acf can be used to determine MA order q
pacf(ts((diff_dat)),main='PACF Stock Sales') #pacf can be used to determine AR order p

# Since there are no spikes outside the insignificant zone for both ACF and PACF plots we can conclude that residuals are random with no information or juice in
#them. Hence our ARIMA model is working fine.


#Dickery-Fuller test. 
#It is a statistical test to determine how strongly time series is defined by a trend. we interpret the results using the p-value from the test
#p=value >0.05. fail to reject the null hypothesis. THe data has a unit root and is non-stationary
#p-value <=0.05. Reject the null hypotheisis(H0).The data doesnot have unit root and is stationary
#From output the p-value is 0.02789 which is less than 0.05 thus reject the null hypothesis and is stationary
adf.test(diff_dat, alternative = "stationary")



#  This can also be tested formally with a Ljung-Box test, as follows:


Box.test(forecast$residuals, lag=20, type="Ljung-Box")
#Here, p is greater than .05, suggesting that there are NO significant autocorrelations between successive forecasting errors.
#Since the correlogram shows that none of the sample autocorrelations for lags 1-20 exceed the significance bounds, and the p-value for the Ljung-Box test 
#is 0.9, we can conclude that there is very little evidence for non-zero autocorrelations in the forecast errors at lags 1-20.



#}
