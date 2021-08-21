###*************************************************************************
###*************************************************************************
###                                                                      ***
###                    machine learning in time series                   ***
###                     1- prediction by time series                     ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
library(readxl)
Exer13_5_data <- read_excel("C:/Users/lenovo/Desktop/Exer13_5_data.xls")
View(Exer13_5_data)
df<-Exer13_5_data
gdp<-as.vector(df[[3]])
gdp<-ts(gdp,frequency = 4,start = c(1947,1))
plot(gdp)
decgdp<-decompose(gdp)
decgdp
plot(decgdp)
#### Plot the Different Components Individually
plot(decgdp$trend)
plot(decgdp$seasonal)
plot(decgdp$random)
###*************************************************************************
###                                                                      ***
###                   2- prediction by time series                       ***
###                     a- prediction by ARIMA                           ***
###                                                                      ***
###*************************************************************************
###*************************************************************************

###      SPLITTING THE DATA TRAINING AND TEST SETS

train_set<-ts(gdp,frequency = 4,start = c(1947,1),end = c(2000,4))
test_set<-ts(gdp,frequency = 4,start = c(2001,1),end = c(2010,4))
train_set
####        study stationnarty
acf(train_set)
pacf(train_set)
library(tseries)
library(forecast)
adf.test(train_set)
PP.test(train_set)
dtrain_set<-diff(train_set,differences = 1)
plot(dtrain_set)
adf.test(dtrain_set)
PP.test(dtrain_set)
acf(dtrain_set)
pacf(dtrain_set)
ddtrain_set<-diff(train_set,differences = 2)
plot(ddtrain_set)
adf.test(ddtrain_set)
PP.test(ddtrain_set)
# Training the model ------------------------------------------------------

model1<-auto.arima(ddtrain_set)
model1


###*  *** Prediction ***
#     --------------------

predtest_set <- forecast(model1,h=40) 
predtest_set
plot(predtest_set)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set
mae <- Metrics::mae(actual = actual, predicted = predtest_set)
mse <- Metrics::mse(actual = actual, predicted = predtest_set)
rmse <- Metrics::rmse(actual = actual, predicted = predtest_set)

# Table of results

knitr::kable(cbind(mae, mse, rmse))
###*************************************************************************
###                                                                      ***
###                        Prediction by time series                     ***
###                     b- Prediction by exponenial smoothing            ***
###                                                                      ***
###*************************************************************************

###      SPLITTING THE DATA TRAINING AND TEST SETS
train_set<-ts(gdp,frequency = 4,start = c(1947,1),end = c(2000,4))
# Training the model ------------------------------------------------------
args(HoltWinters)
model2<-HoltWinters(train_set)
model2
test_set<-ts(gdp,frequency = 4,start = c(2001,1),end = c(2010,4))
###*  *** Prediction ***
#     --------------------

predtest_set <- forecast(model2,h=40) 
predtest_set
plot(predtest_set)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set
mae <- Metrics::mae(actual = actual, predicted = predtest_set)
mse <- Metrics::mse(actual = actual, predicted = predtest_set)
rmse <- Metrics::rmse(actual = actual, predicted = predtest_set)

# Table of results

knitr::kable(cbind(mae, mse, rmse))
