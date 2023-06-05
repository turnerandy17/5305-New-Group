# Matthew Brodie, Monica Cao, Andy Turner
# OMSBA 5305 - DTC
# 6/3/2/3
 
# Import Packages --------------------------------------------------------------
library(readxl)
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)
library(vtable)
library(dynlm)
library(tseries)
library(stats)
library(TidyDensity)
library(urca)

# Import Data ------------------------------------------------------------------
# In the first video, you need to find a time series data that interests you, 
# is approximately stationary or can be made stationary, and has at least 100 observations.
raw_data <- read_excel("ch3_COI_3MTBill.xls")
# View(raw_data)

fcst <- raw_data %>%
  select(-'CPI') %>%
  rename(date = 'observation_date',
         TB3 = 'TB3Month')

fcst <- fcst %>%
  mutate(date = date(date), #convert to date
         TB3 = as.numeric(TB3)) #convert rate to numeric

fcst <- fcst %>%
  filter(date > "2000-12-1")


tb3 <- fcst %>%
  select(TB3)

summary(fcst) # give simple summary
vtable(fcst) # give aesthetic summary for deck


# Exploratory Graph -----------------------------------------------------------
# You will perform in-sample evaluations by loading the data into RStudio, 
# plotting the time series, and removing trend and seasonality when needed.

# setting up simple line graph
ex_graph <- ggplot(fcst, mapping = aes(date, TB3)) +
  geom_line() +
  labs(x = "Date",
       y = "Rate",
       title = "3 Month T-Bill Rate") +
  scale_x_date(date_breaks = "2 year",
               date_label = "%Y") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = ),
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold",
                                  color = "gray30"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 14,
                                     color = "ivory4"),
        plot.caption = element_text(color = "gray60"))

ex_graph # view the graph


# Stationary Tests -------------------------------------------------------------
# You will also stationarize the time series using first differencing when necessary.
# Use augmented Dickey-Fuller tests to confirm you have got a stationary time series.

time_series <- ts(fcst$TB3, frequency = 12, start = c(2001,1))
adf.test(time_series)
adf_one <- adf.test(time_series)
print('Based on this Dickey-Fuller Test, we fail to reject the null hypothesis and this time series is not stationary. To combat this, we will take a log-difference time series.')

# Take log difference
ts_log <- diff(log(time_series))
adf.test(ts_log)
adf_two <- adf.test(ts_log)
print('Based on this Dickey-Fuller Test, we reject the null hypothesis and our data has been made stationary using the log-difference time series data set.')

# KPSS test for Stationarity
time_series %>% ur.kpss() %>% summary()
ts_log %>% ur.kpss() %>% summary()

# Summary: based on ADF, original time series is not stationary.
# To combat this, we are taking a log-difference which makes our data stationary
# and passes the Dickey-Fuller test. Note that we filtered out data prior to 2001.


# ACF, PACF, White-Noise -------------------------------------------------------
# Based on the ACF and PACF, you will choose three linear models (MA, AR, or ARMA) 
# and estimate them.

# Logged Time Series
acf(ts_log, lag.max = 20, plot = TRUE)
acf(ts_log, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(ts_log, lag.max = 20, plot = TRUE)
pacf(ts_log, lag.max = 20, plot = FALSE) # prints autocorrelations
Box.test(ts_log, type = 'Ljung-Box')

# ACTION: NEED TO DETERMINE WHAT INTEREPRETATIONS ARE FOR ACF/PACF...
# Monica to take this this...


# PART 1 Forecast Models (lines 116:307)----------------------------------------
# Based on the ACF and PACF, you will choose three linear models (MA, AR, or ARMA) and estimate them.
# You will present the estimation results, show the ACF and PACF correlograms of residuals from
# each specification, and verify they are white noise using Q-Test.
# You will summarize all the model estimation and evaluation in a table (refer to table 8.2 
# on page 214 in your textbook) and make six-period ahead forecasts. You will plot the multistep
# of forecasts and their correspondence bands for each specification and comment on your preferred model and why.

# LOGGED Complex Forecast Models (MA, AR, ARMA) --------------------------------
# Based on the ACF/PACF, we are focusing on the ARMA models


# Create ARMA(1,1) model
log_arma11 <- arima(ts_log, order=c(1,0,1))
summary(log_arma11)
autoplot(log_arma11)
checkresiduals(log_arma11)
aic_arma11 <- AIC(log_arma11)
bic_arma11 <- BIC(log_arma11)
# 6 month fcst
log_arma11_6mth <- forecast(log_arma11, h = 6)
plot(log_arma11_6mth)

# Create ARMA(2,2) model
log_arma22 <- arima(ts_log, order=c(2,0,2))
summary(log_arma22) 
autoplot(log_arma22)
checkresiduals(log_arma22)
aic_arma22 <- AIC(log_arma22)
bic_arma22 <- BIC(log_arma22)
# 6 month fcst
log_arma22_6mth <- forecast(log_arma22, h = 6)
plot(log_arma22_6mth)

# Create ARMA(1,2) model
log_arma12 <- arima(ts_log, order=c(1,0,2))
summary(log_arma12)
autoplot(log_arma12)
checkresiduals(log_arma12)
aic_arma12 <- AIC(log_arma12)
bic_arma12 <- BIC(log_arma12)
# 6 month fcst
log_arma12_6mth <- forecast(log_arma12, h = 6)
plot(log_arma12_6mth)

# Create ARMA(2,1) model
log_arma21 <- arima(ts_log, order=c(2,0,1))
summary(log_arma21)
autoplot(log_arma21)
checkresiduals(log_arma21)
aic_arma21 <- AIC(log_arma21)
bic_arma21 <- BIC(log_arma21)
# 6 month fcst
log_arma21_6mth <- forecast(log_arma21, h = 6)
plot(log_arma21_6mth)

# Model Estimates Table --------------------------------------------------------
# This part was built in our slide deck...


# END OF PART 1 ----------------------------------------------------------------
# ------------------------------------------------------------------------------


# PART 2 Full Project Spec -----------------------------------------------------
# You will describe your forecasting environment and make the following options: one-step ahead forecast (h=1),
# split your sample into two parts. Using the first 90% sample as estimation sample and the 
# rest as prediction sample.

# Use the fixed sampling scheme, consider at least three models (at least one of them is ARMA
# models, which could be AR,MA, or ARMA). You will use quadratic loss function and Mean Squared
# Error (MSE) to choose the optimal forecast. You will implement the forecast optimality tests
# (MPE and informational efficiency tests) for each model, discard any model if necessary, and add
# a simpler forecast that is calculated by averaging the last four observations (call it simple average
# 4 naive model), ft,1 = (yt + yt−1 + yt−2 + yt−3) /4. 

# When necessary, you implement the test of unconditional predictability and explain which forecast 
# is preferred. You will make combined forecasts from your top three to five models, which should 
# include ARMA models and the simple average 4 naive model. Then use three linear combination schemes:
# 1) an equal-weighted forecast,
# 2) a forecast that weights each individual forecast by the inverse of its MSE,
# 3) an OLS weighted optimal forecast. 
# You will show the weights and MSE of these three combined forecasts in a table
# format, similar to Table 9.8 on page 246, and comment on which one you prefer.


# Out of Sample - Part 2 Split Sample into Two Parts ---------------------------
# You will describe your forecasting environment and make the following options: one-step ahead forecast (h=1),
# split your sample into two parts. Using the first 90% sample as estimation sample and the 
# rest as prediction sample.

# Setting up train (90%) and test (10%)
# Logged:
train_log <- ts(diff(log(fcst$TB3)), frequency = 12, start = c(2001,1))
# Note: data runs through 2023,1
# data starts at 2001, 1
# 90% of data is 2001,1 through 2020,11
# 10% of data is 2020,12 through 2023,3


# Consider at Least 3 Models and Implement the Following -----------------------
# Use the fixed sampling scheme, consider at least three models (at least one of them is ARMA
# models, which could be AR,MA, or ARMA). You will use quadratic loss function and Mean Squared
# Error (MSE) to choose the optimal forecast. You will implement the forecast optimality tests
# (MPE and informational efficiency tests) for each model, discard any model if necessary, and add
# a simpler forecast that is calculated by averaging the last four observations (call it simple average
# 4 naive model), ft,1 = (yt + yt−1 + yt−2 + yt−3) /4. 

# Model 1 Fixed Scheme - ARMA 2 ------------------------------------------------
# setting vectors = to 27 values because 10% of our data is 27 values
fcst1 <- numeric(27)
ferror1 <- numeric(27)
loss1 <- numeric(27)

model_1 <- dynlm(train_log ~ stats::lag(train_log, -1) + stats::lag(train_log, -2),
               start = c(2001,1),
               end = c(2020,11))
summary(model_1)

for (i in 1:27) {
  fcst1[i] <- coef(model_1)[1] + coef(model_1)[2] * train_log[239+i] + coef(model_1)[3] * train_log[239+i] 
  ferror1[i] <- train_log[239+i] - fcst1[i]
  loss1[i] <- ferror1[i]^2
}

cbind(fcst1, ferror1, loss1)
MSE1 <- mean(loss1)
paste('MSE Model 1 Fixed Scheme: ', MSE1)

mpetest_1 <- lm(ferror1 ~1)
summary(mpetest_1)
IETest_1 <- lm(ferror1 ~ fcst1)
summary(IETest_1) # Informal Efficiency Test Model 1

# Model 2 Fixed Scheme -  ------------------------------------------------
# setting vectors = to 27 values because 10% of our data is 27 values
fcst2 <- numeric(27)
ferror2 <- numeric(27)
loss2 <- numeric(27)

model_2 <- dynlm(train_log ~ stats::lag(train_log, -1) + stats::lag(train_log, -2),
                 start = c(2001,1),
                 end = c(2020,11))
summary(model_1)

for (i in 1:27) {
  fcst2[i] <- coef(model_1)[1] + coef(model_1)[2] * train_log[239+i] + coef(model_1)[3] * train_log[239+i] 
  ferror2[i] <- train_log[239+i] - fcst1[i]
  loss2[i] <- ferror1[i]^2
}

cbind(fcst2, ferror2, loss2)
MSE2 <- mean(loss2)
paste('MSE Model 1 Fixed Scheme: ', MSE2)

mpetest_2 <- lm(ferror2 ~ 1)
summary(mpetest_2)
IETest_2 <- lm(ferror2 ~ fcst2)
summary(IETest_2) # Informal Efficiency Test Model 2

# Model 3 Fixed Scheme -  ------------------------------------------------
# setting vectors = to 27 values because 10% of our data is 27 values
fcst3 <- numeric(27)
ferror3 <- numeric(27)
loss3 <- numeric(27)

model_3 <- dynlm(train_log ~ stats::lag(train_log, -1) + stats::lag(train_log, -2),
                 start = c(2001,1),
                 end = c(2020,11))
summary(model_3)

for (i in 1:27) {
  fcst3[i] <- coef(model_1)[1] + coef(model_1)[2] * train_log[239+i] + coef(model_1)[3] * train_log[239+i] 
  ferror3[i] <- train_log[239+i] - fcst1[i]
  loss3[i] <- ferror1[i]^2
}

cbind(fcst3, ferror3, loss3)
MSE3 <- mean(loss3)
paste('MSE Model 3 Fixed Scheme: ', MSE3)

mpetest_3 <- lm(ferror3 ~1)
summary(mpetest_3)
IETest_3 <- lm(ferror3 ~ fcst3)
summary(IETest_3) # Informal Efficiency Test Model 3

# Model 4 (Average 4 - Simple Naive) -------------------------------------------
fcst4 <- numeric(27)
ferror4 <- numeric(27)
loss4 <- numeric(27)

for (i in 1:27){
  fcst4[i] <- (train_log[239 + i] + 
                 train_log[238 + i] +
                 train_log[237 + i] +
                 train_log[236 + i]
                 ) / 4
  ferror4[i] <- train_log[239+i] - fcst4[i]
  loss4[i] <- ferror4[i] ^ 2
}

cbind(fcst4, ferror4, loss4)
MSE4 <- mean(loss1)
paste('MSE Model 4 - Simple Naive Fixed Scheme: ', MSE4)

mpetest_4 <- lm(ferror4 ~1)
summary(mpetest_4)
IETest_4 <- lm(ferror4 ~ fcst4)
summary(IETest_4) # Informal Efficiency Test Model 4


# Combined Forecasts -----------------------------------------------------------
# When necessary, you implement the test of unconditional predictability and explain which forecast 
# is preferred. You will make combined forecasts from your top three to five models, which should 
# include ARMA models and the simple average 4 naive model. Then use three linear combination schemes:
# 1) an equal-weighted forecast,
# 2) a forecast that weights each individual forecast by the inverse of its MSE,
# 3) an OLS weighted optimal forecast.

# Example Model Following the Code in Lecture ----------------------------------
g <- window(train_log, start = c(2020,12))
comb_1 <- lm(g ~ fcst1 + fcst4)
summary(comb_1)

fcst_comb_1 <- numeric(27)
ferror_comb_1 <- numeric(27)
loss_comb_1 <- numeric(27)

fcst_comb_1 <- comb_1$fitted.values
ferror_comb_1 <- g - fcst_comb_1
loss_comb_1 <- ferror_comb_1 ^ 2

MSE_comb_1 <- mean(loss_comb_1)
paste('MSE Combination 1:', MSE_comb_1)

mpetest_comb_1 <- lm(ferror_comb_1 ~1)
summary(mpetest_comb_1)
IETest_comb_1 <- lm(ferror_comb_1 ~ fcst_comb_1)
summary(IETest_comb_1) # Informal Efficiency Test Combination Model 1


# END OF PART 2


# Code Graveyard ---------------------------------------------------------------
# This section houses test code and models we ran that we determined to not be 
# immediately relevant. The remaining code in this file is only for reference.

# ACF, PACF, White-Noise
# Based on the ACF and PACF, you will choose three linear models (MA, AR, or ARMA) 
# and estimate them.

# Time Series
acf(time_series, lag.max = 20, plot = TRUE)
acf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(time_series, lag.max = 20, plot = TRUE)
pacf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
Box.test(time_series, type = "Ljung-Box")


# Time Series
acf(time_series, lag.max = 20, plot = TRUE)
acf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(time_series, lag.max = 20, plot = TRUE)
pacf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
Box.test(time_series, type = "Ljung-Box")


# NOT LOGGED Complex Forecast Models (MA, AR, ARMA)
# Create MA(1) Model 
ma1 <- arima(time_series, order=c(0,0,1))
summary(ma1)
autoplot(ma1)
ma1_plots <- checkresiduals(ma1)
ma1_plots
aic_ma1 <- AIC(ma1)
bic_ma1 <- BIC(ma1)
# 6 month fcst
ma1_6mth <- forecast(ma1, h = 6)
plot(ma1_6mth)

# Create MA(2) Model
ma2 <- arima(time_series, order=c(0,0,2))
summary(ma2) 
autoplot(ma2)
checkresiduals(ma2)
aic_ma2 <- AIC(ma2)
bic_ma2 <- BIC(ma2)
# 6 month fcst
ma2_6mth <- forecast(ma2, h = 6)
plot(ma2_6mth)

# Create AR(1) model
ar1 <- arima(time_series, order=c(1,0,0))
summary(ar1)
autoplot(ar1)
checkresiduals(ar1)
aic_ar1 <- AIC(ar1)
bic_ar1 <- BIC(ar1)
# 6 month fcst
ar1_6mth <- forecast(ar1, h = 6)
plot(ar1_6mth)

# Create AR(2) model
ar2 <- arima(time_series, order=c(2,0,0))
summary(ar2)
autoplot(ar2)
checkresiduals(ar2)
aic_ar2 <- AIC(ar2)
bic_ar2 <- BIC(ar2)
# 6 month fcst
ar2_6mth <- forecast(ar2, h = 6)
plot(ar2_6mth)

# Create ARMA(1,1) model
arma11 <- arima(time_series, order=c(1,0,1))
summary(arma11)
autoplot(arma11)
checkresiduals(arma11)
aic_arma11 <- AIC(arma11)
bic_arma11 <- BIC(arma11)
# 6 month fcst
arma11_6mth <- forecast(arma11, h = 6)
plot(arma11_6mth)

# Create ARMA(2,2) model
arma22 <- arima(time_series, order=c(2,0,2))
summary(arma22) 
autoplot(arma22)
checkresiduals(arma22)
aic_arma22 <- AIC(arma22)
bic_arma22 <- BIC(arma22)
# 6 month fcst
arma22_6mth <- forecast(arma22, h = 6)
plot(arma22_6mth)

# Create ARMA(1,2) model
arma12 <- arima(time_series, order=c(1,0,2))
summary(arma12)
autoplot(arma12)
checkresiduals(arma12)
aic_arma12 <- AIC(arma12)
bic_arma12 <- BIC(arma12)
# 6 month fcst
arma12_6mth <- forecast(arma12, h = 6)
plot(arma12_6mth)

# Create ARMA(2,1) model
arma21 <- arima(time_series, order=c(2,0,1))
summary(arma21)
autoplot(arma21)
checkresiduals(arma21)
aic_arma21 <- AIC(arma21)
bic_arma21 <- BIC(arma21)
# 6 month fcst
arma21_6mth <- forecast(arma21, h = 6)
plot(ma1_6mth)

# LOGGED MODELS THAT WERE DISCARDED --------------------------------------------
# Create MA(1) Model 
log_ma1 <- arima(ts_log, order=c(0,0,1))
summary(log_ma1)
autoplot(log_ma1)
checkresiduals(log_ma1)
aic_ma1 <- AIC(log_ma1)
bic_ma1 <- BIC(log_ma1)
# 6 month fcst
log_ma1_6mth <- forecast(log_ma1, h = 6)
plot(log_ma1_6mth)

# Create MA(2) Model
log_ma2 <- arima(ts_log, order=c(0,0,2))
summary(log_ma2) 
autoplot(log_ma2)
checkresiduals(log_ma2)
aic_ma2 <- AIC(log_ma2)
bic_ma2 <- BIC(log_ma2)
# 6 month fcst
log_ma2_6mth <- forecast(log_ma2, h = 6)
plot(log_ma2_6mth)

# Create AR(1) model
log_ar1 <- arima(ts_log, order=c(1,0,0))
summary(log_ar1)
autoplot(log_ar1)
checkresiduals(log_ar1)
aic_ar1 <- AIC(log_ar1)
bic_ar1 <- BIC(log_ar1)
# 6 month fcst
log_ar1_6mth <- forecast(log_ar1, h = 6)
plot(log_ar1_6mth)

# Create AR(2) model
log_ar2 <- arima(ts_log, order=c(2,0,0))
summary(log_ar2)
autoplot(log_ar2)
checkresiduals(log_ar2)
aic_ar2 <- AIC(log_ar2)
bic_ar2 <- BIC(log_ar2)
# 6 month fcst
log_ar2_6mth <- forecast(log_ar2, h = 6)
plot(log_ar2_6mth)







