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

# Based on the above Our PACF plot does have a significant spike at lag 1, however since there are other
# lags with significant spikes and the ACF plot also shows other significant spikes, we am leaning towards ARMA.
# For the ACF and PACF interpretations, both ACF and PACF plots show significant spikes at different lags,
# which suggests a possible ARMA model for our log time series.

# PART 1 Forecast Models (lines 114:194)----------------------------------------
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
coef_est_log_arma11 <- coef(log_arma11)
# Obtain the standard errors of the coefficient estimates
coef_se_log_arma11 <- sqrt(diag(vcov(log_arma11)))
# Calculate the t-ratios
t_ratios_log_arma11 <- coef_est_log_arma11  / coef_se_log_arma11
paste('T-ratios Logged Model ARMA 1,1: ', t_ratios_log_arma11)

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
coef_est_log_arma22 <- coef(log_arma22)
# Obtain the standard errors of the coefficient estimates
coef_se_log_arma22 <- sqrt(diag(vcov(log_arma22)))
# Calculate the t-ratios
t_ratios_log_arma22 <- coef_est_log_arma22  / coef_se_log_arma22
paste('T-ratios Logged Model ARMA 2,2: ', t_ratios_log_arma22)

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
coef_est_log_arma12 <- coef(log_arma12)
# Obtain the standard errors of the coefficient estimates
coef_se_log_arma12 <- sqrt(diag(vcov(log_arma12)))
# Calculate the t-ratios
t_ratios_log_arma12 <- coef_est_log_arma12  / coef_se_log_arma12
paste('T-ratios Logged Model ARMA 1,2: ', t_ratios_log_arma12)

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
coef_est_log_arma21 <- coef(log_arma21)
# Obtain the standard errors of the coefficient estimates
coef_se_log_arma21 <- sqrt(diag(vcov(log_arma21)))
# Calculate the t-ratios
t_ratios_log_arma21 <- coef_est_log_arma21  / coef_se_log_arma21
paste('T-ratios Logged Model ARMA 2,1: ', t_ratios_log_arma21)

# Model Estimates Table --------------------------------------------------------
# This part was built in our slide deck...



# END OF PART 1 ----------------------------------------------------------------
# ------------------------------------------------------------------------------
# BEGINNING OF PART 2 ----------------------------------------------------------

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

# Model 1 Fixed Scheme - AR3 ---------------------------------------------------
# setting vectors = to 27 values because 10% of our data is 27 values
fcst1 <- numeric(27)
ferror1 <- numeric(27)
loss1 <- numeric(27)

model_1 <- dynlm(train_log ~ stats::lag(train_log, -1) + stats::lag(train_log, -3),
               start = c(2001,1),
               end = c(2020,11))
summary(model_1)

for (i in 1:27) {
  fcst1[i] <- coef(model_1)[1] + coef(model_1)[2] * train_log[238+i] + coef(model_1)[3] * train_log[236+i] 
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

# Model 2 Fixed Scheme - AR 1 ------------------------------------------------
# setting vectors = to 27 values because 10% of our data is 27 values
fcst2 <- numeric(27)
ferror2 <- numeric(27)
loss2 <- numeric(27)

model_2 <- dynlm(train_log ~ stats::lag(train_log, -1),
                 start = c(2001,1),
                 end = c(2020,11))
summary(model_2)

for (i in 1:27) {
  fcst2[i] <- coef(model_2)[1] + coef(model_2)[2] * train_log[238+i]
  ferror2[i] <- train_log[239+i] - fcst2[i]
  loss2[i] <- ferror1[i]^2
}

cbind(fcst2, ferror2, loss2)
MSE2 <- mean(loss2)
paste('MSE Model 2 Fixed Scheme: ', MSE2)

mpetest_2 <- lm(ferror2 ~ 1)
summary(mpetest_2)
IETest_2 <- lm(ferror2 ~ fcst2)
summary(IETest_2) # Informal Efficiency Test Model 2

# Model 3 Fixed Scheme - ARMA --------------------------------------------------
# setting vectors = to 27 values because 10% of our data is 27 values
fcst3 <- numeric(27)
ferror3 <- numeric(27)
loss3 <- numeric(27)

model_3 <- dynlm(train_log ~ stats::lag(train_log, -1) + 
                   stats::lag(train_log, -2) +
                   stats::lag(train_log, -4),
                 start = c(2001,1),
                 end = c(2020,11))
summary(model_3)

for (i in 1:27) {
  fcst3[i] <- coef(model_3)[1] + coef(model_3)[2] * train_log[238+i] + coef(model_3)[3] * train_log[237+i] + coef(model_3)[4] * train_log[235+i]
  ferror3[i] <- train_log[239+i] - fcst3[i]
  loss3[i] <- ferror3[i]^2
}

cbind(fcst3, ferror3, loss3)
MSE3 <- mean(loss3)
paste('MSE Model 3 Fixed Scheme: ', MSE3)

mpetest_3 <- lm(ferror3 ~ 1)
summary(mpetest_3)
IETest_3 <- lm(ferror3 ~ fcst3)
summary(IETest_3) # Informal Efficiency Test Model 3

# Model 4 (Average 4 - Simple Naive) -------------------------------------------
fcst4 <- numeric(27)
ferror4 <- numeric(27)
loss4 <- numeric(27)

model_4 <- dynlm(train_log ~ stats::lag(train_log, -1) + stats::lag(train_log, -1),
                 start = c(2001,1),
                 end = c(2020,11))
summary(model_4)

for (i in 1:27){
  fcst4[i] <- (train_log[238 + i] + 
                 train_log[237 + i] +
                 train_log[236 + i] +
                 train_log[235 + i]
                 ) / 4
  ferror4[i] <- train_log[239+i] - fcst4[i]
  loss4[i] <- ferror4[i] ^ 2
}

cbind(fcst4, ferror4, loss4)
MSE4 <- mean(loss4)
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

# Equal Weighted Forecast ------------------------------------------------------
forecast_1 <- as.vector(fitted(model_1))
forecast_2 <- as.vector(fitted(model_2))
forecast_3 <- as.vector(fitted(model_3))
forecast_4 <- as.vector(fitted(model_4))

# Find the minimum length
min_length <- min(length(forecast_1), length(forecast_2), length(forecast_3), length(forecast_4))

# Trim the forecast vectors
forecast_1 <- forecast_1[1:min_length]
forecast_2 <- forecast_2[1:min_length]
forecast_3 <- forecast_3[1:min_length]
forecast_4 <- forecast_4[1:min_length]

# Calc Weights
n_models <- 4
combined_forecast_ew <- (forecast_1 + forecast_2 + forecast_3 + forecast_4) / n_models

fcst_ew <- numeric(27)
ferror_ew <- numeric(27)
loss_ew <- numeric(27)

fcst_ew <- combined_forecast_ew[(length(combined_forecast_ew) - 26):length(combined_forecast_ew)]

for (i in 1:27) {
  ferror_ew[i] <- train_log[length(train_log) - 27 + i] - fcst_ew[i]
  loss_ew[i] <- ferror_ew[i]^2
}

MSE_ew <- mean(loss_ew)
paste('MSE Combination Equal Weights Fcst:', MSE_ew)

mpetest_ew <- lm(ferror_ew ~ 1)
summary(mpetest_ew)
IETest_ew <- lm(ferror_ew ~ fcst_ew)
summary(IETest_ew) # Informal Efficiency Test Combination Equal Weights

# Weighted Individual by Inverse MSE -------------------------------------------
forecast_1_iMSE <- as.vector(fitted(model_1))
forecast_2_iMSE <- as.vector(fitted(model_2))
forecast_3_iMSE <- as.vector(fitted(model_3))
forecast_4_iMSE <- as.vector(fitted(model_4))

# Find the minimum length
min_length <- min(length(forecast_1_iMSE), length(forecast_2_iMSE), length(forecast_3_iMSE), length(forecast_4_iMSE))

# Trim the forecast vectors
forecast_1_iMSE <- forecast_1_iMSE[1:min_length]
forecast_2_iMSE <- forecast_2_iMSE[1:min_length]
forecast_3_iMSE <- forecast_3_iMSE[1:min_length]
forecast_4_iMSE <- forecast_4_iMSE[1:min_length]

# Calculate inverse of MSE
mse_inverse <- 1 / c(MSE1, MSE2, MSE3, MSE4)
mse_inverse <- na.omit(mse_inverse)

# Set up forecast model
fcst_iMSE <- numeric(27)
ferror_iMSE <- numeric(27)
loss_iMSE <- numeric(27)

combined_iMSE_forecast <- forecast_1_iMSE + forecast_2_iMSE + forecast_3_iMSE + forecast_4_iMSE

fcst_iMSE <- combined_iMSE_forecast[(length(combined_iMSE_forecast) - 26):length(combined_iMSE_forecast)]

for (i in 1:27) {
  ferror_iMSE[i] <- train_log[length(train_log) - 27 + i] - fcst_iMSE[i]
  loss_iMSE[i] <- ferror_iMSE[i]^2
}

MSE_iMSE <- mean(loss_iMSE)
paste('MSE Inverse Fcst:', MSE_iMSE)

mpetest_iMSE <- lm(ferror_iMSE ~1)
summary(mpetest_iMSE)
IETest_iMSE <- lm(ferror_ew ~ fcst_iMSE)
summary(IETest_iMSE) # Informal Efficiency Test Combination Equal Weights


# OLS Weighted Optimal Fcst ----------------------------------------------------
forecast_1_ols <- as.vector(fitted(model_1))
forecast_2_ols <- as.vector(fitted(model_2))
forecast_3_ols <- as.vector(fitted(model_3))
forecast_4_ols <- as.vector(fitted(model_4))

# Find the minimum length
min_length <- min(length(forecast_1_ols), length(forecast_2_ols), length(forecast_3_ols), length(forecast_4_ols))

# Trim the forecast vectors
forecast_1_ols <- forecast_1_ols[1:min_length]
forecast_2_ols <- forecast_2_ols[1:min_length]
forecast_3_ols <- forecast_3_ols[1:min_length]
forecast_4_ols <- forecast_4_ols[1:min_length]

# Calculate equal weights
combined_ols_forecast <- (forecast_1_ols + forecast_2_ols + forecast_3_ols + forecast_4_ols)

fcst_ols <- numeric(27)
ferror_ols <- numeric(27)
loss_ols <- numeric(27)

fcst_ols <- combined_ols_forecast[(length(combined_ols_forecast) - 26):length(combined_ols_forecast)]

for (i in 1:27) {
  ferror_ols[i] <- train_log[length(train_log) - 27 + i] - fcst_ols[i]
  loss_ols[i] <- ferror_ols[i]^2
}

MSE_ols <- mean(loss_ols)
paste('MSE Combination Ordinary Lease Squares (OLS) Fcst:', MSE_ols)

mpetest_ols <- lm(ferror_ols ~ 1)
summary(mpetest_ols)
IETest_ols <- lm(ferror_ols ~ fcst_ols)
summary(IETest_ols) # Informal Efficiency Test Combination Equal Weights

# END OF PART 2

