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

time_series <- ts(fcst$TB3, frequency = 12, start = c(2001,1)) # I think we should only focus on 2010 forward
adf.test(time_series) # initial Dickey-Fuller Test NEED HELP INTERPRETING
adf_one <- adf.test(time_series) # saving as variable in case needed later

# Take log difference in case this is needed - not sure if it is
ts_log <- diff(log(time_series))
adf.test(ts_log) # log Dickey-Fuller Test NEED HELP INTERPRETING
adf_two <- adf.test(ts_log) # saving as variable in case needed later

# KPSS test for Stationarity - not sure if needed
time_series %>% ur.kpss() %>% summary()
ts_log %>% ur.kpss() %>% summary()

# ACTION: DETERMINE HOW TO INTERPRET ADF AND IF LOG DIFF IS NEEDED.


# ACF, PACF, White-Noise -------------------------------------------------------
# Based on the ACF and PACF, you will choose three linear models (MA, AR, or ARMA) 
# and estimate them.

# Time Series
acf(time_series, lag.max = 20, plot = TRUE)
acf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(time_series, lag.max = 20, plot = TRUE)
pacf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
Box.test(time_series, type = "Ljung-Box")

# Logged Time Series
acf(ts_log, lag.max = 20, plot = TRUE)
acf(ts_log, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(ts_log, lag.max = 20, plot = TRUE)
pacf(ts_log, lag.max = 20, plot = FALSE) # prints autocorrelations
Box.test(ts_log, type = 'Ljung-Box')

# ACTION: NEED TO DETERMINE WHAT INTEREPRETATIONS ARE FOR ACF/PACF...
 


# PART 1 Forecast Models (lines 112:300)----------------------------------------
# Based on the ACF and PACF, you will choose three linear models (MA, AR, or ARMA) and estimate them.
# You will present the estimation results, show the ACF and PACF correlograms of residuals from
# each specification, and verify they are white noise using Q-Test.
# You will summarize all the model estimation and evaluation in a table (refer to table 8.2 
# on page 214 in your textbook) and make six-period ahead forecasts. You will plot the multistep
# of forecasts and their correspondence bands for each specification and comment on your preferred model and why.

# NOT LOGGED Complex Forecast Models (MA, AR, ARMA) ----------------------------
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


# LOGGED Complex Forecast Models (MA, AR, ARMA) --------------------------------
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
# ACTION: Either build this out in Rstudio or put together a regular table in the deck

# END OF PART 1






# PART 2 -----------------------------------------------------------------------

# Out of Sample - Part 2 -------------------------------------------------------
train <- window(time_series, start = c(1960,1), end = c(2016,11))
test <- window(time_series, start = c(2016,12), end = c(2023,3))

# NOTE: WE NEED TO PICK A COUPLE OF THE MODELS FROM ABOVE. I CHOSE THESE RANDOMLY
# ARMA(2,1) Model
arma21_train <- arima(train, order=c(2,0,1)) #create model with training set
arma21_fcst <- forecast(arma21_train, h = 6) #create forecast
errors_arma21 <- forecast::accuracy(arma21_fcst, test) %>% as.data.frame()
mae_arma21<-errors_arma21["MAE"][2,1]
mae_arma21_train<-errors_arma21["MAE"][1,1]

# MA(2) Model
ma2_train <- arima(train, order=c(0,0,2))
ma2_fcst <- forecast(ma2_train, h = 6)
errors_ma2 <- forecast::accuracy(ma2_fcst, test) %>% as.data.frame()
mae_ma2 <- errors_ma2["MAE"][2,1]
mae_ma2_train <- errors_ma2["MAE"][1,1]

# AR(1) Model
ar1_train <- arima(train, order=c(1,0,0))
ar1_fcst <- forecast(ar1_train, h=20)
errors_ar1<-forecast::accuracy(ar1_fcst, test) %>% as.data.frame()
mae_ar1<-errors_ar1["MAE"][2,1]
mae_ar1_train<-errors_ar1["MAE"][1,1]

#Build results table
MAE_Training<-c(mae_arma21_train, mae_ar1_train, mae_ma2_train)
MAE_Test<-c(mae_arma21, mae_ar1, mae_ma2)
Model<-c("ARMA(2,1)","AR(1)", "MA(2)")
forecasting_error_results<-data.frame(Model, MAE_Training, MAE_Test)


#Stats Test: Validate Results from Out of Sample Evaluation --------------------
# t testing errors
error_arma21<-arma21_fcst$residuals
error_ar1<-ar1_fcst$residuals
error_ma2<-ma2_fcst$residuals

# output results
t.test(error_arma21, error_ar1)
t.test(error_arma21, error_ma2)
t.test(error_ar1, error_ma2)























