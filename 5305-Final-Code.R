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
raw_data <- read_excel("ch3_COI_3MTBill.xls")
View(raw_data)

fcst <- raw_data %>%
  select(-'CPI') %>%
  rename(date = 'observation_date',
         TB3 = 'TB3Month')

fcst <- fcst %>%
  mutate(date = date(date), #convert to date
         TB3 = as.numeric(TB3)) #convert rate to numeric

tb3 <- fcst %>%
  select(TB3)

summary(fcst) # give simple summary
vtable(fcst) # give aesthetic summary for deck


# Exploratory Graph -----------------------------------------------------------

# setting up simple line graph
ex_graph <- ggplot(fcst, mapping = aes(date, TB3)) +
  geom_line() +
  labs(x = "Date",
       y = "Rate",
       title = "3 Month T-Bill Rate") +
  scale_x_date(date_breaks = "5 year",
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
time_series <- ts(fcst$TB3, frequency = 12) # I think we should only focus on 2010 forward
adf.test(time_series) # initial Dickey-Fuller Test NEED HELP INTERPRETING
adf_one <- adf.test(time_series) # saving as variable in case needed later

# Take log difference in case this is needed - not sure if it is
ts_log <- diff(log(time_series))
adf.test(ts_log) # log Dickey-Fuller Test NEED HELP INTERPRETING
adf_two <- adf.test(ts_log) # saving as variable in case needed later

# KPSS test for Stationarity - not sure if needed
time_series %>% ur.kpss() %>% summary()
ts_log %>% ur.kpss() %>% summary()


# ACF, PACF, White-Noise -------------------------------------------------------
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

# NEED TO DETERMINE WHAT  INTEREPRETATIONS ARE...
 

# Simple Forecast Models -------------------------------------------------------
lm_1 <- lm(TB3~date, data = fcst)
summary(lm_1)
lm_aic <- AIC(lm_1)
lm_bic <- BIC(lm_1) # referred to differently in class
lm_1_plots <- checkresiduals(lm_1)
lm_1_plots

# WANT TO ADD RECURSIVE, ROLLING, FIXED, etc.


# NOT LOGGED Complex Forecast Models (MA, AR, ARMA) ----------------------------
# Create MA(1) Model 
ma1 <- arima(time_series, order=c(0,0,1))
summary(ma1)
autoplot(ma1)
checkresiduals(ma1)
aic_ma1 <- AIC(ma1)
bic_ma1 <- BIC(ma1)

# Create MA(2) Model
ma2 <- arima(time_series, order=c(0,0,2))
summary(ma2) 
autoplot(ma2)
checkresiduals(ma2)
aic_ma2 <- AIC(ma2)
bic_ma2 <- BIC(ma2)

# Create AR(1) model
ar1 <- arima(time_series, order=c(1,0,0))
summary(ar1)
autoplot(ar1)
checkresiduals(ar1)
aic_ar1 <- AIC(ar1)
bic_ar1 <- BIC(ar1)

# Create AR(2) model
ar2 <- arima(time_series, order=c(2,0,0))
summary(ar2)
autoplot(ar2)
checkresiduals(ar2)
aic_ar2 <- AIC(ar2)
bic_ar2 <- BIC(ar2)

# Create ARMA(1,1) model
arma11 <- arima(time_series, order=c(1,0,1))
summary(arma11)
autoplot(arma11)
checkresiduals(arma11)
aic_arma11 <- AIC(arma11)
bic_arma11 <- BIC(arma11)

# Create ARMA(2,2) model
arma22 <- arima(time_series, order=c(2,0,2))
summary(arma22) 
autoplot(arma22)
checkresiduals(arma22)
aic_arma22 <- AIC(arma22)
bic_arma22 <- BIC(arma22)

# Create ARMA(1,2) model
arma12 <- arima(time_series, order=c(1,0,2))
summary(arma12)
autoplot(arma12)
checkresiduals(arma12)
aic_arma12 <- AIC(arma12)
bic_arma12 <- BIC(arma12)

# Create ARMA(2,1) model
arma21 <- arima(time_series, order=c(2,0,1))
summary(arma21)
autoplot(arma21)
checkresiduals(arma21)
aic_arma21 <- AIC(arma21)
bic_arma21 <- BIC(arma21)


# LOGGED Complex Forecast Models (MA, AR, ARMA) --------------------------------
# Create MA(1) Model 
log_ma1 <- arima(ts_log, order=c(0,0,1))
summary(ma1)
autoplot(ma1)
checkresiduals(ma1)
aic_ma1 <- AIC(ma1)
bic_ma1 <- BIC(ma1)

# Create MA(2) Model
log_ma2 <- arima(ts_log, order=c(0,0,2))
summary(ma2) 
autoplot(ma2)
checkresiduals(ma2)
aic_ma2 <- AIC(ma2)
bic_ma2 <- BIC(ma2)

# Create AR(1) model
log_ar1 <- arima(ts_log, order=c(1,0,0))
summary(ar1)
autoplot(ar1)
checkresiduals(ar1)
aic_ar1 <- AIC(ar1)
bic_ar1 <- BIC(ar1)

# Create AR(2) model
log_ar2 <- arima(ts_log, order=c(2,0,0))
summary(ar2)
autoplot(ar2)
checkresiduals(ar2)
aic_ar2 <- AIC(ar2)
bic_ar2 <- BIC(ar2)

# Create ARMA(1,1) model
log_arma11 <- arima(ts_log, order=c(1,0,1))
summary(arma11)
autoplot(arma11)
checkresiduals(arma11)
aic_arma11 <- AIC(arma11)
bic_arma11 <- BIC(arma11)

# Create ARMA(2,2) model
log_arma22 <- arima(ts_log, order=c(2,0,2))
summary(arma22) 
autoplot(arma22)
checkresiduals(arma22)
aic_arma22 <- AIC(arma22)
bic_arma22 <- BIC(arma22)

# Create ARMA(1,2) model
log_arma12 <- arima(ts_log, order=c(1,0,2))
summary(arma12)
autoplot(arma12)
checkresiduals(arma12)
aic_arma12 <- AIC(arma12)
bic_arma12 <- BIC(arma12)

# Create ARMA(2,1) model
log_arma21 <- arima(ts_log, order=c(2,0,1))
summary(arma21)
autoplot(arma21)
checkresiduals(arma21)
aic_arma21 <- AIC(arma21)
bic_arma21 <- BIC(arma21)


