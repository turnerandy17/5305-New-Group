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


# ACF and PACF -----------------------------------------------------------------
# ACF and PACF Time Series
acf(time_series, lag.max = 20, plot = TRUE)
acf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(time_series, lag.max = 20, plot = TRUE)
pacf(time_series, lag.max = 20, plot = FALSE) # prints autocorrelations

# ACF and PACF Logged Time Series
acf(ts_log, lag.max = 20, plot = TRUE)
acf(ts_log, lag.max = 20, plot = FALSE) # prints autocorrelations
pacf(ts_log, lag.max = 20, plot = TRUE)
pacf(ts_log, lag.max = 20, plot = FALSE) # prints autocorrelations

# NEED TO DETERMINE WHAT ACF PACF INTEREPRETATIONS ARE...

# Simple Forecast Models -------------------------------------------------------













