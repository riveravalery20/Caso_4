library(readr)
library(tidyverse)
library(lubridate)
library(fpp2)
library(tseries)
library(urca)

CHILE <- read_csv("ECONOMÍA AZUL-CHILE contaminación.csv")

CHILE_ts <- CHILE %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  mutate(
    date = parse_date_time(TIME_PERIOD, orders = c("ym", "Y-m", "Y-m-d", "Ymd")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(date) & !is.na(OBS_VALUE)) %>%
  arrange(date)

serie_CHILE <- ts(
  CHILE_ts$OBS_VALUE,
  start = c(year(min(CHILE_ts$date)), month(min(CHILE_ts$date))),
  frequency = 12
)

autoplot(serie_CHILE)
serie_CHILE
ggAcf(serie_CHILE)
diff(serie_CHILE)
autoplot(diff(serie_CHILE))
ggAcf(diff(serie_CHILE))

Box.test(serie_CHILE, lag=10, type="Ljung-Box")
Box.test(diff(serie_CHILE), lag=10, type="Ljung-Box")

ggAcf(serie_CHILE)
ggPacf(serie_CHILE)
ggtsdisplay(serie_CHILE)

summary(ur.kpss(serie_CHILE))
adf.test(serie_CHILE)
adf.test(diff(serie_CHILE))

fit <- auto.arima(serie_CHILE, seasonal=TRUE)
fit
fit %>% forecast(h=12) %>% autoplot(include=36)

fit2 <- Arima(serie_CHILE, order=c(3,0,0), seasonal=list(order=c(0,1,1), period=12))
fit2
