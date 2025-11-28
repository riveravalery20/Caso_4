library(readr)
library(tidyverse)
library(lubridate)
library(fpp2)
library(tseries)
library(urca)

ESPAÑA <- read_csv("PRODUCTIVIDAD EN ESPAÑA.csv")

ESPAÑA_ts <- ESPAÑA %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  mutate(
    date = parse_date_time(TIME_PERIOD, orders = c("ym", "Y-m", "Y-m-d", "Ymd")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(date) & !is.na(OBS_VALUE)) %>%
  arrange(date)

serie_ESPAÑA <- ts(
  ESPAÑA_ts$OBS_VALUE,
  start = c(year(min(ESPAÑA_ts$date)), month(min(ESPAÑA_ts$date))),
  frequency = 12
)

autoplot(serie_ESPAÑA)
serie_ESPAÑA
ggAcf(serie_ESPAÑA)
diff(serie_ESPAÑA)
autoplot(diff(serie_ESPAÑA))
ggAcf(diff(serie_ESPAÑA))

Box.test(serie_ESPAÑA, lag=10, type="Ljung-Box")
Box.test(diff(serie_ESPAÑA), lag=10, type="Ljung-Box")

ggAcf(serie_ESPAÑA)
ggPacf(serie_ESPAÑA)
ggtsdisplay(serie_ESPAÑA)

summary(ur.kpss(serie_ESPAÑA))
adf.test(serie_ESPAÑA)
adf.test(diff(serie_ESPAÑA))

fit <- auto.arima(serie_ESPAÑA, seasonal=TRUE)
fit
fit %>% forecast(h=12) %>% autoplot(include=36)

fit2 <- Arima(serie_ESPAÑA, order=c(3,0,0), seasonal=list(order=c(0,1,1), period=12))
fit2
