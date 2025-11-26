library(readxl)
library(lubridate)
library(tidyverse)
library(fpp2)
library(tseries)
library(urca)

FRANCIA <- read_excel("ECONOMÍA VERDE-FRANCIA.xlsx")

FRANCIA_sep <- FRANCIA %>%
  separate(
    col = `STRUCTURE,STRUCTURE_ID,STRUCTURE_NAME,ACTION,REF_AREA,,FREQ,,MEASURE,,UNIT_MEASURE,,GROUP,,TIME_PERIOD,,OBS_VALUE,,OBS_STATUS,,OBS_STATUS_2,,OBS_STATUS_3,,OBS_STATUS_4,,UNIT_MULT,,PRICE_BASE,,DECIMALS,,CONVERSION_TYPE,,BASE_PER,`,
    into = c("STRUCTURE","STRUCTURE_ID","STRUCTURE_NAME","ACTION","REF_AREA","VACIO1",
             "FREQ","VACIO2","MEASURE","VACIO3","UNIT_MEASURE","VACIO4",
             "GROUP","VACIO5","TIME_PERIOD","VACIO6","OBS_VALUE","VACIO7",
             "OBS_STATUS","VACIO8","OBS_STATUS_2","VACIO9","OBS_STATUS_3",
             "VACIO10","OBS_STATUS_4","VACIO11","UNIT_MULT","VACIO12",
             "PRICE_BASE","VACIO13","DECIMALS","VACIO14","CONVERSION_TYPE","VACIO15","BASE_PER"),
    sep = ",", fill = "right"
  )

FRANCIA_ts <- FRANCIA_sep %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(TIME_PERIOD) & !is.na(OBS_VALUE)) %>%
  arrange(TIME_PERIOD)

serie_FRANCIA <- ts(FRANCIA_ts$OBS_VALUE,
                    start = min(FRANCIA_ts$TIME_PERIOD),
                    frequency = 1)

serie_FRANCIA <- window(serie_FRANCIA, end = c(2024, 1))

autoplot(serie_FRANCIA)
serie_FRANCIA
ggAcf(serie_FRANCIA)
diff(serie_FRANCIA)
autoplot(diff(serie_FRANCIA))
ggAcf(diff(serie_FRANCIA))

Box.test(serie_FRANCIA, lag=10, type="Ljung-Box")
Box.test(diff(serie_FRANCIA), lag=10, type="Ljung-Box")

ggAcf(serie_FRANCIA)
ggPacf(serie_FRANCIA)
ggtsdisplay(serie_FRANCIA)

summary(ur.kpss(serie_FRANCIA))
adf.test(serie_FRANCIA)
adf.test(diff(serie_FRANCIA))

fit <- auto.arima(serie_FRANCIA, seasonal=FALSE)
fit
fit %>% forecast(h=10) %>% autoplot(include=20)

fit2 <- Arima(serie_FRANCIA, order=c(3,0,0))
fit2
summary(ur.kpss(serie_FRANCIA))
adf.test(serie_FRANCIA)
adf.test(diff(serie_FRANCIA))

fit <- auto.arima(serie_FRANCIA, seasonal=FALSE)
fit
fit %>% forecast(h=10) %>% autoplot(include=20)

fit2 <- Arima(serie_FRANCIA, order=c(3,0,0))
fit2

