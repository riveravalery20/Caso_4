
library(readxl)
library(lubridate)

JAPON <- read_excel("HORAS TRABAJADAS POR TRABAJADOR-JAPON.xlsx")

head(JAPON)

colnames(JAPON)

JAPON_sep <- JAPON %>%
  separate(col = `STRUCTURE,STRUCTURE_ID,STRUCTURE_NAME,ACTION,REF_AREA,,MEASURE,,FREQ,,TIME_PERIOD,,OBS_VALUE,,OBS_STATUS,,UNIT_MEASURE,,UNIT_MULT,,CURRENCY,,BASE_PER,,METHODOLOGY,,DECIMALS,,PRICE_BASE,,Aserie_JAPONUSTMENT,`,
           into = c("STRUCTURE","STRUCTURE_ID","STRUCTURE_NAME","ACTION","REF_AREA","VACIO1","MEASURE","VACIO2","FREQ","VACIO3","TIME_PERIOD","VACIO4","OBS_VALUE","VACIO5","OBS_STATUS","VACIO6","UNIT_MEASURE","VACIO7","UNIT_MULT","VACIO8","CURRENCY","VACIO9","BASE_PER","VACIO10","METHODOLOGY","VACIO11","DECIMALS","VACIO12","PRICE_BASE","VACIO13","ADJUSTMENT"), 
           sep = ",", fill = "right")

JAPON_ts <- JAPON_sep %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(TIME_PERIOD) & !is.na(OBS_VALUE)) %>%
  arrange(TIME_PERIOD)

# Crear la serie de tiempo (ejemplo para datos anuales)
serie_JAPON <- ts(JAPON_ts$OBS_VALUE, start = min(JAPON_ts$TIME_PERIOD), frequency = 1)

print(serie_JAPON)

plot(serie_JAPON, main = "Serie Temporal Anual", ylab = "Valor", xlab = "Año")



#MODELO----------------------

library(fpp2)
library(tidyverse)

autoplot(serie_JAPON)
serie_JAPON
ggAcf(serie_JAPON)
diff(serie_JAPON)
autoplot(diff(serie_JAPON))
ggAcf(diff(serie_JAPON))

Box.test(serie_JAPON, lag=10, type="Ljung-Box")
Box.test(diff(serie_JAPON), lag=10, type="Ljung-Box")

ggAcf(serie_JAPON)
ggPacf(serie_JAPON)
ggtsdisplay(serie_JAPON)

library(tseries)
library(urca)
summary(ur.kpss(serie_JAPON))
adf.test(serie_JAPON)
adf.test(diff(serie_JAPON))

fit <- auto.arima(serie_JAPON, seasonal=FALSE)
fit
fit %>% forecast(h=10) %>% autoplot(include=80)

fit2 <- Arima(serie_JAPON, order=c(3,0,0))
fit2
