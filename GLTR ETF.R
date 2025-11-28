library(quantmod)
library(tseries)
library(fpp2)
library(gridExtra)
library(tidyverse)

AccionesEX <- getSymbols("GLTR", src = "yahoo", auto.assign = FALSE, 
                         from = "2022-10-31")

names(AccionesEX)
GLTR <- AccionesEX$GLTR.Close
plot(GLTR, main = "Precio de cierre GLTR")


# Ventana de entrenamiento
Entrenamiento_GLTR <- window(GLTR, start = "2022-10-31", end = "2025-10-31")

# Ventana de prueba
Prueba_GLTR <- window(GLTR, start = "2025-11-03")

autoplot(Entrenamiento_GLTR, main = "Serie de entrenamiento GLTR")


# ACF de la serie original
ggAcf(Entrenamiento_GLTR)
adf.test(Entrenamiento_GLTR)# p-value >> 0.05: la serie NO es estacionaria, requiere diferenciación

GLTR_diff <- diff(Entrenamiento_GLTR) %>% na.omit()

# Visualización de la serie diferenciada
autoplot(GLTR_diff, main = "Serie diferenciada GLTR")

# ACF de la serie diferenciada
ggAcf(GLTR_diff) # Prueba ADF en serie dif p-value < 0.05: la serie dif SÍ es estacionaria
adf.test(GLTR_diff)

# Gráficos ACF y PACF
grid.arrange(ggAcf(GLTR_diff),
             ggPacf(GLTR_diff),
             nrow = 1)

# MODELOS ARIMA

Modelo_GLTR_auto <- auto.arima(Entrenamiento_GLTR)

Modelo_GLTR_1 <- Arima(Entrenamiento_GLTR, order = c(3, 1, 4))
Modelo_GLTR_2 <- Arima(Entrenamiento_GLTR, order = c(5, 1, 5))
Modelo_GLTR_3 <- Arima(Entrenamiento_GLTR, order = c(6, 1, 6))
Modelo_GLTR_4 <- Arima(Entrenamiento_GLTR, order = c(7, 1, 6))

# Visualización de los modelos
Modelo_GLTR_auto
Modelo_GLTR_1
Modelo_GLTR_2
Modelo_GLTR_3  # Instabilidad numérica (NaNs)-
Modelo_GLTR_4  # sobre-parametrizado

# VALIDACIÓN DE RESIDUOS

checkresiduals(Modelo_GLTR_auto) # p-value = 0.5984 > 0.05: NO se rechaza H0 (residuos son ruido blanco)

# PRONÓSTICO

h_value <- 5  # 5 períodos adelante

pronostico_GLTR <- Modelo_GLTR_auto %>% 
  forecast(h = h_value, level = 0.95)

pronostico_GLTR %>% 
  autoplot(include = 100)

pronostico_GLTR

# TABLA COMPARATIVA - VALORES REALES VS PRONÓSTICOS

tabla_comparativa_GLTR <- data.frame(
  Fecha = as.Date(index(Prueba_GLTR[1:5])),
  Pronostico = as.numeric(pronostico_GLTR$mean[1:5]),
  Real = as.numeric(Prueba_GLTR[1:5])
)

# Cálculo de errores
tabla_comparativa_GLTR$Error <- tabla_comparativa_GLTR$Real - tabla_comparativa_GLTR$Pronostico
tabla_comparativa_GLTR$ErrorAbsoluto <- abs(tabla_comparativa_GLTR$Error)
tabla_comparativa_GLTR$ErrorPorcentual <- (tabla_comparativa_GLTR$ErrorAbsoluto / tabla_comparativa_GLTR$Real) * 100

print(tabla_comparativa_GLTR)

# MÉTRICAS DE DESEMPEÑO

MAE <- mean(tabla_comparativa_GLTR$ErrorAbsoluto)
RMSE <- sqrt(mean(tabla_comparativa_GLTR$Error^2))
MAPE <- mean(tabla_comparativa_GLTR$ErrorPorcentual)

MAE
RMSE
MAPE

