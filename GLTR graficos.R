#Librerías

library(quantmod)
library(tseries)
library(fpp2)
library(gridExtra)
library(tidyverse)
library(scales)
library(kableExtra)
library(ggplot2)
library(forecast)
library(viridis)
library(patchwork)
library(grid)

# PALETA DE COLORES PARA GLTR

gltr_pal <- list(
  primary = "#2C3E50",
  secondary = "#E74C3C",
  tertiary = "#3498DB",
  positive = "#27AE60",
  negative = "#E74C3C",
  text_dark = "#2C3E50",
  text_gray = "#7F8C8D",
  grid = "#BDC3C7"
)

# DATOS MODELO

AccionesEX <- getSymbols("GLTR", src = "yahoo", auto.assign = FALSE, 
                         from = "2022-10-31")
GLTR <- AccionesEX$GLTR.Close

Entrenamiento_GLTR <- window(GLTR, start = "2022-10-31", end = "2025-10-31")
Prueba_GLTR <- window(GLTR, start = "2025-11-03")


# Serie dividida en ventana de entrenamiento y prueba

df_train_GLTR <- data.frame(
  Fecha = index(Entrenamiento_GLTR),
  Precio = as.numeric(Entrenamiento_GLTR),
  Conjunto = "Entrenamiento"
)

df_test_GLTR <- data.frame(
  Fecha = index(Prueba_GLTR),
  Precio = as.numeric(Prueba_GLTR),
  Conjunto = "Prueba"
)

df_completo_GLTR <- bind_rows(df_train_GLTR, df_test_GLTR)

fecha_corte_GLTR <- as.Date("2025-11-01")

ggplot(df_completo_GLTR, aes(x = Fecha, y = Precio)) +
  geom_ribbon(data = df_train_GLTR, 
              aes(ymin = min(df_completo_GLTR$Precio) * 0.95, ymax = Precio),
              fill = gltr_pal$primary, alpha = 0.08) +
  geom_ribbon(data = df_test_GLTR, 
              aes(ymin = min(df_completo_GLTR$Precio) * 0.95, ymax = Precio),
              fill = gltr_pal$secondary, alpha = 0.15) +
  geom_line(data = df_train_GLTR, color = gltr_pal$primary, linewidth = 0.9) +
  geom_line(data = df_test_GLTR, color = gltr_pal$secondary, linewidth = 1.1) +
  geom_vline(xintercept = fecha_corte_GLTR, 
             linetype = "dashed", color = gltr_pal$negative, linewidth = 0.8) +
  annotate("text", x = fecha_corte_GLTR, y = max(df_completo_GLTR$Precio) * 1.02,
           label = "Corte: 01-Nov-2025", hjust = -0.05, vjust = 0,
           color = gltr_pal$negative, fontface = "bold", size = 3.5) +
  annotate("label", 
           x = as.Date("2024-01-01"), 
           y = max(df_completo_GLTR$Precio) * 0.85,
           label = paste0("ENTRENAMIENTO\n", nrow(df_train_GLTR), " observaciones"),
           fill = gltr_pal$primary, color = "white", 
           fontface = "bold", size = 3.5, label.padding = unit(0.5, "lines")) +
  annotate("label", 
           x = max(df_test_GLTR$Fecha) - 1,
           y = min(df_completo_GLTR$Precio) * 1.15,
           label = paste0("PRUEBA\n", nrow(df_test_GLTR), " obs."),
           fill = gltr_pal$secondary, color = "white", 
           fontface = "bold", size = 3.2, label.padding = unit(0.4, "lines")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     expand = expansion(mult = c(0.05, 0.08))) +
  labs(
    title = "Partición de Datos: Entrenamiento vs Prueba",
    subtitle = "GLTR | Serie de precios de cierre diarios",
    x = NULL,
    y = "Precio de Cierre (USD)",
    caption = paste0("Fuente: Yahoo Finance | Período: ", 
                     min(df_completo_GLTR$Fecha), " a ", max(df_completo_GLTR$Fecha))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 16, color = gltr_pal$text_dark,
                              margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = gltr_pal$secondary,
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = gltr_pal$text_gray,
                                margin = margin(t = 15), hjust = 0),
    axis.title.y = element_text(face = "bold", size = 10, color = gltr_pal$text_gray),
    axis.text = element_text(size = 9, color = gltr_pal$text_gray),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = gltr_pal$grid, linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 25, 15, 15)
  )

# ESTACIONARIEDAD

## Serie (Original) Gráfico ACF 

acf_data_GLTR <- acf(Entrenamiento_GLTR, lag.max = 30, plot = FALSE)

df_acf_GLTR <- data.frame(
  Lag = acf_data_GLTR$lag[-1], 
  ACF = acf_data_GLTR$acf[-1]
)

n_GLTR <- length(Entrenamiento_GLTR)
limite_sup_GLTR <- qnorm(0.975) / sqrt(n_GLTR)
limite_inf_GLTR <- -limite_sup_GLTR
ggplot(df_acf_GLTR, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), 
               color = gltr_pal$primary, linewidth = 0.8) +
  geom_point(color = gltr_pal$primary, size = 2) +
  geom_hline(yintercept = limite_sup_GLTR, linetype = "dashed", 
             color = gltr_pal$secondary, linewidth = 0.7) +
  geom_hline(yintercept = limite_inf_GLTR, linetype = "dashed", 
             color = gltr_pal$secondary, linewidth = 0.7) +
  geom_hline(yintercept = 0, color = gltr_pal$text_gray, linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = limite_inf_GLTR, ymax = limite_sup_GLTR,
           fill = gltr_pal$secondary, alpha = 0.1) +
  annotate("label", x = 20, y = 0.5,
           label = "Serie NO estacionaria",
           fill = gltr_pal$negative, color = "white",
           fontface = "bold", size = 3.5, label.padding = unit(0.5, "lines")) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(-0.1, 1.05), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Función de Autocorrelación (ACF) - Serie en Niveles",
    subtitle = "GLTR: Precio de cierre | Datos de entrenamiento",
    x = "Rezago (Lag)",
    y = "Autocorrelación",
    caption = "Bandas rojas: Límites de significancia al 95%"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 14, color = gltr_pal$text_dark),
    plot.subtitle = element_text(size = 10, color = gltr_pal$secondary),
    plot.caption = element_text(size = 9, color = gltr_pal$text_gray, hjust = 0),
    axis.title = element_text(face = "bold", size = 10, color = gltr_pal$text_gray),
    axis.text = element_text(size = 9, color = gltr_pal$text_gray),
    panel.grid.major = element_line(color = gltr_pal$grid, linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

# Tabla: Resultados Test ADF
adf_resultado_GLTR <- adf.test(Entrenamiento_GLTR)

tabla_adf_GLTR <- data.frame(
  Métrica = c("Estadístico Dickey-Fuller", 
              "Orden de Rezagos (Lag)", 
              "P-valor",
              "Nivel de Significancia (α)",
              "Hipótesis Nula (H₀)",
              "Decisión"),
  
  Valor = c(round(adf_resultado_GLTR$statistic, 4),
            adf_resultado_GLTR$parameter,
            round(adf_resultado_GLTR$p.value, 4),
            "0.05",
            "Serie tiene raíz unitaria",
            ifelse(adf_resultado_GLTR$p.value > 0.05, 
                   "No rechazar H₀", "Rechazar H₀")),
  
  Interpretación = c("Valor del estadístico de prueba",
                     "Rezagos incluidos en el test",
                     "Probabilidad bajo H₀",
                     "Umbral de decisión",
                     "La serie NO es estacionaria",
                     ifelse(adf_resultado_GLTR$p.value > 0.05,
                            "Serie NO estacionaria",
                            "Serie estacionaria"))
)


kable(tabla_adf_GLTR, 
      caption = "Prueba de Dickey-Fuller Aumentada (ADF) - Serie en Niveles GLTR",
      align = c("l", "c", "l")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(3, bold = TRUE, color = "#e17055") %>% 
  row_spec(6, bold = TRUE, background = "#fef3f2")


# DIFERENCIANDO (d=1)

GLTR_diff <- diff(Entrenamiento_GLTR) %>% na.omit()
GLTR_diff_df <- data.frame(
  Fecha = as.Date(time(GLTR_diff)),
  Cambio = as.numeric(GLTR_diff)
)
GLTR_diff_df$ID <- seq.int(nrow(GLTR_diff_df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

GLTR_diff_df <- GLTR_diff_df %>% accumulate_by(~ID)

fig_diff_animated <- plot_ly(
  data = GLTR_diff_df,
  x = ~Fecha,  
  y = ~Cambio,
  frame = ~frame,
  type = 'scatter',
  mode = 'lines',
  fill = 'tozeroy',
  fillcolor = 'rgba(0, 100, 200, 0.3)',
  line = list(color = 'rgb(0, 0, 0)', width = 1.5),
  text = ~paste(
    "Fecha: ", format(Fecha, "%d/%m/%Y"),
    "<br>Cambio: $", round(Cambio, 4)
  ),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "<b>Serie diferenciada GLTR</b>",
      font = list(size = 16, family = "Arial"),
      x = 0.5,
      xanchor = 'center'
    ),
    xaxis = list(
      title = "Index",
      range = c(min(GLTR_diff_df$Fecha), max(GLTR_diff_df$Fecha)),
      showgrid = TRUE,
      gridcolor = 'rgba(200, 200, 200, 0.3)',
      zeroline = FALSE
    ),
    yaxis = list(
      title = "GLTR Close",
      showgrid = TRUE,
      gridcolor = 'rgba(200, 200, 200, 0.3)',
      zeroline = TRUE,
      zerolinecolor = 'rgba(0, 0, 0, 0.3)',
      zerolinewidth = 1
    ),
    plot_bgcolor = '#f0f0f0',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  ) %>%
  animation_opts(
    frame = 50,
    transition = 0,
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Fecha: ",
      font = list(color = "black")
    )
  ) %>%
  animation_button(
    x = 1, xanchor = "right", 
    y = 0, yanchor = "bottom"
  )

fig_diff_animated

# Gráfico ACF de serie diferenciada


acf_diff_data_GLTR <- acf(GLTR_diff, lag.max = 30, plot = FALSE)

df_acf_diff_GLTR <- data.frame(
  Lag = acf_diff_data_GLTR$lag[-1], 
  ACF = acf_diff_data_GLTR$acf[-1]
)

n_diff_GLTR <- length(GLTR_diff)
limite_sup_diff_GLTR <- qnorm(0.975) / sqrt(n_diff_GLTR)
limite_inf_diff_GLTR <- -limite_sup_diff_GLTR

ggplot(df_acf_diff_GLTR, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), 
               color = gltr_pal$tertiary, linewidth = 0.8) +
  geom_point(color = gltr_pal$tertiary, size = 2) +
  geom_hline(yintercept = limite_sup_diff_GLTR, linetype = "dashed", 
             color = gltr_pal$positive, linewidth = 0.7) +
  geom_hline(yintercept = limite_inf_diff_GLTR, linetype = "dashed", 
             color = gltr_pal$positive, linewidth = 0.7) +
  geom_hline(yintercept = 0, color = gltr_pal$text_gray, linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = limite_inf_diff_GLTR, ymax = limite_sup_diff_GLTR,
           fill = gltr_pal$positive, alpha = 0.1) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(-0.3, 0.4), breaks = seq(-0.3, 0.4, 0.1)) +
  labs(
    title = "Función de Autocorrelación (ACF) - Serie Diferenciada",
    subtitle = "GLTR: Primera diferencia | Datos de entrenamiento",
    x = "Rezago (Lag)",
    y = "Autocorrelación",
    caption = "Bandas verdes: Límites de significancia al 95%"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 14, color = gltr_pal$text_dark),
    plot.subtitle = element_text(size = 10, color = gltr_pal$tertiary),
    plot.caption = element_text(size = 9, color = gltr_pal$text_gray, hjust = 0),
    axis.title = element_text(face = "bold", size = 10, color = gltr_pal$text_gray),
    axis.text = element_text(size = 9, color = gltr_pal$text_gray),
    panel.grid.major = element_line(color = gltr_pal$grid, linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

# Tabla: Resultados Test ADF (serie diferenciada)

adf_diff_resultado_GLTR <- adf.test(GLTR_diff)

tabla_adf_diff_GLTR <- data.frame(
  Métrica = c("Estadístico Dickey-Fuller", 
              "Orden de Rezagos (Lag)", 
              "P-valor",
              "Nivel de Significancia (α)",
              "Hipótesis Nula (H₀)",
              "Decisión"),
  Valor = c(round(adf_diff_resultado_GLTR$statistic, 4),
            adf_diff_resultado_GLTR$parameter,
            round(adf_diff_resultado_GLTR$p.value, 4),
            "0.05",
            "Serie tiene raíz unitaria",
            ifelse(adf_diff_resultado_GLTR$p.value > 0.05, 
                   "No rechazar H₀", "Rechazar H₀")),
  Interpretación = c("Valor del estadístico de prueba",
                     "Rezagos incluidos en el test",
                     "Probabilidad bajo H₀",
                     "Umbral de decisión",
                     "La serie NO es estacionaria",
                     ifelse(adf_diff_resultado_GLTR$p.value > 0.05,
                            "Serie NO estacionaria",
                            "Serie estacionaria"))
)
kable(tabla_adf_diff_GLTR, 
      caption = "Prueba de Dickey-Fuller Aumentada (ADF) - Serie Diferenciada GLTR",
      align = c("l", "c", "l")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(3, bold = TRUE, color = "#27AE60") %>% 
  row_spec(6, bold = TRUE, background = "#d5f4e6")

# IDENTIFICACIÓN DEL MODELO


crear_grafico_acf_pacf <- function(serie, titulo_serie = "GLTR", lag_max = NULL) {
  
  acf_data <- acf(serie, plot = FALSE, lag.max = lag_max)
  pacf_data <- pacf(serie, plot = FALSE, lag.max = lag_max)
  
  acf_df <- data.frame(
    lag = as.numeric(acf_data$lag)[-1], 
    acf = as.numeric(acf_data$acf)[-1]
  )
  
  pacf_df <- data.frame(
    lag = as.numeric(pacf_data$lag), 
    pacf = as.numeric(pacf_data$acf)
  )
  
  conf_level <- qnorm((1 + 0.95)/2)/sqrt(acf_data$n.used)
  
  y_max <- max(c(abs(acf_df$acf), abs(pacf_df$pacf), conf_level)) * 1.2
  
  # GRÁFICO ACF
  p_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", 
             width = 0.7,
             fill = ifelse(acf_df$acf > 0, "#2196F3", "#FF5252"),
             color = NA,
             alpha = 0.8) +
    
    geom_hline(yintercept = conf_level, 
               color = "#757575", 
               linetype = "dashed", 
               linewidth = 0.5,
               alpha = 0.7) +
    geom_hline(yintercept = -conf_level, 
               color = "#757575", 
               linetype = "dashed", 
               linewidth = 0.5,
               alpha = 0.7) +
    geom_hline(yintercept = 0, 
               color = "#212121", 
               linewidth = 0.3) +
    
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = -conf_level, ymax = conf_level,
             fill = "#E8F5E9",
             alpha = 0.2) +
    
    geom_text(data = subset(acf_df, abs(acf) > conf_level),
              aes(label = round(acf, 2)),
              vjust = ifelse(subset(acf_df, abs(acf) > conf_level)$acf > 0, -0.8, 1.2),
              size = 3.2,
              fontface = "bold",
              color = "#212121") +
    
    labs(
      title = "AUTOCORRELACIÓN (ACF)",
      subtitle = paste("Serie diferenciada:", titulo_serie),
      x = "Desfase (Lags)",
      y = "Autocorrelación",
      caption = paste(" ")
    ) +
    
    scale_x_continuous(breaks = scales::pretty_breaks(n = min(20, max(acf_df$lag)))) +
    scale_y_continuous(limits = c(-y_max, y_max),
                       breaks = scales::pretty_breaks(n = 8)) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "#E0E0E0", fill = NA, linewidth = 0.5),
      
      plot.title = element_text(
        face = "bold",
        size = 14,
        color = "#0D47A1",
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "#546E7A",
        hjust = 0,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = 9,
        color = "#78909C",
        hjust = 0,
        margin = margin(t = 10)
      ),
      
      axis.title = element_text(
        size = 11,
        color = "#37474F",
        face = "bold"
      ),
      axis.text = element_text(
        size = 10,
        color = "#546E7A"
      ),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "#F5F5F5", linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      
      plot.margin = margin(15, 20, 15, 15)
    )
  
  # GRÁFICO PACF
  p_pacf <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
    geom_bar(stat = "identity", 
             width = 0.7,
             fill = ifelse(pacf_df$pacf > 0, "#4CAF50", "#FF9800"),
             color = NA,
             alpha = 0.8) +
    
    geom_hline(yintercept = conf_level, 
               color = "#757575", 
               linetype = "dashed", 
               linewidth = 0.5,
               alpha = 0.7) +
    geom_hline(yintercept = -conf_level, 
               color = "#757575", 
               linetype = "dashed", 
               linewidth = 0.5,
               alpha = 0.7) +
    geom_hline(yintercept = 0, 
               color = "#212121", 
               linewidth = 0.3) +
    
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = -conf_level, ymax = conf_level,
             fill = "#F3E5F5",
             alpha = 0.2) +
    
    geom_text(data = subset(pacf_df, abs(pacf) > conf_level),
              aes(label = round(pacf, 2)),
              vjust = ifelse(subset(pacf_df, abs(pacf) > conf_level)$pacf > 0, -0.8, 1.2),
              size = 3.2,
              fontface = "bold",
              color = "#212121") +
    
    labs(
      title = "AUTOCORRELACIÓN PARCIAL (PACF)",
      subtitle = paste("Serie diferenciada:", titulo_serie),
      x = "Desfase (Lags)",
      y = "Autocorrelación Parcial",
      caption = paste(" ", 
                      format(Sys.Date(), "%d/%m/%Y"))
    ) +
    
    scale_x_continuous(breaks = scales::pretty_breaks(n = min(20, max(pacf_df$lag)))) +
    scale_y_continuous(limits = c(-y_max, y_max),
                       breaks = scales::pretty_breaks(n = 8)) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "#E0E0E0", fill = NA, linewidth = 0.5),
      
      plot.title = element_text(
        face = "bold",
        size = 14,
        color = "#7B1FA2",
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "#546E7A",
        hjust = 0,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = 9,
        color = "#78909C",
        hjust = 1,
        margin = margin(t = 10)
      ),
      
      axis.title = element_text(
        size = 11,
        color = "#37474F",
        face = "bold"
      ),
      axis.text = element_text(
        size = 10,
        color = "#546E7A"
      ),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "#F5F5F5", linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      
      plot.margin = margin(15, 20, 15, 15)
    )
  
  return(list(ACF = p_acf, PACF = p_pacf))
}

graficos <- crear_grafico_acf_pacf(
  serie = GLTR_diff,
  titulo_serie = "GLTR (Diferenciada d=1)",
  lag_max = 36
)


panel_completo <- grid.arrange(
  graficos$ACF,
  graficos$PACF,
  nrow = 1,
  top = textGrob(
    "",
    gp = gpar(fontsize = 18, fontface = "bold", col = "#0D47A1"),
    vjust = 1.5
  ),
  bottom = textGrob(
    paste("Análisis generado:", Sys.Date(), "| Método: Diferenciación (d=1)"),
    gp = gpar(fontsize = 10, col = "#78909C"),
    vjust = -0.5
  ),
  padding = unit(2, "cm")
)

# ESTIMACIÓN Y COMPARACIÓN DE MODELOS
# criterios AIC, BIC, AICc
Modelo_GLTR_auto <- auto.arima(Entrenamiento_GLTR)
Modelo_GLTR_1 <- Arima(Entrenamiento_GLTR, order = c(3, 1, 4))
Modelo_GLTR_2 <- Arima(Entrenamiento_GLTR, order = c(1, 1, 1))
Modelo_GLTR_3 <- Arima(Entrenamiento_GLTR, order = c(2, 1, 4))
Modelo_GLTR_4 <- Arima(Entrenamiento_GLTR, order = c(4, 1, 4))

tabla_modelos_GLTR <- data.frame(
  Modelo = c('Auto-ARIMA', 'ARIMA(3,1,4)', 'ARIMA(1,1,1)', 'ARIMA(2,1,4)', 'ARIMA(4,1,4)'),
  AIC = round(c(Modelo_GLTR_auto$aic, Modelo_GLTR_1$aic, Modelo_GLTR_2$aic, 
                Modelo_GLTR_3$aic, Modelo_GLTR_4$aic), 2),
  AICc = round(c(Modelo_GLTR_auto$aicc, Modelo_GLTR_1$aicc, Modelo_GLTR_2$aicc, 
                 Modelo_GLTR_3$aicc, Modelo_GLTR_4$aicc), 2),
  BIC = round(c(Modelo_GLTR_auto$bic, Modelo_GLTR_1$bic, Modelo_GLTR_2$bic, 
                Modelo_GLTR_3$bic, Modelo_GLTR_4$bic), 2)
) %>% arrange(AICc)

kable(tabla_modelos_GLTR, 
      caption = "Comparación de Modelos ARIMA - Criterios de Información",
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(1, bold = TRUE, background = "#d5f4e6")

# Métricas de Precisión
accuracy_auto <- accuracy(Modelo_GLTR_auto)
accuracy_1 <- accuracy(Modelo_GLTR_1)
accuracy_2 <- accuracy(Modelo_GLTR_2)
accuracy_3 <- accuracy(Modelo_GLTR_3)
accuracy_4 <- accuracy(Modelo_GLTR_4)

tabla_accuracy_GLTR <- data.frame(
  Modelo = c('Auto-ARIMA', 'ARIMA(3,1,4)', 'ARIMA(1,1,1)', 'ARIMA(2,1,4)', 'ARIMA(4,1,4)'),
  ME = round(c(accuracy_auto[1], accuracy_1[1], accuracy_2[1], accuracy_3[1], accuracy_4[1]), 4),
  RMSE = round(c(accuracy_auto[2], accuracy_1[2], accuracy_2[2], accuracy_3[2], accuracy_4[2]), 4),
  MAE = round(c(accuracy_auto[3], accuracy_1[3], accuracy_2[3], accuracy_3[3], accuracy_4[3]), 4),
  MAPE = round(c(accuracy_auto[5], accuracy_1[5], accuracy_2[5], accuracy_3[5], accuracy_4[5]), 2)
)

kable(tabla_accuracy_GLTR, 
      caption = "Métricas de Precisión - Modelos ARIMA GLTR",
      align = c("l", "c", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center")

#Se elige autoarima y revisamos sus residuales

residuos <- residuals(Modelo_GLTR_auto)
fitted_vals <- fitted(Modelo_GLTR_auto)
n <- length(residuos)
media_resid <- mean(residuos, na.rm = TRUE)
sd_resid <- sd(residuos, na.rm = TRUE)

residuos_auto <- residuals(Modelo_GLTR_auto)

residuos_df <- data.frame(
  tiempo = time(residuos_auto),
  residuales = as.numeric(residuos_auto)
)

sd_resid <- sd(residuos_df$residuales, na.rm = TRUE)
residuos_df$extremo <- abs(residuos_df$residuales) > 2 * sd_resid
residuos_df$muy_extremo <- abs(residuos_df$residuales) > 3 * sd_resid

# Gráfico 1: Residuales vs Tiempo - Versión Mejorada

ggplot(residuos_df, aes(x = tiempo, y = residuales)) +
 
  geom_ribbon(
    aes(ymin = -1.96 * sd_resid, 
        ymax = 1.96 * sd_resid),
    fill = "#E8F4F8",
    alpha = 0.3
  ) +

  geom_line(color = "#1E88E5", linewidth = 0.8) +
  
  geom_hline(yintercept = 0, color = "#2E5A87", linetype = "solid", 
             linewidth = 1.2, alpha = 0.8) +
  
  geom_smooth(method = "loess", se = TRUE, color = "#D55E00", 
              fill = "#F0E442", alpha = 0.3, linewidth = 0.8) +
  
  geom_point(
    data = residuos_df[residuos_df$extremo, ],
    aes(x = tiempo, y = residuales),
    color = "red",
    size = 2,
    alpha = 0.7
  ) +
  
  
  geom_text(
    data = residuos_df[residuos_df$muy_extremo, ],
    aes(x = tiempo, y = residuales, label = round(residuales, 2)),
    vjust = -1,
    size = 3,
    color = "#D32F2F"
  ) +
  

  labs(
    title = "Análisis de Residuales - Modelo Auto ARIMA",
    subtitle = paste("Especificación del modelo: ARIMA(", 
                     paste(arimaorder(Modelo_GLTR_auto), collapse = ","), ")"),
    x = "Período Temporal",
    y = "Valor de los Residuales",
    caption = "Fuente: Análisis propio | Línea naranja: tendencia LOESS"
  ) +
  

  theme_minimal(base_size = 11) +
  theme(

    plot.title = element_text(
      face = "bold", 
      size = 16,
      color = "#1E3A5F",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "#4A4A4A",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#7F7F7F",
      hjust = 1,
      margin = margin(t = 10)
    ),
  
    axis.title = element_text(
      face = "bold",
      size = 11,
      color = "#333333"
    ),
    axis.text = element_text(
      size = 10,
      color = "#555555"
    ),
    axis.line = element_line(color = "#CCCCCC"),
    axis.ticks = element_line(color = "#CCCCCC"),
    
    panel.grid.major = element_line(
      color = "#F0F0F0",
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    plot.margin = margin(20, 30, 20, 30),
   
    legend.position = "none"
  )

# Gráfico 2: Histograma y densidad 


residuales_valores <- as.numeric(residuos_auto)

residuales_df <- data.frame(Residuales = residuales_valores)

media_res <- mean(residuales_valores)
sd_res <- sd(residuales_valores)
n_res <- length(residuales_valores)

rango_residuales <- range(residuales_valores)
x_lim_inf <- rango_residuales[1] - 0.1 * diff(rango_residuales)
x_lim_sup <- rango_residuales[2] + 0.1 * diff(rango_residuales)

dens <- density(residuales_valores)
max_densidad <- max(dens$y)
y_lim_sup <- max_densidad * 1.15

ggplot(residuales_df, aes(x = Residuales)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "#2E5A87",
    alpha = 0.7,
    color = "white",
    size = 0.3
  ) +
  
  geom_density(
    color = "#D55E00",
    linewidth = 1.2,
    adjust = 1.2
  ) +
  
  stat_function(
    fun = dnorm,
    args = list(mean = media_res, sd = sd_res),
    color = "#009E73",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_vline(
    xintercept = media_res,
    color = "#333333",
    linetype = "solid",
    linewidth = 1,
    alpha = 0.8
  ) +
  
  geom_vline(
    xintercept = c(media_res - sd_res, media_res + sd_res),
    color = "#666666",
    linetype = "dashed",
    linewidth = 0.6,
    alpha = 0.6
  ) +
  
  geom_vline(
    xintercept = c(media_res - 2*sd_res, media_res + 2*sd_res),
    color = "#999999",
    linetype = "dotted",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  
  annotate(
    "text",
    x = media_res,
    y = y_lim_sup * 0.95,
    label = paste("Media =", round(media_res, 3)),
    hjust = ifelse(media_res > mean(c(x_lim_inf, x_lim_sup)), 1.1, -0.1),
    size = 3.5,
    color = "#333333",
    fontface = "bold"
  ) +
  
  stat_function(
    fun = dnorm,
    args = list(mean = media_res, sd = sd_res),
    geom = "area",
    fill = "#009E73",
    alpha = 0.1,
    xlim = c(media_res - 2*sd_res, media_res + 2*sd_res)
  ) +
  
  coord_cartesian(
    xlim = c(x_lim_inf, x_lim_sup),
    ylim = c(0, y_lim_sup)
  ) +
  
  scale_x_continuous(
    breaks = seq(
      floor(x_lim_inf/5)*5, 
      ceiling(x_lim_sup/5)*5, 
      by = ifelse(diff(rango_residuales) > 10, 5, 2)
    )
  ) +
  
  labs(
    title = "Distribución de Residuales - Modelo Auto ARIMA",
    subtitle = paste(
      "Media =", round(media_res, 4), 
      "| SD =", round(sd_res, 4),
      "| n =", format(n_res, big.mark = ",")
    ),
    x = "Valor de los Residuales",
    y = "Densidad",
    caption = "Líneas: Media (negra sólida), ±1 SD (gris discontinua), ±2 SD (gris punteada)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold", 
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "gray40",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray60",
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Gráfico 3: ACF de residuales

acf_residuos <- acf(residuos_auto, lag.max = 40, plot = FALSE)

df_acf_residuos <- data.frame(
  Lag = acf_residuos$lag[-1], 
  ACF = acf_residuos$acf[-1]
)

n_residuos <- length(residuos_auto)
limite_sup_residuos <- qnorm(0.975) / sqrt(n_residuos)
limite_inf_residuos <- -limite_sup_residuos

ggplot(df_acf_residuos, aes(x = Lag, y = ACF)) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = limite_inf_residuos, ymax = limite_sup_residuos,
           fill = "#E63946", 
           alpha = 0.05) +
  
  geom_segment(aes(xend = Lag, yend = 0), 
               color = "#2E86AB", 
               linewidth = 0.9, 
               lineend = "round") +
  
  geom_point(color = "#2E86AB", 
             fill = "#2E86AB", 
             size = 2.5, 
             shape = 21, 
             stroke = 0.5) +
  
  geom_hline(yintercept = c(limite_sup_residuos, limite_inf_residuos), 
             linetype = "dashed", 
             color = "#E63946", 
             linewidth = 0.6) +
  
  geom_hline(yintercept = 0, 
             color = "#4A4A4A", 
             linewidth = 0.4) +
  
  scale_x_continuous(breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  scale_y_continuous(limits = c(-0.35, 0.35),
                     breaks = seq(-0.3, 0.3, by = 0.1),
                     expand = expansion(mult = c(0, 0.05))) +
  
  labs(
    title = "FUNCIÓN DE AUTOCORRELACIÓN DE RESIDUALES",
    subtitle = "Modelo Auto ARIMA | Análisis de independencia de residuales",
    x = "Rezago (Lag)",
    y = "Coeficiente de Autocorrelación (ACF)",
    caption = paste0(
      "Intervalo de confianza del 95% (±", 
      round(limite_sup_residuos, 3), 
      ") | n = ", 
      format(n_residuos, big.mark = ",")
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
  
    plot.background = element_rect(fill = "#F8F9FA", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 25, 20, 25),
 
    plot.title = element_text(
      face = "bold", 
      size = 16, 
      color = "#212529",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12, 
      color = "#6C757D",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9, 
      color = "#6C757D",
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    
    axis.title = element_text(
      face = "bold", 
      size = 11, 
      color = "#212529"
    ),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(
      size = 10, 
      color = "#6C757D"
    ),
    
    panel.grid.major = element_line(
      color = "#E9ECEF", 
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      fill = NA, 
      color = "#E9ECEF", 
      linewidth = 0.5
    )
  )

## 4.5.2 Prueba de Ljung-Box
ljung_box_result <- Box.test(residuals(Modelo_GLTR_auto), lag = 10, type = "Ljung-Box")

tabla_ljung_box <- data.frame(
  Estadístico = c("X-squared", "Grados de libertad", "P-valor", "Decisión"),
  Valor = c(round(ljung_box_result$statistic, 4),
            ljung_box_result$parameter,
            round(ljung_box_result$p.value, 4),
            ifelse(ljung_box_result$p.value > 0.05, 
                   "No rechazar H₀: Residuos = Ruido Blanco ✓", 
                   "Rechazar H₀: Residuos ≠ Ruido Blanco ✗"))
)

kable(tabla_ljung_box, 
      caption = "Prueba de Ljung-Box - Validación de Residuos",
      align = c("l", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(4, bold = TRUE, background = "#d5f4e6")


#  PRONÓSTICO Y EVALUACIÓN

# Mejora del gráfico de pronóstico
library(ggplot2)
library(fable)
library(scales)

# Crear gráfico mejorado
grafico_mejorado <- function(pronostico, modelo, include_n = 100) {
  
  # Extraer información del modelo para el subtítulo
  modelo_texto <- paste0("Modelo: ", deparse(modelo))
  
  # Determinar color principal basado en la tendencia del pronóstico
  ultimo_valor <- tail(pronostico$.mean, 1)
  primer_valor_pronostico <- head(pronostico$.mean, 1)
  
  # Color basado en dirección del pronóstico
  color_principal <- if(ultimo_valor >= primer_valor_pronostico) {
    "#2E8B57"  # Verde para tendencia alcista
  } else {
    "#DC143C"  # Rojo para tendencia bajista
  }
  
  # Color para datos históricos
  color_historico <- "#4A4A4A"
  
  # Crear el gráfico
  p <- pronostico %>%
    autoplot(include = include_n, level = 0.95) +
    
    # Personalizar estética
    geom_line(aes(y = .mean), color = color_principal, size = 1.2, 
              data = as_tibble(pronostico) %>% filter(!is.na(.mean))) +
    
    # Títulos y etiquetas
    labs(
      title = "PRONÓSTICO DE SERIE TEMPORAL - MODELO GLTR",
      subtitle = paste0("Intervalo de confianza del 95% | ", modelo_texto),
      x = "Período Temporal",
      y = "Precio de Cierre (USD)",
      caption = paste0(
        "Pronóstico generado: ", format(Sys.Date(), "%d/%m/%Y"), 
        " | Horizonte: ", nrow(pronostico)-include_n, " períodos futuros"
      )
    ) +
    
    # Tema personalizado
    theme_minimal(base_size = 13) +
    theme(
      # Títulos
      plot.title = element_text(
        face = "bold", 
        size = 18, 
        color = "#2C3E50",
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12, 
        color = "#7F8C8D",
        hjust = 0.5,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = 10, 
        color = "#95A5A6",
        hjust = 1,
        margin = margin(t = 10)
      ),
      
      # Ejes
      axis.title = element_text(
        face = "bold", 
        color = "#2C3E50",
        size = 12
      ),
      axis.text = element_text(color = "#2C3E50"),
      axis.line = element_line(color = "#BDC3C7"),
      
      # Panel y fondo
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Leyenda
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "#2C3E50"),
      legend.box = "horizontal",
      legend.margin = margin(b = 5)
    ) +
    
    # Formato de ejes
    scale_y_continuous(
      labels = dollar_format(prefix = "$", big.mark = ","),
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    
    # Colores personalizados para series
    scale_color_manual(
      values = c(
        "Datos Históricos" = color_historico,
        "Pronóstico" = color_principal,
        "95% CI" = "#D1E7DD"
      ),
      labels = c("Datos Históricos", "Pronóstico", "Intervalo de Confianza 95%")
    ) +
    
    # Anotación para destacar el último valor pronosticado
    annotate(
      "point",
      x = tail(time(pronostico), 1),
      y = ultimo_valor,
      color = color_principal,
      size = 4
    ) +
    
    annotate(
      "text",
      x = tail(time(pronostico), 1),
      y = ultimo_valor,
      label = paste0("Último pronóstico:\n", dollar(ultimo_valor, prefix = "$")),
      color = color_principal,
      size = 3.5,
      fontface = "bold",
      hjust = -0.1,
      vjust = 0.5
    )
  
  # Si hay suficientes datos, agregar línea de tendencia
  if (include_n > 20) {
    p <- p + 
      geom_smooth(
        data = as_tibble(pronostico) %>% 
          filter(!is.na(.mean)) %>% 
          head(include_n),
        aes(x = as.numeric(Index), y = .mean),
        method = "loess",
        se = FALSE,
        color = color_principal,
        alpha = 0.3,
        linetype = "dashed",
        size = 0.7
      )
  }
  
  return(p)
}

# Ejecutar la función con tus datos
grafico_final <- grafico_mejorado(pronostico_GLTR, Modelo_GLTR_auto, include_n = 100)

# Mostrar el gráfico
print(grafico_final)



h_value <- length(Prueba_GLTR)
pronostico_GLTR <- Modelo_GLTR_auto %>% 
  forecast(h = h_value, level = 0.95)

pronostico_GLTR %>% 
  autoplot(include = 100) +
  labs(
    title = "Pronóstico del Precio de Cierre GLTR",
    subtitle = paste0("Modelo: ", Modelo_GLTR_auto, " | Intervalo de Confianza: 95%"),
    x = "Fecha",
    y = "Precio de Cierre (USD)",
    caption = paste0("Horizonte de pronóstico: ", h_value, " observaciones")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Títulos
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 5),
      color = "#2C3E50"
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      margin = margin(b = 15),
      color = "#7F8C8D"
    ),
    plot.caption = element_text(
      size = 9,
      hjust = 1,
      margin = margin(t = 10),
      color = "#95A5A6",
      face = "italic"
    ),
    
    # Ejes
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      margin = margin(t = 10),
      color = "#34495E"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      margin = margin(r = 10),
      color = "#34495E"
    ),
    axis.text = element_text(size = 10, color = "#5D6D7E"),
    axis.line = element_line(color = "#BDC3C7", linewidth = 0.5),
    
    # Grilla
    panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#F8F9F9", linewidth = 0.3),
    
    # Fondo
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "white", color = "#BDC3C7", linewidth = 0.3),
    legend.key = element_rect(fill = "white", color = NA),
    legend.margin = margin(t = 10),
    
    # Margen general
    plot.margin = margin(20, 20, 15, 20)
  ) +
  # Personalizar colores de la serie y pronóstico
  scale_color_manual(
    values = c("Data" = "#3498DB", "Forecast" = "#E74C3C"),
    name = "Serie"
  ) +
  scale_fill_manual(
    values = c("95%" = alpha("#E74C3C", 0.2)),
    name = "Intervalo"
  )

## Evaluación con Datos de Prueba
tabla_comparativa_GLTR <- data.frame(
  Día = 1:length(Prueba_GLTR),
  Fecha = format(index(Prueba_GLTR), "%Y-%m-%d"),
  Real = round(as.numeric(Prueba_GLTR), 2),
  Pronóstico = round(as.numeric(pronostico_GLTR$mean), 2),
  Error_USD = round(as.numeric(Prueba_GLTR) - as.numeric(pronostico_GLTR$mean), 2),
  Error_Pct = round(((as.numeric(Prueba_GLTR) - as.numeric(pronostico_GLTR$mean)) / 
                       as.numeric(Prueba_GLTR)) * 100, 3)
)

kable(tabla_comparativa_GLTR[1:5, ], 
      caption = "Comparación: Valores Reales vs Pronósticos GLTR",
      align = c("c", "c", "c", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center")

# Gráfico: Predicho vs Real
fechas_pronostico_GLTR <- index(Prueba_GLTR)

ggplot() +
  geom_line(data = data.frame(Fecha = index(Entrenamiento_GLTR), 
                              Precio = as.numeric(Entrenamiento_GLTR)),
            aes(x = Fecha, y = Precio), color = 'black', alpha = 0.6) +
  geom_line(data = data.frame(Fecha = index(Prueba_GLTR), 
                              Precio = as.numeric(Prueba_GLTR)),
            aes(x = Fecha, y = Precio), color = gltr_pal$negative, linewidth = 1.2) +
  geom_line(data = data.frame(Fecha = fechas_pronostico_GLTR,
                              Precio = as.numeric(pronostico_GLTR$mean)),
            aes(x = Fecha, y = Precio), color = gltr_pal$tertiary, linewidth = 1) +
  labs(
    title = "GLTR: Pronóstico vs Real",
    subtitle = paste0("Negro=Entrenamiento | Rojo=Real | Azul=Pronóstico ", Modelo_GLTR_auto),
    x = "Fecha", 
    y = "Precio USD"
  ) +
  theme_minimal()

# MÉTRICAS DE DESEMPEÑO FINALES
MAE_GLTR <- mean(abs(tabla_comparativa_GLTR$Error_USD))
RMSE_GLTR <- sqrt(mean(tabla_comparativa_GLTR$Error_USD^2))
MAPE_GLTR <- mean(abs(tabla_comparativa_GLTR$Error_Pct))

tabla_metricas_finales <- data.frame(
  Métrica = c("MAE (Error Absoluto Medio)", "RMSE (Raíz del Error Cuadrático Medio)", "MAPE (Error Porcentual Absoluto Medio)"),
  Valor = c(round(MAE_GLTR, 4), round(RMSE_GLTR, 4), paste0(round(MAPE_GLTR, 2), "%")),
  Interpretación = c("Promedio de errores en USD", "Penaliza errores grandes", "Error promedio en porcentaje")
)

kable(tabla_metricas_finales, 
      caption = "Métricas de Desempeño del Pronóstico - Datos de Prueba",
      align = c("l", "c", "l")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center")
