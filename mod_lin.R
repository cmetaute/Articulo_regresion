# =============================================================================
#  Modelo lineal - Solidos solubles totales (SST) del lulo
#  Molano-Diaz, Reyes-Medina y Alvarez-Herrera (2022). Biotecnologia en el
#  Sector Agropecuario y Agroindustrial, 20(2), 60-75. Cuadro 2 (SST).
#
#  Y  = SST (°Brix)
#  X1 = dias despues de cosecha (ddc)
#  X2 = dosis de 1-MCP (µg/L)
#
#  El articulo no reporta una ecuacion de regresion: usa un ANOVA factorial
#  con prueba de Duncan sobre las mediciones individuales, que no estan
#  publicadas. Con los 15 promedios del Cuadro 2 se ajusta el modelo lineal
#  sst = b0 + b1*dias + b2*dosis + e
# =============================================================================


paquetes  <- c("readxl", "ggplot2")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")

library(readxl)
library(ggplot2)

theme_set(theme_minimal(base_size = 12))


url_datos <- paste0("https://raw.githubusercontent.com/",
                    "cmetaute/Articulo_regresion/main/datos.xlsx")

archivo_temporal <- tempfile(fileext = ".xlsx")
download.file(url_datos, archivo_temporal, mode = "wb", quiet = TRUE)

crudo <- suppressMessages(read_excel(archivo_temporal, sheet = 1, col_names = FALSE))
datos <- as.data.frame(crudo[, 1:3])
names(datos) <- c("dias", "dosis", "sst")
datos[] <- lapply(datos, function(v) suppressWarnings(as.numeric(v)))
datos <- datos[complete.cases(datos), ]
rownames(datos) <- NULL

niveles_dosis <- sort(unique(datos$dosis))
datos$dosis_f <- factor(datos$dosis, levels = niveles_dosis,
                        labels = paste(niveles_dosis, "µg/L"))


# Verificación contra el artículo

# El articulo dice que a los 32 ddc las dosis de 280 y 560 µg/L tuvieron
# 8,4 % y 7,7 % mas SST que el testigo.
sst32 <- datos[datos$dias == 32, ]
dif32 <- 100 * (sst32$sst[sst32$dosis > 0] / sst32$sst[sst32$dosis == 0] - 1)

cat("\n==== VERIFICACIÓN CONTRA EL ARTÍCULO ====\n")
cat("SST a los 32 ddc (0, 280, 560 µg/L):", sst32$sst, "\n")
cat("Aumento frente al testigo (%):", round(dif32, 1),
    "| el artículo reporta 8,4 y 7,7\n")


# Modelo lineal

modelo <- lm(sst ~ dias + dosis, data = datos)

cat("\n==== MODELO LINEAL: sst ~ dias + dosis ====\n")
print(summary(modelo))

cat("\nIntervalos de confianza del 95 % para los coeficientes:\n")
print(round(confint(modelo), 5))

cat("\nTabla ANOVA (suma de cuadrados secuencial):\n")
print(anova(modelo))

b <- coef(modelo)
cat("\nModelo ajustado: SST =", round(b[1], 4), "+", round(b[2], 4), "* dias +",
    format(b[3], digits = 3), "* dosis\n")
cat("R² =", round(summary(modelo)$r.squared, 4),
    "| R² ajustado =", round(summary(modelo)$adj.r.squared, 4), "\n")


# Supuestos

datos$ajustado  <- fitted(modelo)
datos$residuo   <- resid(modelo)
datos$estudent  <- rstudent(modelo)
datos$cook      <- cooks.distance(modelo)
datos$palanca   <- hatvalues(modelo)

cat("\n==== SUPUESTOS DEL MODELO ====\n")
cat("Shapiro-Wilk de los residuos: p-valor =",
    round(shapiro.test(datos$residuo)$p.value, 3), "\n")
cat("Residuos estudentizados con |r| > 2:", sum(abs(datos$estudent) > 2), "\n")
cat("Distancia de Cook máxima:", round(max(datos$cook), 3),
    "(día", datos$dias[which.max(datos$cook)],
    ", dosis", datos$dosis[which.max(datos$cook)], ")\n")


# Modelo con término cuadrático en los días

modelo_cuad <- lm(sst ~ dias + I(dias^2) + dosis, data = datos)

cat("\n==== COMPARACIÓN CON UN TÉRMINO CUADRÁTICO EN LOS DÍAS ====\n")
cat("R² ajustado lineal:    ", round(summary(modelo)$adj.r.squared, 4), "\n")
cat("R² ajustado cuadrático:", round(summary(modelo_cuad)$adj.r.squared, 4), "\n")
print(anova(modelo, modelo_cuad))


# Figuras

# Figura 1. SST observado y recta ajustada por dosis
rejilla <- expand.grid(dias = seq(0, 32, length.out = 100), dosis = niveles_dosis)
rejilla$sst <- predict(modelo, rejilla)
rejilla$dosis_f <- factor(rejilla$dosis, levels = niveles_dosis,
                          labels = paste(niveles_dosis, "µg/L"))

figura1 <- ggplot(datos, aes(x = dias, y = sst, colour = dosis_f, shape = dosis_f)) +
  geom_point(size = 3) +
  geom_line(data = rejilla, linewidth = 0.8) +
  scale_x_continuous(breaks = sort(unique(datos$dias))) +
  labs(title = "SST observado y modelo lineal ajustado",
       subtitle = "Las rectas casi se superponen: la dosis no aporta al modelo",
       x = "Días después de cosecha", y = "SST (°Brix)",
       colour = "Dosis", shape = "Dosis")
print(figura1)

# Figura 2. Valores observados frente a ajustados
figura2 <- ggplot(datos, aes(x = ajustado, y = sst)) +
  geom_abline(intercept = 0, slope = 1, colour = "#C0392B") +
  geom_point(colour = "#1F4E79", size = 2.5) +
  labs(title = "Valores observados frente a ajustados",
       x = "SST ajustado (°Brix)", y = "SST observado (°Brix)")
print(figura2)

# Figura 3. Residuos frente a valores ajustados
figura3 <- ggplot(datos, aes(x = ajustado, y = residuo)) +
  geom_hline(yintercept = 0, colour = "#C0392B") +
  geom_point(colour = "#1F4E79", size = 2.5) +
  labs(title = "Residuos frente a valores ajustados",
       x = "SST ajustado (°Brix)", y = "Residuo")
print(figura3)

# Figura 4. Residuos frente a los días
figura4 <- ggplot(datos, aes(x = dias, y = residuo)) +
  geom_hline(yintercept = 0, colour = "#C0392B") +
  geom_point(colour = "#1F4E79", size = 2.5) +
  scale_x_continuous(breaks = sort(unique(datos$dias))) +
  labs(title = "Residuos frente a los días de almacenamiento",
       subtitle = "El patrón en forma de arco indica curvatura que la recta no captura",
       x = "Días después de cosecha", y = "Residuo")
print(figura4)

# Figura 5. Gráfico Q-Q de los residuos estudentizados
figura5 <- ggplot(datos, aes(sample = estudent)) +
  stat_qq(colour = "#1F4E79", size = 2.5) +
  stat_qq_line(colour = "#C0392B") +
  labs(title = "Gráfico Q-Q de los residuos estudentizados",
       x = "Cuantiles teóricos", y = "Cuantiles observados")
print(figura5)

# Figura 6. Distancia de Cook
datos$observacion <- seq_len(nrow(datos))
figura6 <- ggplot(datos, aes(x = observacion, y = cook)) +
  geom_col(fill = "#7FA7C9") +
  scale_x_continuous(breaks = datos$observacion) +
  labs(title = "Distancia de Cook",
       x = "Observación", y = "Distancia de Cook")
print(figura6)

# Figura 7. Modelo con término cuadrático
rejilla$sst_cuad <- predict(modelo_cuad, rejilla)

figura7 <- ggplot(datos, aes(x = dias, y = sst)) +
  geom_point(aes(colour = dosis_f, shape = dosis_f), size = 3) +
  geom_line(data = rejilla[rejilla$dosis == 0, ], aes(y = sst), linetype = "dashed",
            colour = "#C0392B") +
  geom_line(data = rejilla[rejilla$dosis == 0, ], aes(y = sst_cuad),
            colour = "#1F4E79") +
  scale_x_continuous(breaks = sort(unique(datos$dias))) +
  labs(title = "Modelo lineal y modelo con término cuadrático (testigo, 0 µg/L)",
       subtitle = "Lineal (rojo, discontinuo) y cuadrático (azul)",
       x = "Días después de cosecha", y = "SST (°Brix)",
       colour = "Dosis", shape = "Dosis")
print(figura7)
