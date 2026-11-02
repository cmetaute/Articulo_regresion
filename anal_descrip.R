# =============================================================================
#  Análisis descriptivo - Solidos solubles totales (SST) del lulo
#  Efecto de los días de almacenamiento y de la dosis de 1-MCP
#  (15 registros: promedios publicados en la Tabla 1 del artículo)
# =============================================================================


paquetes  <- c("readxl", "ggplot2", "tidyr", "GGally")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")

library(readxl)
library(ggplot2)
library(tidyr)
library(GGally)

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


# Preparación

etiquetas <- c(dias  = "Días de almacenamiento",
               dosis = "Dosis de 1-MCP (µg/L)",
               sst   = "SST (°Brix)")
variables <- names(etiquetas)

niveles_dosis <- sort(unique(datos$dosis))
datos$dosis_f <- factor(datos$dosis, levels = niveles_dosis,
                        labels = paste(niveles_dosis, "µg/L"))


# Calidad de los datos

cat("\n==== 3. CALIDAD DE LOS DATOS ====\n")
cat("Registros (filas):", nrow(datos), "| Variables (columnas):", length(variables), "\n")
cat("Datos faltantes por variable:\n")
print(colSums(is.na(datos[variables])))
cat("Filas repetidas:", sum(duplicated(datos[variables])), "\n")
cat("Combinaciones día x dosis repetidas:",
    sum(duplicated(datos[c("dias", "dosis")])), "(una sola medición por combinación)\n")

cat("\nRegistros por día:\n");  print(table(datos$dias))
cat("\nRegistros por dosis:\n"); print(table(datos$dosis))
cat("\nDiseño día x dosis:\n");  print(table(dias = datos$dias, dosis = datos$dosis))


# Resumen numérico

asimetria <- function(v) mean((v - mean(v))^3) / mean((v - mean(v))^2)^1.5
curtosis  <- function(v) mean((v - mean(v))^4) / mean((v - mean(v))^2)^2 - 3

resumir <- function(v) {
  c(n         = length(v),
    media     = mean(v),
    mediana   = median(v),
    sd        = sd(v),
    cv        = 100 * sd(v) / mean(v),
    minimo    = min(v),
    q1        = unname(quantile(v, 0.25)),
    q3        = unname(quantile(v, 0.75)),
    maximo    = max(v),
    asimetria = asimetria(v),
    curtosis  = curtosis(v))
}

cat("\n==== 4. RESUMEN NUMÉRICO ====\n")
cat("Todas las variables:\n")
print(round(t(sapply(datos[variables], resumir)), 2))

cat("\nSST por dosis de 1-MCP:\n")
print(round(do.call(rbind, tapply(datos$sst, datos$dosis, resumir)), 2))

cat("\nSST por día de almacenamiento:\n")
print(round(do.call(rbind, tapply(datos$sst, datos$dias, resumir)), 2))

cat("\nSST promedio (filas = días, columnas = dosis):\n")
print(round(tapply(datos$sst, list(dias = datos$dias, dosis = datos$dosis), mean), 2))


# Forma de la distribución de SST

# Histograma
grafico_histograma <- ggplot(datos, aes(x = sst)) +
  geom_histogram(aes(y = after_stat(density)), bins = 6,
                 fill = "#7FA7C9", colour = "white") +
  geom_density(colour = "#1F4E79", linewidth = 0.8) +
  labs(title = "Distribución de los sólidos solubles totales",
       subtitle = "Histograma y curva de densidad",
       x = etiquetas["sst"], y = "Densidad")
print(grafico_histograma)

# Diagramas de caja por dosis y por día
grafico_caja_dosis <- ggplot(datos, aes(x = dosis_f, y = sst)) +
  geom_boxplot(fill = "#7FA7C9", outlier.colour = "#C0392B", width = 0.5) +
  geom_jitter(width = 0.08, colour = "#1F4E79", size = 2) +
  labs(title = "SST según la dosis de 1-MCP",
       x = etiquetas["dosis"], y = etiquetas["sst"])
print(grafico_caja_dosis)

grafico_caja_dias <- ggplot(datos, aes(x = factor(dias), y = sst)) +
  geom_boxplot(fill = "#7FA7C9", outlier.colour = "#C0392B", width = 0.5) +
  geom_jitter(width = 0.08, colour = "#1F4E79", size = 2) +
  labs(title = "SST según los días de almacenamiento",
       x = etiquetas["dias"], y = etiquetas["sst"])
print(grafico_caja_dias)

# Gráfico Q-Q
grafico_qq <- ggplot(datos, aes(sample = sst)) +
  stat_qq(colour = "#1F4E79", size = 2) +
  stat_qq_line(colour = "#C0392B") +
  labs(title = "Gráfico Q-Q de SST contra la distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles observados")
print(grafico_qq)

cat("\n==== 5. NORMALIDAD DE SST ====\n")
cat("Prueba de Shapiro-Wilk: p-valor =", round(shapiro.test(datos$sst)$p.value, 3), "\n")


#  Valores atípicos

es_atipico <- function(v) {
  ric <- IQR(v)
  v < quantile(v, 0.25) - 1.5 * ric | v > quantile(v, 0.75) + 1.5 * ric
}

cat("\n==== 6. VALORES ATÍPICOS DE SST (regla 1.5 × RIC) ====\n")
cat("Cantidad de atípicos:", sum(es_atipico(datos$sst)), "\n")

mas_alto <- datos[which.max(datos$sst), ]
mas_bajo <- datos[which.min(datos$sst), ]
cat("SST más alto: ", mas_alto$sst, " (día ", mas_alto$dias, ", dosis ", mas_alto$dosis, ")\n", sep = "")
cat("SST más bajo: ", mas_bajo$sst, " (día ", mas_bajo$dias, ", dosis ", mas_bajo$dosis, ")\n", sep = "")


# Relaciones entre variables

# Matriz de dispersión
grafico_pares <- ggpairs(datos[variables],
                         columnLabels = unname(etiquetas),
                         lower = list(continuous = wrap("points", alpha = 0.7,
                                                        colour = "#1F4E79")),
                         diag  = list(continuous = wrap("densityDiag",
                                                        fill = "#7FA7C9")),
                         progress = FALSE) +
  labs(title = "Relaciones entre las variables")
print(grafico_pares)

# Correlaciones de Pearson y Spearman
cor_pearson  <- cor(datos[variables], method = "pearson")
cor_spearman <- cor(datos[variables], method = "spearman")

cat("\n==== 7. CORRELACIONES ====\n")
cat("Pearson:\n");  print(round(cor_pearson, 2))
cat("\nSpearman:\n"); print(round(cor_spearman, 2))
cat("\n-> La correlación entre días y dosis es ", round(cor_pearson["dias", "dosis"], 2),
    ": el diseño es balanceado y las dos variables explicativas no están relacionadas.\n", sep = "")

# Evolución del SST por dosis
grafico_evolucion <- ggplot(datos, aes(x = dias, y = sst, colour = dosis_f,
                                       shape = dosis_f)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = sort(unique(datos$dias))) +
  labs(title = "Evolución del SST durante el almacenamiento",
       subtitle = "Una línea por dosis de 1-MCP",
       x = etiquetas["dias"], y = etiquetas["sst"],
       colour = "Dosis", shape = "Dosis")
print(grafico_evolucion)

# Forma de la relación SST ~ días
grafico_sst_dias <- ggplot(datos, aes(x = dias, y = sst)) +
  geom_point(colour = "#1F4E79", size = 2.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "#C0392B", linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE,
              colour = "#1F4E79") +
  labs(title = "SST frente a los días de almacenamiento",
       subtitle = "Ajuste lineal (rojo, discontinuo) y cuadrático (azul)",
       x = etiquetas["dias"], y = etiquetas["sst"])
print(grafico_sst_dias)

r2_lineal     <- summary(lm(sst ~ dias, data = datos))$r.squared
r2_cuadratico <- summary(lm(sst ~ dias + I(dias^2), data = datos))$r.squared
cat("\nR² de SST ~ días:           ", round(r2_lineal, 3), "\n")
cat("R² de SST ~ días + días²:   ", round(r2_cuadratico, 3), "\n")
