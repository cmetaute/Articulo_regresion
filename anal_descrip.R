# 1. PAQUETE Y LECTURA DE DATOS ------------------------------------------
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl", repos = "https://cloud.r-project.org")
}

archivo <- "Datos Regresion.xlsx"
if (!file.exists(archivo)) {
  stop("No encuentro datos.xlsx. Abre RStudio con la carpeta del repositorio como directorio de trabajo.")
}
datos <- as.data.frame(readxl::read_excel(archivo, sheet = "datos"))
columnas <- c("dias_almacenamiento", "dosis_1mcp_ug_L", "sst_brix")
stopifnot(all(columnas %in% names(datos)))
datos <- datos[, columnas]
stopifnot(all(vapply(datos, is.numeric, logical(1))))



cat("\n--- ESTRUCTURA DE LA BASE ---\n")
str(datos)
cat("\n--- PRIMERAS FILAS ---\n")
print(head(datos))
cat("\n--- DIMENSIONES (filas, columnas) ---\n")
print(dim(datos))
cat("\n--- DATOS FALTANTES POR VARIABLE ---\n")
print(colSums(is.na(datos)))
cat("\n--- FILAS DUPLICADAS ---\n")
print(sum(duplicated(datos)))

# 3. ESTADISTICA DESCRIPTIVA GLOBAL --------------------------------------
cat("\n--- RESUMEN ESTADISTICO ---\n")
print(summary(datos))

estadisticos <- function(x) {
  x <- x[!is.na(x)]
  c(n = length(x), media = mean(x), mediana = median(x),
    desviacion_estandar = sd(x), varianza = var(x),
    minimo = min(x), q1 = unname(quantile(x, 0.25)),
    q3 = unname(quantile(x, 0.75)), maximo = max(x),
    rango = diff(range(x)))
}

resumen_global <- as.data.frame(t(vapply(datos, estadisticos, numeric(10))))
resumen_global$variable <- rownames(resumen_global)
resumen_global <- resumen_global[, c("variable", setdiff(names(resumen_global), "variable"))]
rownames(resumen_global) <- NULL
print(resumen_global, row.names = FALSE)

# 4. DESCRIPCION DE LAS CONDICIONES EXPERIMENTALES -------------------------
cat("\n--- FRECUENCIA POR DIA ---\n")
print(table(datos$dias_almacenamiento))
cat("\n--- FRECUENCIA POR DOSIS ---\n")
print(table(datos$dosis_1mcp_ug_L))

cat("\n--- SST POR DOSIS DE 1-MCP ---\n")
por_dosis <- aggregate(sst_brix ~ dosis_1mcp_ug_L, datos, estadisticos)
print(por_dosis)

cat("\n--- SST POR DIA DE ALMACENAMIENTO ---\n")
por_dia <- aggregate(sst_brix ~ dias_almacenamiento, datos, estadisticos)
print(por_dia)

cat("\n--- CORRELACIONES (DESCRIPTIVAS) ---\n")
print(cor(datos, use = "complete.obs", method = "pearson"))

# 5. GUARDAR RESULTADOS EN UNA CARPETA ------------------------------------
dir.create("resultados", showWarnings = FALSE)
write.csv(resumen_global, "resultados/resumen_global.csv", row.names = FALSE)
write.csv(por_dosis, "resultados/resumen_por_dosis.csv", row.names = FALSE)
write.csv(por_dia, "resultados/resumen_por_dia.csv", row.names = FALSE)

# 6. GRAFICAS DESCRIPTIVAS (R BASE) ---------------------------------------
png("resultados/histograma_sst.png", width = 1000, height = 700)
hist(datos$sst_brix, breaks = 6,
     main = "Distribucion de solidos solubles totales",
     xlab = "SST (grados Brix)", ylab = "Frecuencia")
dev.off()

png("resultados/caja_sst_por_dosis.png", width = 1000, height = 700)
boxplot(sst_brix ~ factor(dosis_1mcp_ug_L), data = datos,
        main = "SST segun dosis de 1-MCP", xlab = "Dosis de 1-MCP (ug/L)",
        ylab = "SST (grados Brix)")
dev.off()

png("resultados/sst_por_dias_y_dosis.png", width = 1100, height = 750)
dosis <- sort(unique(datos$dosis_1mcp_ug_L))
matriz <- with(datos, tapply(sst_brix,
                             list(dias_almacenamiento, dosis_1mcp_ug_L), mean))
matplot(as.numeric(rownames(matriz)), matriz, type = "b", pch = 1:3,
        lty = 1:3, xlab = "Dias de almacenamiento",
        ylab = "SST (grados Brix)",
        main = "SST durante el almacenamiento por dosis")
legend("topright", legend = paste(dosis, "ug/L"), col = 1:3,
       pch = 1:3, lty = 1:3, title = "Dosis 1-MCP")
dev.off()



