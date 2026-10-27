# ANALISIS DESCRIPTIVO - DATOS DEL LULO

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl", repos = "https://cloud.r-project.org")
}

archivo <- "datos.xlsx"
if (!file.exists(archivo)) archivo <- file.path("Articulo_regresion", "datos.xlsx")
if (!file.exists(archivo)) archivo <- file.choose()


datos <- readxl::read_excel(archivo, sheet = 1, col_names = FALSE)
datos <- as.data.frame(datos[, 1:3])
names(datos) <- c("dias", "dosis", "sst")
datos[] <- lapply(datos, function(x) suppressWarnings(as.numeric(x)))
datos <- na.omit(datos)


cat("\nANALISIS DESCRIPTIVO DEL LULO\n")
cat("\nCantidad de registros y variables:\n")
print(dim(datos))
cat("\nPrimeros registros:\n")
print(head(datos))
cat("\nResumen estadistico:\n")
print(summary(datos))
cat("\nDesviacion estandar:\n")
print(sapply(datos, sd))
cat("\nVarianza:\n")
print(sapply(datos, var))
cat("\nPromedio de SST por dosis:\n")
print(aggregate(sst ~ dosis, data = datos, mean))
cat("\nPromedio de SST por dia:\n")
print(aggregate(sst ~ dias, data = datos, mean))
cat("\nCorrelacion entre las variables:\n")
print(cor(datos))


hist(datos$sst, main = "Histograma de SST", xlab = "SST (grados Brix)")
boxplot(sst ~ factor(dosis), data = datos,
        main = "SST por dosis", xlab = "Dosis de 1-MCP (ug/L)",
        ylab = "SST (grados Brix)")
plot(datos$dias, datos$sst,
     main = "SST y dias de almacenamiento",
     xlab = "Dias", ylab = "SST (grados Brix)", pch = 19)

