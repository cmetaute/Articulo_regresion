# ANALISIS DESCRIPTIVO - DATOS DEL LULO

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl", repos = "https://cloud.r-project.org")
}
url_datos <- paste0("https://raw.githubusercontent.com/",
                    "cmetaute/Articulo_Regresion/main/datos.xlsx")



datos <- readxl::read_excel(archivo, sheet = 1, col_names = FALSE)
datos <- as.data.frame(datos[, 1:3])
names(datos) <- c("dias", "dosis", "sst")
datos[] <- lapply(datos, function(x) suppressWarnings(as.numeric(x)))
datos <- na.omit(datos)


cat("\nANALISIS DESCRIPTIVO DEL LULO\n")
print(dim(datos))
print(head(datos))
print(summary(datos))
print(sapply(datos, sd))
print(sapply(datos, var))
print(aggregate(sst ~ dosis, data = datos, mean))
print(aggregate(sst ~ dias, data = datos, mean))
print(cor(datos))


hist(datos$sst, main = "Histograma de SST", xlab = "SST (grados Brix)")
boxplot(sst ~ factor(dosis), data = datos,
        main = "SST por dosis", xlab = "Dosis de 1-MCP (ug/L)",
        ylab = "SST (grados Brix)")
plot(datos$dias, datos$sst,
     main = "SST y dias de almacenamiento",
     xlab = "Dias", ylab = "SST (grados Brix)", pch = 19)

