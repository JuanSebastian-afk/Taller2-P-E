
## PUNTO 1: Instalación y carga de paquetes requerridos

install.packages("FactoMineR")  ## instala el paquete
library(FactoMineR)  ## llama el paquete

data("decathlon")  ## llama los datos decatlon



## PUNTO 2: Estadisticas descriptivas

# Seleccionar solo las columnas numéricas
datos_num <- decathlon[, 1:12]

# Construir la tabla con las estadísticas
estadisticas <- data.frame(
  Variable = colnames(datos_num),
  Promedio = round(apply(datos_num, 2, mean, na.rm = TRUE), 2),
  Mediana  = round(apply(datos_num, 2, median, na.rm = TRUE), 2),
  Varianza = round(apply(datos_num, 2, var, na.rm = TRUE), 2),
  Q1       = round(apply(datos_num, 2, quantile, probs = 0.25, na.rm = TRUE), 2),
  Q3       = round(apply(datos_num, 2, quantile, probs = 0.75, na.rm = TRUE), 2),
  Minimo   = round(apply(datos_num, 2, min, na.rm = TRUE), 2),
  Maximo   = round(apply(datos_num, 2, max, na.rm = TRUE), 2),
  row.names = NULL
)

# Exportar a un archivo Excel
write_xlsx(estadisticas, "estadisticas_descriptivas_decathlon.xlsx")



## PUNTO 3: Diagrama de disperción calculo de corelación para 100m y 400m

cor(decathlon$100m, decathlon$400m)

plot(decathlon$100m, decathlon$400m,
     xlab = "100m (s)",
     ylab = "400m (s)",
     main = "Relación entre 100m y 400m")


## PUNTO 4: Calculo de los coeficientes de correlación



## PUNTO 5: Diagrama de cajas para las pruebas Discus y Javeline

# Dividir la pantalla para ver los 2 gráficos juntos
par(mfrow = c(1, 2))

# Boxplot para Discus por Competition
boxplot(Discus ~ Competition, data = decathlon,
        main = "Lanzamiento de Disco",
        xlab = "Competencia",
        ylab = "Distancia (metros)",
        col = c("skyblue", "orange"))

# Boxplot para Javeline por Competition
boxplot(Javeline ~ Competition, data = decathlon,
        main = "Lanzamiento de Jabalina",
        xlab = "Competencia",
        ylab = "Distancia (metros)",
        col = c("lightgreen", "coral"))

# Ver si hay atípicos numéricos en Discus
cat("Atípicos en Discus:\n")
by(decathlon$Discus, decathlon$Competition, function(x) boxplot.stats(x)$out)

# Ver si hay atípicos numéricos en Javeline
cat("\nAtípicos en Javeline:\n")
by(decathlon$Javeline, decathlon$Competition, function(x) boxplot.stats(x)$out)


## PUNTO 6:
   
cor(decathlon$Long.jump, decathlon$High.jump)

plot(decathlon$Long.jump, decathlon$High.jump,
     xlab = "Long.jump (m)",
     ylab = "High.jump (m)",
     main = "Relación entre Long.jump y High.jump")


   
## PUNTO 7:


## PUNTO 8:
