
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

# Instalar paquetes (solo la primera vez; luego se pueden dejar comentados)
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("writexl")

# Cargar paquetes y datos
library(FactoMineR)
library(corrplot)
library(writexl)
data("decathlon")

# 1. Seleccionar únicamente las 10 pruebas del decatlón
competencias <- decathlon[, 1:10]

# 2. Calcular la matriz de correlación de Pearson
matriz_correlaciones <- cor(competencias)

# 3. Redondear a 3 decimales y mostrar la matriz
matriz_correlaciones_redondeada <- round(matriz_correlaciones, 3)
print(matriz_correlaciones_redondeada)

# 4. Guardar la matriz en un archivo Excel
tabla_cor <- data.frame(Prueba = rownames(matriz_correlaciones_redondeada),
                        matriz_correlaciones_redondeada,
                        check.names = FALSE)
write_xlsx(tabla_cor, "matriz_correlacion_decathlon.xlsx")

# 5. Mapa de calor de las correlaciones
corrplot(matriz_correlaciones,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Matriz de Correlación - Eventos del Decatlón",
         mar = c(0, 0, 2, 0))


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

   pv <- decathlon$Pole.vault
media <- mean(pv)
CV <- sd(pv) / media * 100

m2 <- mean((pv - media)^2)
m3 <- mean((pv - media)^3)
m4 <- mean((pv - media)^4)
asimetria <- m3 / m2^(3/2)
curtosis  <- m4 / m2^2

media; sd(pv); CV; asimetria; curtosis

hist(pv, breaks = seq(4.15, 5.45, by = 0.1), freq = FALSE,
     col = "lightsteelblue", border = "white",
     main = "Histograma - Pole.vault", xlab = "Altura (m)", ylab = "Densidad")
lines(density(pv), lwd = 2, col = "darkblue")
abline(v = media, col = "red", lwd = 2, lty = 2)




#Punto 8
datos <- decathlon
par(mfrow = c(2, 3), mar = c(4.5, 4.5, 3, 1))

# Punto 3: 100m vs 400m
plot(datos$100m, datos$400m, pch = 19, col = "steelblue",
     main = paste0("100m vs 400m (r = ",
                   round(cor(datos$100m, datos$400m), 3), ")"),
     xlab = "100m (s)", ylab = "400m (s)")
abline(lm(400m ~ 100m, data = datos), col = "red", lwd = 2)

# Punto 5: Discus por competencia
boxplot(Discus ~ Competition, data = datos, col = c("orange", "lightgreen"),
        main = "Discus por competencia", xlab = "Competencia", ylab = "Distancia (m)")

# Punto 5: Javeline por competencia
boxplot(Javeline ~ Competition, data = datos, col = c("orange", "lightgreen"),
        main = "Javeline por competencia", xlab = "Competencia", ylab = "Distancia (m)")

# Punto 6: Long.jump vs High.jump
plot(datos$Long.jump, datos$High.jump, pch = 19, col = "darkorange",
     main = paste0("Long.jump vs High.jump (r = ",
                   round(cor(datos$Long.jump, datos$High.jump), 3), ")"),
     xlab = "Long.jump (m)", ylab = "High.jump (m)")
abline(lm(High.jump ~ Long.jump, data = datos), col = "red", lwd = 2)

# Punto 7: Histograma de Pole.vault
hist(pv, breaks = seq(4.15, 5.45, by = 0.1), freq = FALSE,
     col = "lightsteelblue", border = "white",
     main = "Histograma - Pole.vault", xlab = "Altura (m)", ylab = "Densidad")
lines(density(pv), lwd = 2, col = "darkblue")
abline(v = media, col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))


## PUNTO 8:

   par(mfrow = c(2, 3))

# Punto 3
plot(decathlon$100m, decathlon$400m,
     xlab = "100m (s)",
     ylab = "400m (s)",
     main = "Relación entre 100m y 400m")

# Punto 5
boxplot(Discus ~ Competition, data = decathlon,
        main = "Lanzamiento de Disco",
        xlab = "Competencia",
        ylab = "Distancia (metros)",
        col = c("skyblue", "orange"))

boxplot(Javeline ~ Competition, data = decathlon,
        main = "Lanzamiento de Jabalina",
        xlab = "Competencia",
        ylab = "Distancia (metros)",
        col = c("lightgreen", "coral"))

# Punto 6
plot(decathlon$Long.jump, decathlon$High.jump,
     xlab = "Long.jump (m)",
     ylab = "High.jump (m)",
     main = "Relación entre Long.jump y High.jump")

# Punto 7
pv <- decathlon$Pole.vault
hist(pv, breaks = seq(4.15, 5.45, by = 0.1), freq = FALSE,
     col = "lightsteelblue", border = "white",
     main = "Histograma - Pole.vault", xlab = "Altura (m)", ylab = "Densidad")
lines(density(pv), lwd = 2, col = "darkblue")
abline(v = mean(pv), col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))
