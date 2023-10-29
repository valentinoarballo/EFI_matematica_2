# Importo la libreria ggplot2
library(ggplot2)
library(dplyr)
library(scales)

file.choose()
# Asignacion de los datos en una variable, TRUE por que la primer linea son los titulos y "," porque se separan por ,
DB <- read.csv("/home/dyz/itec/matematica/EFI2/elbueno.csv", TRUE, ";")
DB <- read.csv("/home/valen/itec/matematica/EFI_matematica_2/elbueno.csv", TRUE, ";")

# Los indices de los productos
fechasdb <- (DB$json_fechas)
preciosdb <- (DB$json_precios)

fechasv <- c()
preciosv <- c()

for (x in 1:length(fechasdb)) {

  string_fecha <- fechasdb[x]
  string_fecha <- substring(string_fecha, 2, nchar(string_fecha) -1)
  string_fecha <- gsub("\"", "", string_fecha)
  string_fecha <- strsplit(string_fecha, ",")[[1]]
  vectorfecha <- as.vector(string_fecha)
  vectorfecha <- as.Date(string_fecha, format = "%d/%m")
  
  string_precio <- preciosdb[x]
  string_precio <- substring(string_precio, 2, nchar(string_precio) -1)
  string_precio <- strsplit(string_precio, ",")[[1]]
  vectorprecio <- as.vector(string_precio)
  vectorprecio <- as.integer(vectorprecio)
  
  fechasv <- append(fechasv, vectorfecha)
  preciosv <- append(preciosv, vectorprecio)
  
}

datos <- data.frame(fechasdf = fechasv, preciosdf = preciosv)
datos <- aggregate(preciosdf ~ fechasdf, data = datos, FUN = mean)
datosrbind <- datos

# Hago el grafico de regresion lineal con los datos actuales
ggplot(data=datos, aes(x=fechasdf, y=preciosdf)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 1) + 
  labs(title = "Regresion lineal datos actualidad",
       x = "Fechas",
       y = "Precios") +
  theme_light()

cat("Para chequear la fuerza de la correlacion")
# Para usar cor.tests
fechas_numeric <- as.numeric(datos$fechasdf)

# Veo si hay corelaccion entre las variables
print(cor.test(datos$preciosdf, fechas_numeric))

cat("La prediccion en base a los datos actuales")
# Ajusto un modelo de regresion lineal en base a los datos
model <- lm((preciosdf = datos$preciosdf) ~ (fechasdf = datos$fechasdf), data = datos)
summary(model)

# Ultimo registro
max_fecha <- max(datos$fechasdf) 

# Creo un data frame con 10 registros posteriores a el ultimo
dfDiasFuturos <- data.frame(fechasfuturo = seq((max_fecha) + 1, by = "day", length.out = 31))

predicciones <- predict(model, newdata = (datos$fechasdf = dfDiasFuturos$fechasfuturo))

# Creo un data frame con las fechas futuras y los precios predecidos 
predicciondf <- data.frame(fechasdf = dfDiasFuturos$fechasfuturo, preciosdf = predicciones)

# Concatenacion del data frame original mas las predicciones
copiapredicciondf <- rbind(datosrbind, predicciondf)


ggplot(data=copiapredicciondf, aes(x=fechasdf, y=preciosdf)) +
  # geom_bar(stat = "identity") +
  geom_area(stat = "identity", fill = 4, alpha = 0.5, color = 1, lwd = 0.5, linetype = 1) +
  scale_y_continuous(labels = comma, limits = c(0, 400000), breaks = seq(0, 400000, 25000), expand = c(0, 0)) +
  labs(title = "Prediccion de precios",
       x = "Fechas",
       y = "Precios ") +
  theme_light()


rm(list=ls())




