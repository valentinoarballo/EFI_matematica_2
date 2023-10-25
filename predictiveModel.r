# Importo la libreria ggplot2
library(ggplot2)
library(dplyr)

# file.choose()
# Asignacion de los datos en una variable, TRUE por que la primer linea son los titulos y "," porque se separan por ,
DB <- read.csv("/home/valen/itec/matematica/EFI_matematica_2/elbueno.csv", TRUE, ";")
DB <- read.csv("/home/valen/itec/matematica/EFI_matematica_2/elbueno.csv", TRUE, ";")

# Los indices de los productos
# namedb <- (DB$name)
# sellerdb <- (DB$seller)
# actualPricedb <- (DB$actual_price)
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
dfDiasFuturos <- data.frame(fechasfuturo = seq((max_fecha+1) + 1, by = "day", length.out = 31))

# typeof(datos$fechasdf[1])
# > typeof(datos$fechasdf)
# [1] "double"

# 
# typeof(dfDiasFuturos$fechasfuturo)
# > typeof(dfDiasFuturos$fechasfuturo)
# [1] "double"

predicciones <- predict(model, newdata = (datos$fechasdf = dfDiasFuturos$fechasfuturo))
predicciones
# 1        2        3        4        5        6        7        8        9       10       11       12       13       14 
# 220331.9 223124.4 225916.8 228709.3 231501.8 234294.3 237086.8 239879.2 242671.7 245464.2 248256.7 251049.1 253841.6 256634.1 
# 15       16       17       18       19       20       21       22       23       24       25       26       27       28 
# 259426.6 262219.0 265011.5 267804.0 270596.5 273389.0 276181.4 278973.9 281766.4 284558.9 287351.3 290143.8 292936.3 295728.8 
# 29       30       31 
# 298521.2 301313.7 304106.2 


# Creo un data frame con las fechas futuras y los precios predecidos 
predicciondf <- data.frame(fechasdf = dfDiasFuturos$fechasfuturo, preciosdf = predicciones)
# > dfPredicciones <- data.frame(Price = predicciones, DateDB = dfDiasFuturos$DateDB)
# Error in data.frame(Price = predicciones, DateDB = dfDiasFuturos$DateDB) : 
#   arguments imply differing number of rows: 31, 0

# Concatenacion del data frame original mas las predicciones
copiapredicciondf <- rbind(datosrbind, predicciondf)

# Grafico con las predicciones
ggplot(data=copiapredicciondf, aes(x=fechasdf, y=preciosdf)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 1) + 
  labs(title = "Prediccion de precios",
       x = "Fechas",
       y = "Precios ") +
  theme_light()


rm(list=ls())




