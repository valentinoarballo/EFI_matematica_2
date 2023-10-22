# Importo la libreria ggplot2
library(ggplot2)

# Asignacion de los datos en una variable, TRUE por que la primer linea son los titulos y "," porque se separan por ,
DB <- read.csv("/home/dyz/itec/matematica/EFI2/elbueno.csv", TRUE, ";")

# Los indices de los productos
namedb <- (DB$name)
sellerdb <- (DB$seller)
actualPricedb <- (DB$actual_price)
fechasdb <- (DB$json_fechas)
preciosdb <- (DB$json_precios)

# Deserializacion de la informacion
# string_precio <- preciosdb[4]
string_precio <- "[143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,143181,156268,164081,164081,164081,164081,164081,164081]"
string_precio <- substring(string_precio, 2, nchar(string_precio) -1)
string_precio <- strsplit(string_precio, ",")[[1]]
vectorprecio <- as.vector(string_precio)
vectorprecio <- as.integer(vectorprecio)

# Deserializacion de la informacion
# string_fecha <- fechasdb[4]
string_fecha <- "[\"21/9\",\"22/9\",\"23/9\",\"24/9\",\"25/9\",\"26/9\",\"27/9\",\"28/9\",\"29/9\",\"30/9\",\"1/10\",\"2/10\",\"3/10\",\"4/10\",\"5/10\",\"6/10\",\"7/10\",\"8/10\",\"9/10\",\"10/10\",\"11/10\",\"12/10\",\"13/10\",\"14/10\",\"15/10\",\"16/10\"]"
string_fecha <- substring(string_fecha, 2, nchar(string_fecha) -1)
string_fecha <- gsub("\"", "", string_fecha)
string_fecha <- strsplit(string_fecha, ",")[[1]]
vectorfecha <- as.vector(string_fecha)
vectorfecha <- as.Date(string_fecha, format = "%d/%m")

DateDB<-vectorfecha
Price<-vectorprecio

# Creo un data frame con los datos anteriormente recuperados
data <- data.frame(Price,DateDB)

# Hago el grafico de regresion lineal con los datos actuales
ggplot(data=data, aes(x=DateDB, y=Price)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 0.1) + 
  labs(title = namedb[4],
       x = "Fecha",
       y = paste("Precio en la tienda ",sellerdb[4])) +
  theme_light()


# Para usar cor.tests
DateDB_numeric <- as.numeric(DateDB)

# Veo si hay corelaccion entre las variables
print(cor.test(Price, DateDB_numeric))

# Ajusto un modelo de regresion lineal en base a los datos
model <- lm(Price ~ DateDB, data = DB)
summary(model)


# Modelo de prediccion 
max_fecha <- max(vectorfecha)

# Creo un data frame con los 5 dias posteriores a la ultima fecha con registro
dfDiasFuturos <- data.frame(DateDB = seq(max_fecha + 1, by = "day", length.out = 5))

# Prediccion precios en base a los dias futuros
predicciones <- predict(model, dfDiasFuturos)

# Creo un data frame con las fechas futuras y los precios predecidos 
dfPredicciones <- data.frame(Price = predicciones, DateDB = dfDiasFuturos$DateDB)

# Concatenacion del data frame original mas las predicciones
prediccionData <- rbind(data, dfPredicciones)

# Grafico con las predicciones
ggplot(data=prediccionData, aes(x=DateDB, y=Price)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 1) + 
  labs(title = paste("prediccion de ", namedb[4]),
       x = "Fecha",
       y = paste("Precio en la tienda ",sellerdb[4])) +
  theme_light()


rm(list=ls())




