# Importo la libreria ggplot2
library(ggplot2)

file.choose()
# Asignacion de los datos en una variable, TRUE por que la primer linea son los titulos y "," porque se separan por ,
DB <- read.csv("/home/valen/itec/matematica/EFI_matematica_2/elbueno.csv", TRUE, ";")

# Los indices de los productos
namedb <- (DB$name)
sellerdb <- (DB$seller)
actualPricedb <- (DB$actual_price)
fechasdb <- (DB$json_fechas)
preciosdb <- (DB$json_precios)

fechasfactorizado <- c()
preciosfactorizado <- c()

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
  
  
  print("")
  print(vectorfecha)
  print(vectorprecio)  
  print("")
  # if (all(!(vectorfecha %in% fechasfactorizado))) {
  fechasfactorizado <- append(fechasfactorizado, vectorfecha)
  preciosfactorizado <- append(preciosfactorizado, vectorprecio)
  # }
}

datos <- data.frame(fechasfactorizado, preciosfactorizado)

promedio <- aggregate(preciosfactorizado ~ fechasfactorizado, data = datos, FUN = mean)

# Hago el grafico de regresion lineal con los datos actuales
ggplot(data=promedio, aes(x=fechasfactorizado, y=preciosfactorizado)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 0.1) + 
  labs(title = "Regresion lineal datos actualidad",
       x = "Fechas",
       y = "Precios") +
  theme_light()


# Para usar cor.tests
fechas_numeric <- as.numeric(promedio$fechasfactorizado)

# Veo si hay corelaccion entre las variables
print(cor.test(promedio$preciosfactorizado, fechas_numeric))

# Ajusto un modelo de regresion lineal en base a los datos
model <- lm(promedio$preciosfactorizado ~ promedio$fechasfactorizado, data = promedio)
summary(model)


# Modelo de prediccion 
max_fecha <- max(promedio$fechasfactorizado)

# Creo un data frame con los 5 dias posteriores a la ultima fecha con registro
dfDiasFuturos <- data.frame(promedio = seq(max_fecha + 1, by = "day", length.out = 5))

# Prediccion precios en base a los dias futuros
predicciones <- predict(model, dfDiasFuturos)
# Error in promedio$fechasfactorizado : 
#   $ operator is invalid for atomic vectors













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




