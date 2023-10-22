# Importo la libreria ggplot2
library(ggplot2)

# Asignacion de los datos en una variable, TRUE por que la primer linea son los titulos y "," porque se separan por ,
DB <- read.csv("/home/dyz/itec/matematica/EFI2/elbueno.csv", TRUE, ";")

# Ejemplo de como se ve la informacion en el csv
head(DB)
# id name       seller       actual_price    json_fechas     json_precios
# 1  INTEL I5   HardVision   114734          ["21/9",...]    [120000,...]


# Recupero las fechas y los indices de precios de produccion
namedb <- (DB$name)
sellerdb <- (DB$seller)
actualPricedb <- (DB$actual_price)
fechasdb <- (DB$json_fechas)
preciosdb <- (DB$json_precios)

string_fecha <- fechasdb[1]

string_fecha <- gsub("\"", "", string_fecha)
string_fecha <- gsub("[\\[\\]]", "", string_fecha)
epa = c(1,1,2,3,4)
typeof(epa)
string_fecha <- gsub("[\\[\\]\"]", "", string_fecha)
string_fecha <- substring(string_fecha, 2, nchar(string_fecha) -1)
string_fecha
for (variable in string_fecha) {
  print(variable)
}

rm(list=ls())

string_fecha <- strsplit(string_fecha, ",")[[1]]

array_fechas <- as.Date(string_fecha, format= "%d%m")

array_fechas

preciosdb[1]




# Creo un data frame con los datos anteriormente recuperados
data <- data.frame(Price,DateDB)

# Hago el grafico de regresion lineal
ggplot(data=data, aes(x=DateDB, y=Price)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Puntos de precio de produccion de hardware en estados unidos frente a el paso del tiempo",
       x = "Fecha",
       y = "Indice de precios de produccion") +
  theme_light()


# Para usar cor.tests
DateDB_numeric <- as.numeric(DateDB)
# Veo si hay corelaccion entre las variables
print(cor.test(Price, DateDB_numeric))

# Ajusto un modelo de regresion lineal en base a los datos
model <- lm(Price ~ DateDB, data = DB)
summary(model)


# Modelo de prediccion 

datosPredecidosEnero2024 <- data.frame(DateDB = as.Date("2024-01-01"))

datosPredecidosEnero2025 <- data.frame(DateDB = as.Date("2025-01-01"))

datosPredecidosEnero2026 <- data.frame(DateDB = as.Date("2026-01-01"))

pricePredict2024 <- predict(model, datosPredecidosEnero2024)

pricePredict2025 <- predict(model, datosPredecidosEnero2025)

pricePredict2026 <- predict(model, datosPredecidosEnero2026)


rm(list=ls())

# Crea un nuevo marco de datos con las fechas futuras
future_DateDBs <- seq(from = as.Date("2024-01-01"), by = "month", length.out = 24)



