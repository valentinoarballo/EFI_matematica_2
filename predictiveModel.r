cat("Analisis predictivo sobre el precio de un producto determinado")

# Importacio de librerias
library(ggplot2)
library(dplyr)
library(scales)
library(dotenv)
library(httr)

cat("Carga variables de entorno, una secret key vinculada a mi cuenta personal en una API, lo hago asi para no subir mi llave a github")
dotenv::load_dot_env()

file.choose()
cat("Asignacion de los datos en una variable, TRUE por que la primer linea son los titulos y ';' porque se separan por ;")
DB <- read.csv("/home/dyz/itec/matematica/EFI2/elbueno.csv", TRUE, ";")
DB <- read.csv("/home/valen/itec/matematica/EFI_matematica_2/elbueno.csv", TRUE, ";")

cat("Los indices de los productos, cada fila representara un producto de una tienda individual, se recuperan entre 40 y 60 elementos dependiendo de el producto que se busque, cada producto ademas tiene un historial de el precio que tubo en los ultimos 31 dias")
seller <- (DB$seller)
productname <- (DB$name)
fechasdb <- (DB$json_fechas)
preciosdb <- (DB$json_precios)
precioactual <- (DB$actual_price)

cat("con esta base de datos generada por el webscraper podemos obtener otros datos interesantes, como la tienda web con los precios mas bajos para este producto")
cat("\n por ejemplo, en", seller[1], " venden ", productname[1], " por $", precioactual[1], "siendo la tienda mas barata (que encontro el web scrapper) con este producto")

cat("Creo array vacios para desp")
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
  geom_area(stat = "identity", fill = 4, alpha = 0.5, color = 1, lwd = 0.5, linetype = 1) +
  scale_y_continuous(labels = comma, limits = c(0, 400000), breaks = seq(0, 400000, 25000), expand = c(0, 0)) +
  labs(title = "Prediccion de precios",
       x = "Fechas",
       y = "Precios ") +
  theme_light()

copiaDatos <- datosrbind

rates <- c()

endpoint <- "http://api.exchangeratesapi.io/v1/"
symbols <- "ARS"
key <- Sys.getenv("API_KEY")
# for (i in 1:length(copiaDatos$fechasdf)) {
  # Sys.sleep(1)
  cat("\n", (length(copiaDatos$fechasdf) - i), "operaciones restantes \n")
  cat(copiaDatos$fechasdf[i], " -- ")
  
  date <- copiaDatos$fechasdf[i]
  # Construir la URL con el endpoint, la clave, la fecha y el símbolo
  url <- paste0(endpoint, date, "?access_key=", key, "&symbols=", symbols)
  # Hacer la petición GET a la URL y obtener el contenido como una lista
  response <- GET(url)
  content <- content(response, as = "parsed")
  # Acceder al valor de ARS dentro de la lista y guardarlo en la lista de tasas
  rate <- content$rates$ARS
  cat(rate)
  rates <- c(rates, rate)
}

rates <- rev(rates)
  
for (i in 1: length(copiaDatos$preciosdf)) {
  precios_pesos <- copiaDatos$preciosdf[i]
  tasa_cambio <- rates[i]
  precio_en_euros <- round(precios_pesos / tasa_cambio, 2)
  copiaDatos$preciosdf[i] <- precio_en_euros
}

rm(copiaDatos)

ggplot(data=copiaDatos, aes(x=fechasdf, y=preciosdf)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 1) + 
  labs(title = "Regresion lineal datos actualidad",
       x = "Fechas",
       y = "Precios") +
  theme_light()

cat("Para chequear la fuerza de la correlacion en euros")
# Para usar cor.tests
fechas_numericeuros <- as.numeric(copiaDatos$fechasdf)

# Veo si hay corelaccion entre las variables
print(cor.test(copiaDatos$preciosdf, fechas_numericeuros))
cat("La prediccion en base a los datos actuales pasada a euros")
# Ajusto un modelo de regresion lineal en base a los datos
modeleuros <- lm((preciosdf = copiaDatos$preciosdf) ~ (fechasdf = copiaDatos$fechasdf), data = copiaDatos)
summary(modeleuros)

copiaDatosrbind <- copiaDatos

prediccioneseuro <- predict(modeleuros, newdata = (copiaDatos$fechasdf = dfDiasFuturos$fechasfuturo))

# Creo un data frame con las fechas futuras y los precios predecidos 
predicciondfeuro <- data.frame(fechasdf = dfDiasFuturos$fechasfuturo, preciosdf = prediccioneseuro)


# Concatenacion del data frame original mas las predicciones
copiapredicciondf <- rbind(copiaDatosrbind, predicciondfeuro)

ggplot(data=copiapredicciondf, aes(x=fechasdf, y=preciosdf)) +
  geom_area(stat = "identity", fill = 4, alpha = 0.5, color = 1, lwd = 0.5, linetype = 1) +
  
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 100)) +

    labs(title = "Prediccion de precios",
       x = "Fechas",
       y = "Precios ") +
  
  theme_light()


# rm(list=ls())




