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

cat("Creo array vacios para despues agregar todos los datos con los que voy a trabajar, vectores paralelos con fecha yyyy-mm--dd y precio por historial de cada producto, es decir de momento van a haber fechas repetidas, ya que puede estar el registro de diferentes precios de ,por ejemplo, procesadores en 50 tiendas para el dia 2021-10-25")

fechasv <- c()
preciosv <- c()


for (x in 1:length(fechasdb)) {

  # Toda esta seccion basicamente lo que hace es pasar cada elemento fechasdb a un formato en el que pueda trabajarlo con R
  # Esto es necesario, porq desde JavaScript a los Arrays de historiales fechas[] y precios[] los transformo con una funcion llamada JSON_jsonify(), esto me permite subirlo mas facilmente con sequelize (la libreria para hacer la conexion js <-> sql) a la base de datos
  # El inconveniente esta en que tengo que hacer estas lineas extra en R, pero me ahorra mas pasos por el otro lado del programa 
  string_fecha <- fechasdb[x]
  # > string_fecha  "[\"21/9\",\"22/9\",\"23/9\",\"24/9\",···]"
  
  string_fecha <- substring(string_fecha, 2, nchar(string_fecha) -1)
  # > string_fecha  "\"21/9\",\"22/9\",\"23/9\",\"24/9\",···"     
  
  string_fecha <- gsub("\"", "", string_fecha)
  # > string_fecha  "21/9,22/9,23/9,24/9,···"  
  
  string_fecha <- strsplit(string_fecha, ",")[[1]]
  # > string_fecha "21/9"  "22/9"  "23/9"  "24/9"
  
  vectorfecha <- as.vector(string_fecha)
  # Por las dudas le aclaro que es de tipo vector
  
  vectorfecha <- as.Date(string_fecha, format = "%d/%m")
  # Le asigno a cada variable del vector felcha su tipo y en que formato esta 
  
  
  
  string_precio <- preciosdb[x]
  # > string_precio   "[318637,318637,318637,318637,···]"
  
  string_precio <- substring(string_precio, 2, nchar(string_precio) -1)
  # > string_precio   "318637,318637,318637,318637,···"
  
  string_precio <- strsplit(string_precio, ",")[[1]]
  # > string_precio   "318637" "18637" "318637" "318637" ···
  
  vectorprecio <- as.vector(string_precio)
  vectorprecio <- as.integer(vectorprecio)
  # Cada elemento del vector quiero que sea un entero
  
  # Se "pushean" los vectores 
  fechasv <- append(fechasv, vectorfecha)
  preciosv <- append(preciosv, vectorprecio)
}

cat("Genero un data frame con los vectores obtenidos")
datos <- data.frame(fechasdf = fechasv, preciosdf = preciosv)

cat("Utilizo la funcion aggregate, para calcular resumenes estadísticos para subconjuntos de datos. Es muy similar a la función tapply, en este caso la salida es un data frame")
datos <- aggregate(preciosdf ~ fechasdf, data = datos, FUN = mean)

cat("Le hago una 'copia' a este data frame para utilizarlo luego")
datosrbind <- datos

cat("Hago el grafico de regresion lineal con los datos actuales")
ggplot(data=datos, aes(x=fechasdf, y=preciosdf)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 1) + 
  labs(title = "Regresion lineal datos actualidad",
       x = "Fechas",
       y = "Precios") +
  theme_light()

cat("Para chequear la correlacion tengo que pasar las fechas a un formato numerico")
fechas_numeric <- as.numeric(datos$fechasdf)

cat("Veo si hay corelaccion entre las variables")
print(cor.test(datos$preciosdf, fechas_numeric))

cat("Ajusto un modelo de regresion lineal en base a los datos")
model <- lm((preciosdf = datos$preciosdf) ~ (fechasdf = datos$fechasdf), data = datos)
print(summary(model))

cat("Esto es solo para saber cual es el ultimo registro")
max_fecha <- max(datos$fechasdf) 

cat("Creo un data frame con 31 registros posteriores a el ultimo")
dfDiasFuturos <- data.frame(fechasfuturo = seq((max_fecha) + 1, by = "day", length.out = 31))

cat("Se hace la prediccion en base al modelo")
predicciones <- predict(model, newdata = (datos$fechasdf = dfDiasFuturos$fechasfuturo))

cat("Creo un data frame con las fechas futuras y los precios predecidos")
predicciondf <- data.frame(fechasdf = dfDiasFuturos$fechasfuturo, preciosdf = predicciones)

cat("Concatenacion del data frame original mas las predicciones")
copiapredicciondf <- rbind(datosrbind, predicciondf)

cat("Un grafico de area para representar los datos que ya tenia + los datos nuevos")
ggplot(data=copiapredicciondf, aes(x=fechasdf, y=preciosdf)) +
  geom_area(stat = "identity", fill = 4, alpha = 0.5, color = 1, lwd = 0.5, linetype = 1) +
  scale_y_continuous(labels = comma, limits = c(0, 400000), breaks = seq(0, 400000, 25000), expand = c(0, 0)) +
  labs(title = "Prediccion de precios",
       x = "Fechas",
       y = "Precios ") +
  theme_light()

cat("La prediccion parece estar bastante acertada y tener un margen de error no tan grande, pero el analisis se esta haciendo sobre el precio en pesos del producto. \n")
cat("La volatidad de la moneda puede estar cumpliendo un rol importante jugando en contra de la presicion del analisis. \n ")
cat("Hay demasiados factores externos que lo hacen, pero me parecio interesante ver como se desarrollaria el analisis si lo hacia otra vez pero con una moneda mas estable. \n")
cat("Para esto necesitaba el valor de cambio historico de al menos los ultimos 30 dias del peso argentino contra otra moneda como el dolar o el euro \n")
cat("A lo que encontre una API con una prueba gratuita de 1000 requests que tiene los registros que necesito, asi que hice lo siguiente: \n ")


cat("Primero cree una copia de los datos para poder trabajar sin preocuparme de modificar el data frame original")
datosrbindEuros <- datosrbind

cat("Un vector vacio donde voy a ir metiendo el precio de venta oficial del euro en pesos")
rates <- c()

cat("Asigno unas constantes para formar la url de la request. el endpoint de la API, symbols que representa la moneda de cambio que en este caso es ARS frente a la moneda base por defecto EUR")
endpoint <- "http://api.exchangeratesapi.io/v1/"
symbols <- "ARS"

cat("Y la api key vinculada con mi cuenta personal de gmail, que como el proyecto se va a subir a github y va a ser de acceso publico, no la subo")
key <- Sys.getenv("API_KEY")

cat("Como busco exactamente 31 cambios de moneda de 31 dias que conozco, decidi volver a utilizar vectores paralelos, ya que se la cantidad exacta de cambios que busco")
for (i in 1:length(datosrbindEuros$fechasdf)) {
 # Esto es util durante la produccion, lo dejo como comentario por que no lo quiero borrar, pero vas viendo en tiempo real cuantas operaciones quedan, si se detiene un proceso y lo que debuelve el servidor
  cat("\n", (length(datosrbindEuros$fechasdf) - i), "operaciones restantes \n")
  cat(datosrbindEuros$fechasdf[i], " -- ")
  
  date <- datosrbindEuros$fechasdf[i]
  
  # Construir la URL con el endpoint, la clave, la fecha y el símbolo
  url <- paste0(endpoint, date, "?access_key=", key, "&symbols=", symbols)
  # > url    http://api.exchangeratesapi.io/v1/2023-09-21?access_key=MI_KEY_SECRETA&symbols=ARS 
  
  # R es fundamentalmente un lenguaje de programacion sincrono, por lo q cuando se realiza una solicitud HTTP el hilo de ejecucion se bloquea y espera a la respuesta por parte del servidor
  # Hace una petición GET a la URL y obtiene el contenido como una lista
  response <- GET(url) 

  # Agarro la respuesta y la combierto en contenido legible
  content <- content(response, as = "parsed")
  
  # Acceder al valor de ARS dentro de la lista y guardarlo en la lista de tasas
  rate <- content$rates$ARS
  cat(rate)
  # Agrego la tasa de intercambio a rates
  rates <- c(rates, rate)
}


cat("\n Ahora necesito remplazar los valores en pesos por los valores en euros")  
for (i in 1: length(datosrbindEuros$preciosdf)) {
  # Como son vectores paralelos, recupero el precio promedio que tubo ese dia el producto y el precio del euro oficial ese dia
  precios_pesos <- datosrbindEuros$preciosdf[i]
  tasa_cambio <- rates[i]
  
  # Calculo cual fue el precio de ese dia dividiendo la cantidad de pesos / valor por euro del dia (redondeado a 2 decimales)  
  precio_en_euros <- round(precios_pesos / tasa_cambio, 2)
  
  # Remplazo el precio en pesos por el precio en euros
  datosrbindEuros$preciosdf[i] <- precio_en_euros
}


cat("Ahora puedo ver cual es la correlacion si los precios estan en euros! antes tengo que pasar las fechas ")
# como se que son las mismas fechas de antes, puedo usar el fechas_numeric de antes
print(cor.test(datosrbindEuros$preciosdf, fechas_numeric))

cat("Grafico los datos nuevos")
ggplot(data=datosrbindEuros, aes(x=fechasdf, y=preciosdf)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(size = 1) + 
  labs(title = "Regresion lineal datos actualidad",
       x = "Fechas",
       y = "Precios") +
  theme_light()


cat("Ahora tendria que hacer una prediccion con los precios 'nuevos', comvertidos a euros en realidad")
modeleuros <- lm((preciosdf = datosrbindEuros$preciosdf) ~ (fechasdf = datosrbindEuros$fechasdf), data = datosrbindEuros)
print(summary(modeleuros))

cat("Creo una copia para usarla en el grafico")
dfEurosGrafico <- datosrbindEuros

cat("Hago las nuevas predicciones")
prediccioneseuro <- predict(modeleuros, newdata = (datosrbindEuros$fechasdf = dfDiasFuturos$fechasfuturo))

cat("Creo un data frame con las fechas futuras y los precios predecidos ")
predicciondfeuro <- data.frame(fechasdf = dfDiasFuturos$fechasfuturo, preciosdf = prediccioneseuro)

cat("Hago un rbind de los datos actuales y las predicciones")
predicciondfeuro_rbind <- rbind(dfEurosGrafico, predicciondfeuro)

cat("Otro grafico de area pero para representar los precios ahora en euros!!!")
ggplot(data=copiapredicciondf, aes(x=fechasdf, y=preciosdf)) +
  geom_area(stat = "identity", fill = 4, alpha = 0.5, color = 1, lwd = 0.5, linetype = 1) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 100)) +
    labs(title = "Prediccion de precios",
       x = "Fechas",
       y = "Precios ") +
  theme_light()


rm(list=ls())




