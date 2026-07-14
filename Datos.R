
library(readr)
datos <- read_csv("C:/Users/New User/Downloads/dft-road-casualty-statistics-collision-2024.csv")
datos_modelo <- datos[-c(501:100927),]
dim(datos) #verifico datos

# descargar datos
write.csv(datos_modelo,file="datos_modelo.csv")

names(datos) #verifico nombres de variables

colSums(is.na(datos)) #Número de valores faltantes por variable

round(colMeans(is.na(datos))*100,2) #Porcentaje de valores faltantes

sum(duplicated(datos)) 

datos <- datos[!duplicated(datos), ]

#Selecciono las variables que necesito
datos_modelo1 <- datos_modelo[,c(
  "number_of_casualties",
  "speed_limit",
  "weather_conditions",
  "light_conditions",
  "urban_or_rural_area"
)]

str(datos_modelo)

datos_modelo <- na.omit(datos_modelo)

sum(is.na(datos_modelo))

# descarga de datos de variables seleccionada 
write.csv(
  datos_modelo1,
  "datos_modelo_accidentes.csv",
  row.names = FALSE
)

#exportacion de datos a xls

library(openxlsx)

write.xlsx(
  datos_modelo1,
  "C:/Users/New User/OneDrive/Documents/dato_filtrado_4v.xlsx"
)

summary(datos_modelo$number_of_casualties) #Observo su distribución

hist(datos_modelo$number_of_casualties,
     main="Número de víctimas",
     xlab="Víctimas")

table(datos_modelo$speed_limit)

datos_modelo <- subset(datos_modelo,
                       speed_limit > 0)

table(datos_modelo$weather_conditions) #clima

table(datos_modelo$light_conditions) #iluminaciónt

table(datos_modelo$urban_or_rural_area) #zona

datos_modelo$weather_conditions <-
  factor(datos_modelo$weather_conditions)

datos_modelo$light_conditions <-
  factor(datos_modelo$light_conditions)

datos_modelo$urban_or_rural_area <-
  factor(datos_modelo$urban_or_rural_area)

str(datos_modelo)

mean(datos_modelo$number_of_casualties)

var(datos_modelo$number_of_casualties)

write.csv(datos_modelo,
          "datos_limpios.csv",
          row.names = FALSE)

##########################################

####AJUSTE DEL MODELO POISSON 
#convierte variables categorica a factores]
datos_modelo$weather_conditions <- as.factor(datos_modelo$weather_conditions) #convierte los datos de string a categoricos

datos_modelo$light_conditions <- as.factor(datos_modelo$light_conditions)  #convierte los datos de string a categoricos 

datos_modelo$urban_or_rural_area <- as.factor(datos_modelo$urban_or_rural_area)  #convierte los datos de string a categoricos 


modelo_poisson <- glm(
  number_of_casualties ~
    speed_limit +
    weather_conditions +
    light_conditions +
    urban_or_rural_area,
  family = poisson(link = "log"),
  data = datos_modelo
)

# Resumen del modelo
coef(summary(modelo_poisson))

# Instalar el paquete si es necesario
install.packages("MASS")

library(MASS)

# Ajustar el modelo Binomial Negativa
modelo_bn <- glm.nb(
  number_of_casualties ~
    speed_limit +
    weather_conditions +
    light_conditions +
    urban_or_rural_area,
  data = datos_modelo
)

# Resumen del modelo
summary(modelo_bn)



