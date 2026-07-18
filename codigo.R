#Cargar librerías

library(readr)
library(openxlsx)
library(MASS)

#Importar la base de datos

datos <- read_csv(
  "C:/Users/New User/Downloads/dft-road-casualty-statistics-collision-2024.csv"
)

#Selecionar las primeras 500 observaciones

datos_modelo <- datos[1:500, ]

#Verificarel tamaño de la base

dim(datos_modelo)

#Nombre de las variables

names(datos_modelo)

#Valores perdidos

colSums(is.na(datos_modelo))

round(colMeans(is.na(datos_modelo))*100,2)

#Eliminar registros duplicados

sum(duplicated(datos_modelo))

datos_modelo <- datos_modelo[!duplicated(datos_modelo), ]

#Seleccionar variables de estudio

datos_modelo <- datos_modelo[, c(
  
  "number_of_casualties",
  "speed_limit",
  "weather_conditions",
  "light_conditions",
  "urban_or_rural_area"
  
)]

#Estructura de los datos

str(datos_modelo)

#Eliminar valores faltantes

datos_modelo <- na.omit(datos_modelo)

sum(is.na(datos_modelo))

#Eliminar velocidades no válidas

datos_modelo <- subset(
  datos_modelo,
  speed_limit > 0
)

#Convertir valiables categóricas en factores

datos_modelo$weather_conditions <-
  factor(datos_modelo$weather_conditions)

datos_modelo$light_conditions <-
  factor(datos_modelo$light_conditions)

datos_modelo$urban_or_rural_area <-
  factor(datos_modelo$urban_or_rural_area)

#Verificar la estructura
str(datos_modelo)

#Etadística descriptiva

summary(datos_modelo$number_of_casualties)

mean(datos_modelo$number_of_casualties)

var(datos_modelo$number_of_casualties)


#Tablas de Frecuencias

table(datos_modelo$speed_limit)

table(datos_modelo$weather_conditions)

table(datos_modelo$light_conditions)

table(datos_modelo$urban_or_rural_area)

hist(
  datos_modelo$number_of_casualties,
  main="Número de víctimas",
  xlab="Número de víctimas",
  col="lightblue"
)


#Ajuste del modelo Poisson

modelo_poisson <- glm(
  
  number_of_casualties ~
    
    speed_limit +
    
    weather_conditions +
    
    light_conditions +
    
    urban_or_rural_area,
  
  family = poisson(link = "log"),
  
  data = datos_modelo
  
)

summary(modelo_poisson)

#Ajuste del modelo Binomail Negativo

modelo_bn <- glm.nb(
  
  number_of_casualties ~
    
    speed_limit +
    
    weather_conditions +
    
    light_conditions +
    
    urban_or_rural_area,
  
  data = datos_modelo
  
)

summary(modelo_bn)

#Comparación preliminar

coef(modelo_poisson)

coef(modelo_bn)

logLik(modelo_poisson)

logLik(modelo_bn)

deviance(modelo_poisson)

deviance(modelo_bn)

modelo_bn$theta

#Comparación de los modelos según AIC

data.frame(
  
  Modelo = c("Poisson","Binomial Negativa"),
  
  AIC = c(
    
    AIC(modelo_poisson),
    
    AIC(modelo_bn)
    
  )
  
)

#Extraer los valores de AIC de cada modelo
aic_poisson <- AIC(modelo_poisson)
aic_bn      <- AIC(modelo_bn)

# Guardarlos en un vector junto con sus nombres
aics <- c(Poisson = aic_poisson, Binomial_Negativa = aic_bn)

#Aplicar la fórmula: AIC_i - AIC_min
delta_aic <- aics - min(aics)

# Ver el resultado
print(delta_aic)

# Crear una tabla comparativa con los valores originales y el Delta AIC
tabla_aic <- data.frame(
  Modelo = c("Poisson", "Binomial Negativa"),
  AIC = c(AIC(modelo_poisson), AIC(modelo_bn))
)

# Calcular la columna Delta AIC usando la fórmula exacta
tabla_aic$Delta_AIC <- tabla_aic$AIC - min(tabla_aic$AIC)

print(tabla_aic)

