###############################################################
# CARGAR LIBRERÍAS
###############################################################

library(readr)
library(openxlsx)
library(MASS)

###############################################################
# IMPORTAR LA BASE DE DATOS
###############################################################

datos <- read_csv(
  "C:/Users/New User/Downloads/dft-road-casualty-statistics-collision-2024.csv"
)

###############################################################
# SELECCIONAR LAS PRIMERAS 500 OBSERVACIONES
###############################################################

datos_modelo <- datos[1:500, ]

###############################################################
# VERIFICAR EL TAMAÑO DE LA BASE
###############################################################

dim(datos_modelo)

###############################################################
# NOMBRES DE LAS VARIABLES
###############################################################

names(datos_modelo)

###############################################################
# VALORES PERDIDOS
###############################################################

colSums(is.na(datos_modelo))

round(colMeans(is.na(datos_modelo))*100,2)

###############################################################
# ELIMINAR REGISTROS DUPLICADOS
###############################################################

sum(duplicated(datos_modelo))

datos_modelo <- datos_modelo[!duplicated(datos_modelo), ]

###############################################################
# SELECCIONAR LAS VARIABLES DEL ESTUDIO
###############################################################

datos_modelo <- datos_modelo[, c(
  
  "number_of_casualties",
  "speed_limit",
  "weather_conditions",
  "light_conditions",
  "urban_or_rural_area"
  
)]

###############################################################
# ESTRUCTURA DE LOS DATOS
###############################################################

str(datos_modelo)

###############################################################
# ELIMINAR VALORES FALTANTES
###############################################################

datos_modelo <- na.omit(datos_modelo)

sum(is.na(datos_modelo))

###############################################################
# ELIMINAR VELOCIDADES NO VÁLIDAS
###############################################################

datos_modelo <- subset(
  datos_modelo,
  speed_limit > 0
)

###############################################################
# CONVERTIR VARIABLES CATEGÓRICAS EN FACTORES
###############################################################

datos_modelo$weather_conditions <-
  factor(datos_modelo$weather_conditions)

datos_modelo$light_conditions <-
  factor(datos_modelo$light_conditions)

datos_modelo$urban_or_rural_area <-
  factor(datos_modelo$urban_or_rural_area)

###############################################################
# VERIFICAR LA ESTRUCTURA
###############################################################

str(datos_modelo)

###############################################################
# ESTADÍSTICA DESCRIPTIVA
###############################################################

summary(datos_modelo$number_of_casualties)

mean(datos_modelo$number_of_casualties)

var(datos_modelo$number_of_casualties)

###############################################################
# HISTOGRAMA
###############################################################

hist(
  datos_modelo$number_of_casualties,
  main="Número de víctimas",
  xlab="Número de víctimas"
)

###############################################################
# TABLAS DE FRECUENCIA
###############################################################

table(datos_modelo$speed_limit)

table(datos_modelo$weather_conditions)

table(datos_modelo$light_conditions)

table(datos_modelo$urban_or_rural_area)

###############################################################
# EXPORTAR BASE LIMPIA
###############################################################

write.csv(
  datos_modelo,
  "C:/Users/New User/OneDrive/Documents/datos_modelo_limpio.csv",
  row.names = FALSE
)

###############################################################
# EXPORTAR A EXCEL
###############################################################

write.xlsx(
  datos_modelo,
  "C:/Users/New User/OneDrive/Documents/datos_modelo_limpio.xlsx"
)

###############################################################
# AJUSTE DEL MODELO POISSON
###############################################################

modelo_poisson <- glm(
  
  number_of_casualties ~
    
    speed_limit +
    
    weather_conditions +
    
    light_conditions +
    
    urban_or_rural_area,
  
  family = poisson(link = "log"),
  
  data = datos_modelo
  
)

###############################################################
# RESULTADOS DEL MODELO POISSON
###############################################################

summary(modelo_poisson)

coef(summary(modelo_poisson))

AIC(modelo_poisson)

###############################################################
# AJUSTE DEL MODELO BINOMIAL NEGATIVA
###############################################################

modelo_bn <- glm.nb(
  
  number_of_casualties ~
    
    speed_limit +
    
    weather_conditions +
    
    light_conditions +
    
    urban_or_rural_area,
  
  data = datos_modelo
  
)

###############################################################
# RESULTADOS DEL MODELO BINOMIAL NEGATIVA
###############################################################

summary(modelo_bn)

coef(summary(modelo_bn))

AIC(modelo_bn)

###############################################################
# COMPARACIÓN DE MODELOS
###############################################################

data.frame(
  
  Modelo = c("Poisson","Binomial Negativa"),
  
  AIC = c(
    
    AIC(modelo_poisson),
    
    AIC(modelo_bn)
    
  )
  
)

