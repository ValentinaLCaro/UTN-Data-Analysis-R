# -----------------------------------------------------------------------------
# PROJECT: University INNOVA XXII - Student Satisfaction Study
# ASSIGNMENT 1: Frequency Analysis
# 
# INSTRUCTIONS:
# Frequency Tables:
#    a. Weekly study hours (Determine optimal interval count).
#    b. Academic satisfaction levels (Qualitative Ordinal)
#
# TECHNICAL CONSTRAINTS:
# - Symmetric rounding with 4 decimal places of precision.
# -----------------------------------------------------------------------------

# importar libreria para importar excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

#elegir y leer excel
archivo <- file.choose()
datos <- read_excel(archivo)

##########
# Horas semanales dedicadas al estudio
##########

# definir variables

horas <- datos$TIEMPO_SEMANAL_ESTUDIO_HS # guardar datos de columna en variable
n <- length(horas) # cantidad total de datos
k <- 1 + 3.322 * log10(n) # Sturges para intervalos

rango <- range(horas) # maximo y minimo
amplitud <- (rango[2] - rango[1]) / k # amplitud de cada intervalo
separacion <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud) # saltos de intervalo
clases <- cut(horas, breaks = separacion, right = FALSE) # crear intervalos (cerrado a la izquierda, abierto a la derecha)

# armar la tabla

tabla_horas <- table(clases)
f_acumulada <- cumsum(tabla_horas)
f_relativa <- prop.table(tabla_horas)
f_relativa_acumulada <- cumsum(f_relativa)

tablaFrecuenciaA <- data.frame(
  "Intervalos" = names(tabla_horas),
  "fi" = as.vector(tabla_horas),
  "Fi" = as.vector(f_acumulada),
  "fr" = round(as.vector(f_relativa), 4),
  "Fr" = round(as.vector(f_relativa_acumulada), 4)
)

##########
# Nivel de satisfaccion con la carrera
##########

# Transformar a factor con etiquetas del caso de estudio
datos$SATISF_CON_CARRERA <- factor(datos$SATISF_CON_CARRERA, 
                                   levels = c(1, 2, 3, 4), 
                                   labels = c("Muy insatisfecho", "Insatisfecho", "Satisfecho", "Muy satisfecho"))

nivel <- datos$SATISF_CON_CARRERA

#armar la tabla 

f_absoluta2 <- table(nivel)
f_absoluta_acumulada <- cumsum(f_absoluta2)
f_relativa2 <- prop.table(f_absoluta2)
f_relativa_acumulada2 <- cumsum(f_relativa2)

tablaFrecuenciaB <- data.frame(
  "Niveles" = names(f_absoluta2),
  "fi" = as.vector(f_absoluta2),
  "Fi" = as.vector(f_absoluta_acumulada),
  "fr" = round(as.vector(f_relativa2), 4),
  "Fr" = round(as.vector(f_relativa_acumulada2), 4)
)
tablaFrecuenciaB

