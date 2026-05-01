# -----------------------------------------------------------------------------
# PROJECT: University INNOVA XXII - Student Satisfaction Study
# ASSIGNMENT 2: Descriptive Measures & Data Visualization
# 
# INSTRUCTIONS:
# 1. Descriptive Measures:
#    a. Calculate Central Tendency, Position, and Dispersion for study hours.
#    b. For Satisfaction level: Mode, Median, and Quartiles only
#
# 2. Graphical Representation:
#    a. Histogram for "Weekly Study Hours".
#    b. Pie Chart showing percentages for "Academic Satisfaction".
#
# TECHNICAL CONSTRAINTS:
# - Symmetric rounding with 4 decimal places of precision.
# -----------------------------------------------------------------------------

if (!require(readxl)) install.packages("readxl")
library(readxl)

datos <- read_excel(file.choose())

#==============================================
# Horas semanales dedicadas al estudio
#==============================================

variable_continua <- "TIEMPO_SEMANAL_ESTUDIO_HS"

# valores con sturges

horas <- datos$TIEMPO_SEMANAL_ESTUDIO_HS # guardar datos de columna en variable
n <- length(horas) # cantidad total de datos
k <- 1 + 3.322 * log10(n) # Sturges para intervalos

rango <- range(horas) # maximo y minimo
amplitud <- (rango[2] - rango[1]) / k # amplitud de cada intervalo
separacion <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud) # saltos de intervalo
datos$clases <- cut(horas, breaks = separacion, right = FALSE) # crear intervalos (cerrado a la izquierda, abierto a la derecha)
# se guardan los intervalos en una nueva columna a la tabla original 
# para vincular cada observacion original con sus datos correspondientes

# tabla de datos
marca_clase <- (head(separacion, -1) + tail(separacion, -1)) / 2 # Punto medio de cada intervalo
tabla_clases <- table(datos$clases)
frecuencias <- as.vector(tabla_clases)

f_acum <- cumsum(frecuencias)
f_rel <- prop.table(frecuencias)
f_rel_acum <- cumsum(f_rel)

Tabla_Frecuencias <- data.frame(
  Intervalo = names(tabla_clases),
  Marca_Clase = marca_clase,
  Frec_Abs = frecuencias,
  Frec_Acum = f_acum,
  Frec_Rel = round(f_rel, 4),
  Frec_Rel_Acum = round(f_rel_acum, 4)
)

########
# media
########

media_continua <- sum(marca_clase * frecuencias) / sum(frecuencias)

#######
# moda
#######

i_modal <- which.max(frecuencias) # Buscamos la clase con mayor frecuencia
L_m <- separacion[i_modal] # Límite inferior de la clase modal
f_m <- frecuencias[i_modal]
f_1 <- ifelse(i_modal == 1, 0, frecuencias[i_modal - 1]) # frecuencia anterior
f_2 <- ifelse(i_modal == length(frecuencias), 0, frecuencias[i_modal + 1]) # frecuencia posterior

moda_continua <- L_m + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud

##########
# mediana
##########

n_total <- sum(frecuencias)
n_2 <- n_total / 2 # posicion que ocupa la mediana

clase_mediana_index <- which(f_acum >= n_2)[1] # primer intervalo acumulado donde aparece una frecuencia >= n_2

L <- separacion[clase_mediana_index] # limite inferior del intervalo de la mediana
F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index - 1]) #frecuencia acum. anerior
f_mediana <- frecuencias[clase_mediana_index] # frecuencia de la clase de la mediana

mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud

########################
# medidas de dispersion
########################

varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)
desvio_continua <- sqrt(varianza_continua)
coef_var_continua <- (desvio_continua / media_continua) * 100

###################
# Cuartiles y RIC 
###################

# Posiciones
pos_q1 <- n_total * 0.25
pos_q3 <- n_total * 0.75

# --- Cálculo de Q1 ---
i_q1 <- which(f_acum >= pos_q1)[1]
L_q1 <- separacion[i_q1]
F_ant_q1 <- ifelse(i_q1 == 1, 0, f_acum[i_q1 - 1])
f_q1 <- frecuencias[i_q1]

q1_continua <- L_q1 + ((pos_q1 - F_ant_q1) / f_q1) * amplitud

# --- Cálculo de Q3 ---
i_q3 <- which(f_acum >= pos_q3)[1]
L_q3 <- separacion[i_q3]
F_ant_q3 <- ifelse(i_q3 == 1, 0, f_acum[i_q3 - 1])
f_q3 <- frecuencias[i_q3]

q3_continua <- L_q3 + ((pos_q3 - F_ant_q3) / f_q3) * amplitud

# --- Rango Intercuartílico ---
ric_continua <- q3_continua - q1_continua

# tabla de resultados
horas_stats <- data.frame(
  Media = round(media_continua, 4),
  Mediana = round(mediana_continua, 4),
  Moda = round(moda_continua, 4),
  Q1 = round(q1_continua, 4),
  Q3 = round(q3_continua, 4),
  RIC = round(ric_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_Estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

############
# HISTOGRAMA
############

hist(datos$TIEMPO_SEMANAL_ESTUDIO_HS,
     breaks = separacion,
     col = "skyblue",           # color de las barras
     main = "Histograma horas de estudio", # título del gráfico
     xlab = "Horas",                # etiqueta eje x
     ylab = "Frecuencia absoluta", 
     freq = TRUE)  # frecuencia absoluta

#=======================================
# Niveles de satisfacción
#=======================================

variable_discreta <- "SATISF_CON_CARRERA"


print(summary(datos[[variable_discreta]])) # resumen estadistico basico

##########
# mediana
##########

mediana_discreta <- median(datos[[variable_discreta]], na.rm = TRUE)

#######
# moda
#######

if (!require(modeest)) install.packages("modeest") 
library(modeest)
moda_discreta <- mlv(datos[[variable_discreta]], method = "mfv", na.rm = TRUE)

# tabla de resultados
nivel_stats <- data.frame(
  Mediana = round(mediana_discreta, 4),
  Moda = paste(moda_discreta, collapse = ", ") # se trasforma a texto (paste) por si hay mas de un valor para que se muestren todos
)

# Cuartiles y Rango Intercuartil (RIC)
cuartiles <- quantile(datos[[variable_discreta]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
rango_intercuartil <- IQR(datos[[variable_discreta]], na.rm = TRUE)

##################
# GRÁFICO CIRCULAR
##################

niveles_factor <- factor(datos$SATISF_CON_CARRERA, 
                         levels = c(1, 2, 3, 4), 
                         labels = c("Muy insatisfecho", "Insatisfecho", "Satisfecho", "Muy satisfecho"))

f_absoluta <- table(niveles_factor)

# Calcular porcentajes redondeados
porcentajes <- round(prop.table(f_absoluta) * 100, 2)

# Crear las etiquetas uniendo el nombre y el porcentaje
etiquetas <- paste(names(f_absoluta), "-", porcentajes, "%")

# Graficar
pie(f_absoluta, 
    labels = etiquetas, 
    col = rainbow(4), 
    main = "Nivel de satisfacción")

