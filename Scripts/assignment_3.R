# -----------------------------------------------------------------------------
# PROJECT: University INNOVA XXII - Student Satisfaction Study
# ASSIGNMENT 3: Probability Models (Binomial, Poisson, and Normal)


if (!require(readxl)) install.packages("readxl")
library(readxl)

datos <- read_excel(file.choose())

# ===============
# BINOMIAL MODEL
# ===============
n_estudiantes <- 16

# calculamos las probabilidades de éxito (P) para cada categoría de satisfacción
N_total <- nrow(datos)

P_muy_satisfecho   <- sum(datos$SATISF_CON_CARRERA == 1) / N_total
P_satisfecho       <- sum(datos$SATISF_CON_CARRERA == 2) / N_total
P_insatisfecho     <- sum(datos$SATISF_CON_CARRERA == 3) / N_total
P_muy_insatisfecho <- sum(datos$SATISF_CON_CARRERA == 4) / N_total

# a. P(X > 9) = 1 - P(X <= 9)
prob_5a <- 1 - pbinom(9, size = n_estudiantes, prob = P_muy_satisfecho)
cat("5a) Probabilidad (Más de 9 muy satisfechos):", prob_5a, "\n")

# b. P(4 <= X <= 8) = P(X <= 8) - P(X <= 3)
prob_5b <- pbinom(8, size = n_estudiantes, prob = P_satisfecho) - pbinom(3, size = n_estudiantes, prob = P_satisfecho)
cat("5b) Probabilidad (Entre 4 y 8 satisfechos):", prob_5b, "\n")

# c. P(X < 5) = P(X <= 4)
prob_5c <- pbinom(4, size = n_estudiantes, prob = P_insatisfecho)
cat("5c) Probabilidad (Menos de 5 insatisfechos):", prob_5c, "\n")

# d. P(X = 10) 
prob_5d <- dbinom(10, size = n_estudiantes, prob = P_muy_insatisfecho)
cat("5d) Probabilidad (Exactamente 10 muy insatisfechos):", prob_5d, "\n\n")

# ==============
# POISSON MODEL
# ==============
# a. P(X >= 6) = 1 - P(X <= 5)
# lambda para 20 minutos: (15 / 30) * 20 = 10
lambda_a <- 10

prob_6a <- 1 - ppois(5, lambda = lambda_a)
cat("6a) Probabilidad (Por lo menos 6 en 20 min):", prob_6a, "\n")

# b. P(X <= 12)
# lambda para 40 minutos: (15 / 30) * 40 = 20
lambda_b <- 20

prob_6b <- ppois(12, lambda = lambda_b)
cat("6b) Probabilidad (A lo sumo 12 en 40 min):", prob_6b, "\n")

# c. P(7 < X < 10) = P(X <= 9) - P(X <= 7)
# lambda para 30 minutos: (15 / 30) * 30 = 15
lambda_c <- 15

prob_6c <- ppois(9, lambda = lambda_c) - ppois(7, lambda = lambda_c)
cat("6c) Probabilidad (Más de 7 y menos de 10 en 30 min):", prob_6c, "\n\n")

# ===========================
# NORMAL DISTRIBUTIOIN MODEL
# ===========================
media_estatura <- mean(datos$ESTATURA_CM.)
desv_estatura  <- sd(datos$ESTATURA_CM.)

x_curva <- seq(media_estatura - 4*desv_estatura, media_estatura + 4*desv_estatura, length = 200)
y_curva <- dnorm(x_curva, mean = media_estatura, sd = desv_estatura)

# a. P(X >= 179) = 1 - P(X < 179)
prob_7a <- 1 - pnorm(179, mean = media_estatura, sd = desv_estatura)
cat("7a) Probabilidad (Estatura >= 179 cm):", prob_7a, "\n")

# Gráfico 
plot(x_curva, y_curva, type = 'l', lwd = 2, col = 'black', 
     main = paste0('7a) Probabilidad Estatura >= 179 cm: ', round(prob_7a, 4)), 
     xlab = 'Estatura (cm)', ylab = 'Densidad')

# definir el área a sombrear desde 179 en adelante (hacia la derecha)
x_sombreado_a <- seq(179, media_estatura + 4*desv_estatura, length = 100)
y_sombreado_a <- dnorm(x_sombreado_a, mean = media_estatura, sd = desv_estatura)
polygon(c(179, x_sombreado_a, media_estatura + 4*desv_estatura), c(0, y_sombreado_a, 0), col = '#FF7F00', border = NA)

# b. P(147 <= X <= 172) = P(X < 172) - P(X < 147)
prob_7b <- pnorm(172, mean = media_estatura, sd = desv_estatura) - pnorm(147, mean = media_estatura, sd = desv_estatura)
cat("7b) Probabilidad (Estatura entre 147 y 172 cm):", prob_7b, "\n")

# Gráfico 
plot(x_curva, y_curva, type = 'l', lwd = 2, col = 'black', 
     main = paste0('7b) Probabilidad entre 147 y 172 cm: ', round(prob_7b, 4)), 
     xlab = 'Estatura (cm)', ylab = 'Densidad')

# definir el área a sombrear acotada entre ambos límites (centro)
x_sombreado_b <- seq(147, 172, length = 100)
y_sombreado_b <- dnorm(x_sombreado_b, mean = media_estatura, sd = desv_estatura)
polygon(c(147, x_sombreado_b, 172), c(0, y_sombreado_b, 0), col = '#b8860b', border = NA)

# c. Percentil 97.5
valor_7c <- qnorm(0.975, mean = media_estatura, sd = desv_estatura)
cat("7c) El valor que excede al 97,5% de las estaturas es:", valor_7c, "cm\n")

