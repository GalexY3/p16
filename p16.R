# =============================================================
# PRÁCTICA 16 - Regresión Lineal Múltiple
# Data Science - Mayo 2026
# Dataset: PISA22SP.sav
# =============================================================

# --- 1. CARGA DE LIBRERÍAS Y DATOS ---------------------------

library(haven)

pisa <- read_sav("PISA22SP.sav")
pisa <- na.omit(pisa)  # Eliminar filas con NA


# --- 2. EXPLORACIÓN INICIAL ----------------------------------

# Vistazo a las variables que usaremos
summary(pisa[, c("mates", "lectu", "heduc", "genero")])


# --- 3. PREPARACIÓN DE VARIABLES -----------------------------

# Convertir género a factor con etiquetas legibles
pisa$genero <- as.factor(pisa$genero)
levels(pisa$genero) <- c("mujer", "hombre")

# Comprobar distribución de la variable dependiente
hist(pisa$mates, main = "Distribución de notas en matemáticas",
     xlab = "Puntuación", col = "steelblue")


# --- 4. REGRESIÓN LINEAL SIMPLE (punto de partida) -----------

# Primero estudiamos la relación entre lectura y matemáticas
plot(pisa$lectu, pisa$mates,
     main = "Matemáticas vs Lectura",
     xlab = "Puntuación lectura", ylab = "Puntuación matemáticas",
     col = rgb(0, 0, 1, 0.1), pch = 16)

reg_simple <- lm(mates ~ lectu, data = pisa)
abline(reg_simple, col = "red", lwd = 2)
summary(reg_simple)

# Interpretación:
# El coeficiente de lectu (~0.66) indica que por cada punto más en lectura,
# la nota en matemáticas sube ~0.66 puntos.
# R² ~ 0.56: la lectura explica el 56% de la varianza en matemáticas.


# --- 5. REGRESIÓN LINEAL MÚLTIPLE ----------------------------

# Añadimos género y nivel educativo de los padres (heduc) como predictores
reg_multiple <- lm(mates ~ genero + heduc + lectu, data = pisa)
summary(reg_multiple)

# Interpretación de los coeficientes:
# - genero (hombre): los hombres sacan ~30 puntos más que las mujeres en mates,
#   controlando por el resto de variables.
# - heduc: por cada nivel más de educación de los padres, la nota sube ~3 puntos.
# - lectu: sigue siendo el predictor más fuerte (~0.67 por punto de lectura).
# R² ~ 0.60: el modelo explica el 60% de la varianza (mejora respecto al simple).


# --- 6. EVALUACIÓN DEL MODELO --------------------------------

# Error típico de los residuos (RSE)
RSE <- sqrt(sum(residuals(reg_multiple)^2) / reg_multiple$df.residual)
cat("RSE:", round(RSE, 2), "\n")

# Coeficiente de determinación R²
R2 <- summary(reg_multiple)$r.squared
cat("R²:", round(R2, 4), "\n")

# R² ajustado (penaliza por número de predictores)
R2_adj <- summary(reg_multiple)$adj.r.squared
cat("R² ajustado:", round(R2_adj, 4), "\n")


# --- 7. DIAGNÓSTICO DE RESIDUOS ------------------------------

par(mfrow = c(2, 2))
plot(reg_multiple)
par(mfrow = c(1, 1))

# Gráfico 1 (Residuals vs Fitted): comprueba linealidad y homocedasticidad.
# Gráfico 2 (Q-Q): comprueba normalidad de los residuos.
# Gráfico 3 (Scale-Location): otra forma de ver la homocedasticidad.
# Gráfico 4 (Residuals vs Leverage): detecta puntos influyentes.


# --- 8. COMPROBACIÓN DE MULTICOLINEALIDAD --------------------

# VIF > 10 indica multicolinealidad problemática
if (!require(car)) install.packages("car")
library(car)

vif(reg_multiple)

# Si VIF < 5 en todos los predictores, no hay problema de multicolinealidad.


# --- 9. CONCLUSIONES -----------------------------------------

# El modelo de regresión múltiple con género, educación de los padres
# y puntuación en lectura explica aproximadamente el 60% de la varianza
# en las notas de matemáticas (R² ~ 0.60).
#
# El predictor más potente es la puntuación en lectura, seguido del género
# (los chicos obtienen ~30 puntos más) y el nivel educativo familiar.
# El RSE indica que las predicciones se desvían ~52 puntos de media,
# lo cual es razonable dada la escala PISA (0-1000).
