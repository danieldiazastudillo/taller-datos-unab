####
library(tidyverse)
library(visdat) # tipos de variables
library(dlookr) # resumen medidas estadísticas
library(funModeling)
library(flextable)
library(inspectdf)
library(qqplotr)
library(ggpmisc)
library(VIM) # datos faltantes
library(mice) # imputación datos faltantes

####
# leer los datos
airq_dt <- read.csv("airq_dt.csv", sep = ",", header = TRUE)
## dimensión del conjunto de datos
dim(airq_dt)
## imprimir las primeras filas
head(airq_dt)
# estructura de los datos
str(airq_dt)

visdat::vis_dat(airq_dt, sort_type = FALSE)
library(dplyr)
diagnose(airq_dt) %>% flextable()

# análisis de variables categóricas o enteras
funModeling::freq(airq_dt$Mes)

## analisis de variables numericas
summary(airq_dt$Ozono)

c.skew <- function(x) {
  m3 <- mean((x - mean(x, na.rm = T))^3, na.rm = T)
  skew <- m3 / (sd(x, na.rm = T)^3)
  skew
}
c.skew(airq_dt$Ozono)
# coeficiente de variación
CV <- function(x) {
  cvar <- sd(x, na.rm = T) / mean(x, na.rm = T)
  cvar
}
CV(airq_dt$Ozono)

diagnose_numeric(airq_dt) %>% flextable()

data2 <- na.omit(airq_dt)
## Tabla de frecuencias
n <- length(data2$Ozono)
n
(max(data2$Ozono, na.rm = T) - min(data2$Ozono,
  na.rm = T
)) / (1 + 3.322 * log10(n))
data2$Ozono.cut <- cut(data2$Ozono, breaks = seq(0, 200, 22.86))
tb.freq <- function(x) {
  f_i <- as.vector(table(x)) # freq absoluta
  F_i <- cumsum(f_i) # freq acumulada
  h_i <- f_i / length(x) # freq relativa
  H_i <- F_i / length(x) # freq relativa acumulada
  tf <- cbind(f_i, F_i, h_i, H_i)
  row.names(tf) <- names(table(x))
  tf
}

tb.freq(data2$Ozono.cut)
diagnose_category(data2, Ozono.cut) %>% flextable()

# histograma con curva de densidad
ggplot(data = data2, aes(x = Ozono)) +
  geom_histogram(aes(y = ..density..),
    breaks = seq(0, 200, 22.86),
    col = "black",
    fill = "green",
    alpha = .5
  ) +
  stat_function(fun = dnorm, args = list(
    mean = mean(airq_dt$Ozono, na.rm = T),
    sd = sd(airq_dt$Ozono, na.rm = T)
  ), color = 2, size = 1) +
  labs(x = "Ozono", y = "Densidad") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 11)

## Distribución empírica acumulada
ggplot(data = data2, aes(x = Ozono)) +
  stat_ecdf(geom = "step") +
  labs(x = "Ozono", y = "F(X)") +
  theme(text = element_text(size = 11)) +
  theme_grey(base_size = 12)

## Q-Q plot
ggplot(data = data2, mapping = aes(sample = Ozono)) +
  stat_qq_line() +
  stat_qq_band() +
  stat_qq_point() +
  labs(x = "Q-Normal", y = "Q-Ozono (ppb)") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12)

## Prueba de normalidad
# Ho: la distribución es normal
# H1: no tiene distribución normal
ks.test(data2$Ozono, "pnorm", mean = mean(data2$Ozono,
  na.rm = T
), sd = sd(data2$Ozono, na.rm = T))
shapiro.test(data2$Ozono)
normality(data2) %>% flextable()

## Detección de valores atípicos
# LS=Q3+1.5*IQR
# LI=Q1-1.5*IQR
diagnose_outlier(data2) %>% flextable()

# Boxplot
ggplot(data2, aes(y = Ozono)) +
  geom_boxplot(fill = "lightgreen", varwidth = T) +
  labs(x = "", y = "Oz (ppb)") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12)

# Boxplot por meses
ggplot(data2, aes(y = Ozono, x = factor(Mes))) +
  geom_boxplot(fill = "lightgreen", varwidth = T) +
  labs(x = "Mes", y = "Oz (ppb)") +
  theme(text = element_text(size = 14)) +
  theme_grey(base_size = 16)

# Análisis e imputación de datos faltantes
VIM::aggr(airq_dt,
  col = c("navyblue", "red"),
  numbers = TRUE, sortVars = TRUE,
  labels = names(airq_dt), cex.axis = 1,
  gap = 3, ylab = c("Perdidos", "Patrón")
)

# imputar valores perdidos con función de R
imp_dt <- mice(airq_dt[, 1:6], m = 1, seed = 46)

imp_dt$imp$Ozono
# data con valores imputados
completedData <- complete(imp_dt, 1)
diagnose(completedData) %>% flextable()
