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

# read laptop.csv
laptop <- read.csv("C:\\Users\\daniel.diaz\\Desktop\\taller-datos\\laptop.csv", sep = ";", header = TRUE)
dim(laptop)
head(laptop)
str(laptop)

# Obj-1 | Identificar Tipos de Variables
visdat::vis_dat(laptop, sort_type = FALSE)

# Obj-2 | Diagnostico de los Datos
library(dplyr)
diagnose(laptop) %>% flextable()

# análisis de variables categóricas o enteras
funModeling::freq(laptop$processor_brand)

#  Obj-3 | Representa variable categórica (get summary of all numeric variables)
diagnose_numeric(laptop) %>% flextable()

# Decimal para resumen de medidas
(max(laptop$price, na.rm = TRUE) - min(laptop$price,
  na.rm = TRUE
)) / (1 + 3.322 * log10(n))

# Máximo & mínimo
max(laptop$price)
min(laptop$price)


# Obj-5 | Histograma con curva de densidad
ggplot(data = laptop, aes(x = price )) +
  geom_histogram(aes(y = ..density..),
                 breaks = seq(12000, 500000, 58598.69),
                 col = "black",
                 fill = "green",
                 alpha = .4
  ) +
  stat_function(fun = dnorm, args = list(
    mean = mean(laptop$price, na.rm = T),
    sd = sd(laptop$price, na.rm = T)
  ), color = 2, size = 1) +
  labs(x = "Precio", y = "PC") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 11)

# Obj 6 |  Q-Q
ggplot(data = laptop, mapping = aes(sample = price)) +
  stat_qq_line() +
  stat_qq_band() +
  stat_qq_point() +
  labs(x = "Q-Normal", y = "Q-Price (ppb)") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12)

# Obj-7 | Normalidad
# Ho: la distribución es normal
# H1: no tiene distribución normal
ks.test(laptop$price, "pnorm", mean = mean(laptop$price,
                                          na.rm = TRUE
), sd = sd(laptop$price, na.rm = TRUE))
shapiro.test(laptop$price)
normality(laptop) %>% flextable()

#Obj-8 | Atipicos (boxplot, boxplot(2-variables))

#detecta atipicos
diagnose_outlier(laptop) %>% flextable()

# Boxplot Grande
ggplot(laptop, aes(y = price)) +
  geom_boxplot(fill = "lightgreen", varwidth = T) +
  labs(x = "", y = "Precio (CLP$)") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12)

# Boxplot por marca
ggplot(laptop, aes(y = price, x = factor(processor_brand))) +
  geom_boxplot(fill = "lightgreen", varwidth = T) +
  labs(x = "Marca", y = "Precio (CLP$)") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12)


## Obj-9 | Gráfico Datos Faltantes
VIM::aggr(laptop,
          col = c("cyan", "red"),
          numbers = TRUE, sortVars = TRUE,
          labels = names(laptop), cex.axis = 1,
          gap = 3, ylab = c("Perdidos", "Patrón")
)

