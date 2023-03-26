library(ggplot2)
library(flextable)
library(factoextra)
library(gridExtra) 
library(corrplot)

##leer los datos
laptop <- read.csv("C:/Users/daniel.diaz/Desktop/taller-datos/laptop.csv", sep = ";", header = TRUE)
#cast laptop$display_size to numeric
#laptop$display_size <- as.numeric(laptop$display_size)

dim(laptop)
head(laptop)
str(laptop)



## Coeficientes de correlación
## covarianza
## cov(laptop$price,laptop$processor_brand)
## correlación de Pearson
#cor(laptop$price,laptop$display_size)
## Ho:cor=0 H1:cor<>0
#cor.test(laptop$price,laptop$display_size)
## correlación de Spearman automático
#cor(laptop$price,laptop$processor_brand,method = "spearman")
#cor.test(laptop$price,laptop$processor_brand,method = "spearman")
## Tau de kendall
#cor(laptop$price,laptop$processor_brand,method = "kendall")

#matriz de correlación
options(digits = 2)
cor_matrix <- cor(laptop[,c(6,7,10,13)], method = "spearman")
cor_matrix

corrplot(cor_matrix, method = "ellipse", type = "upper")

###############
#Regresión lineal múltiple

#dividir datos de entrenamiento y prueba
set.seed(20)
selectrows <- sample(1:nrow(laptop), round(0.80 * nrow(laptop)))
#selectrows <- sample(seq_len(nrow(laptop)), round(0.80 * nrow(laptop)))
dat_train <- laptop[selectrows, -c(1,2, 3, 4, 9)]
dat_test <- laptop[-selectrows, -c(1,2, 3, 4, 9)]

head(dat_train)
mod1 <- lm(price~., data = dat_train)
#inferencia de los coeficientes
coefic_lm <- summary(mod1)$coefficients
coefic_lm <- data.frame(Variables = row.names(coefic_lm), round(coefic_lm, 4))
colnames(coefic_lm) <- c("Variables", "Estimación", "Std.Error", "t.value", "P-valor")
library(dplyr)
coefic_lm %>% flextable()

#R2 ajustado
r2_mod1 <- summary(mod1)$adj.r.squared * 100

confint(mod1) #intervalos de confianza
#gráfico Q-Q
plot(mod1, which = 2, col = c("blue"))

dat_train$resid <- mod1$residuals
shapiro.test(dat_train$resid)
dat_train$obs <- 1:length(dat_train$resid)
summary(dat_train$resid)

ggplot(dat_train, aes(x = obs, y = resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = 'red') +
    labs(x = "Observaciones", y = "Residuos") +
    theme(text = element_text(size = 12)) +
    theme_gray(base_size = 12)


## Seleccionar el mejor modelo
mod2_lm <- step(mod1, direction = "backward", trace = 0)
coefic_lm2 <- summary(mod2_lm)$coefficients
coefic_lm2 <- data.frame(Variables = row.names(coefic_lm2), round(coefic_lm2, 4))
colnames(coefic_lm2) <- c("Variables", "Estimación", "Std.Error", "t.value", "P-valor")
coefic_lm2 %>% flextable()

r2_mod2 <- summary(mod2_lm)$adj.r.squared * 100

#predicción para ambos modelos con datos de prueba
dat_test$pred1 <- predict(mod1, dat_test)
cor1 <- cor(dat_test$price, dat_test$pred1)
#error modelo
error1 <- dat_test$pred1 - dat_test$price
#bias
bias1 <- mean(error1)
#RMSE
rmse1 <- sqrt(mean(error1^2))

plot.pred1 <- ggplot(dat_test, aes(x = price, y = pred1))+
  geom_point()+
  geom_line(aes(x= price, y = price), linetype = "dashed", col = 2) +
  labs(x = "Observaciones", y = "Predicciones") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12) + ggtitle("Modelo 1")

#evaluar predicción 2
dat_test$pred2 <- predict(mod2_lm, dat_test)
cor2 <- cor(dat_test$price, dat_test$pred2)

#error mod
error2 <- dat_test$pred2 - dat_test$price
#bias
bias2 <- mean(error2)
#RMSE
rmse2 <- sqrt(mean(error2^2))

medidas_lm <- data.frame(Medidas = c("R2.ajustado", "COR", "BIAS", "RMSE"),
                        Modelo_1 = c(r2_mod1, cor1, bias1, rmse1),
                        Modelo_2 = c(r2_mod2, cor2, bias2, rmse2))

medidas_lm %>% flextable()

plot.pred2 <- ggplot(dat_test, aes(x = price, y = pred2))+
  geom_point()+
  geom_line(aes(x= price, y = price), linetype = "dashed", col = 2) +
  labs(x = "Observaciones", y = "Predicciones") +
  theme(text = element_text(size = 12)) +
  theme_grey(base_size = 12) + ggtitle("Modelo 2")

grid.arrange(plot.pred1, plot.pred2, ncol = 2)
