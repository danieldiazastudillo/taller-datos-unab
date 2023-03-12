library(ggplot2)
library(flextable)
library(factoextra)
library(gridExtra) 
library(corrplot)

##leer los datos
library(MASS)
data(Boston)
str(Boston)

##Coeficientes de correlación
#covarianza
cov(Boston$medv,Boston$lstat) 
#correlación de Pearson
cor(Boston$medv,Boston$lstat) 
#Ho:cor=0 H1:cor<>0
cor.test(Boston$medv,Boston$lstat)
#correlación de Spearman automático
cor(Boston$medv,Boston$lstat,method = "spearman")
cor.test(Boston$medv,Boston$lstat,method = "spearman")
##Tau de kendall
cor(Boston$medv,Boston$lstat,method = "kendall")

#matriz de correlación
options(digits = 2)
cor_matrix<-cor(Boston,method = "spearman")
cor_matrix

corrplot(cor_matrix, method="ellipse",type = "upper")

###############
#Regresión lineal múltiple

#dividir datos de entrenamiento y prueba
set.seed(46)
selectrows <- sample(1:nrow(Boston),round(0.80*nrow(Boston)))
dat.train <- Boston[selectrows,]
dat.test <- Boston[-selectrows,]

head(dat.train)
mod1<-lm(medv~.,data = dat.train)
#inferencia de los coeficientes
coefic.lm<-summary(mod1)$coefficients
coefic.lm<-data.frame(Variables=row.names(coefic.lm),round(coefic.lm,4))
colnames(coefic.lm)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm %>% flextable()

#R2 ajustado
r2.mod1<-summary(mod1)$adj.r.squared*100

confint(mod1)#intervalos de confianza
#gráfico Q-Q
plot(mod1,which = 2,col=c("blue"))

dat.train$resid<-mod1$residuals
shapiro.test(dat.train$resid)
dat.train$obs<-1:length(dat.train$resid)
summary(dat.train$resid)

ggplot(dat.train,aes(x=obs,y=resid))+
  geom_point()+
  geom_hline(yintercept = 0,linetype="dashed", color = "red")+
  labs(x = "No. Observaciones", y = "Residuos")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

###Seleccionar el mejor modelo
mod2.lm <- step(mod1, direction="backward",trace = 0) #trace=1 muestra los pasos que realiza.
#inferencia de los coeficientes
coefic.lm2<-summary(mod2.lm)$coefficients
coefic.lm2<-data.frame(Variables=row.names(coefic.lm2),round(coefic.lm2,4))
colnames(coefic.lm2)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm2 %>% flextable()

r2.mod2<-summary(mod2.lm)$adj.r.squared*100

#predicción para ambos modelos con datos prueba
dat.test$pred1<-predict(mod1,dat.test)
cor1<-cor(dat.test$medv,dat.test$pred1)
# error mod
error1 <- dat.test$pred1 - dat.test$medv
#bias
bias1<-mean(error1)
#RMSE
rmse1<-sqrt(mean(error1^2))

plot.pred1<-ggplot(dat.test,aes(x=medv,y=pred1))+
  geom_point()+
  geom_line(aes(x=medv, y=medv),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("Modelo 1")

#evaluar predicción 2
dat.test$pred2<-predict(mod2.lm,dat.test)
cor2<-cor(dat.test$medv,dat.test$pred2)

# error mod
error2 <- dat.test$pred2 - dat.test$medv
#bias
bias2<-mean(error2)
#RMSE
rmse2<-sqrt(mean(error2^2))

#comparación modelos
medidas.lm=data.frame(Medidas=c("R2.ajustado","COR","BIAS","RMSE"),
                      Modelo_1=c(r2.mod1,cor1,bias1,rmse1),
                      Modelo_2=c(r2.mod2,cor2,bias2,rmse2))

medidas.lm %>% flextable()

plot.pred2<-ggplot(dat.test,aes(x=medv,y=pred2))+
  geom_point()+
  geom_line(aes(x=medv, y=medv),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("Modelo 2")

grid.arrange(plot.pred1, plot.pred2, ncol = 2)
