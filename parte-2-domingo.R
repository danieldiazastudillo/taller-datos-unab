# librerías
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(ROCR)
library(Metrics)
library(yardstick)

#data("laptop", package = "mlbench")
laptop <- read.csv("C:/Users/daniel.diaz/Desktop/taller-datos/laptop.csv", sep=";")
dim(laptop)


library(caret)
data_ej2<-laptop
data_ej2$price_bin<-ifelse(data_ej2$price> 80000,1,0)
data_ej2$price_bin<-factor(data_ej2$price_bin)


set.seed(16)
training.samples <- data_ej2$price_bin %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data_ej2[training.samples, ]
test.data <- data_ej2[-training.samples, ]


#Comparar varios algoritmos
control = trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"


#regresión logística
st.time<-Sys.time()
fit.lr <- train(price_bin ~ os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data = train.data, method = "glm", family = binomial, 
                metric=metric,trControl = control,na.action=na.omit)
end.time<-Sys.time()
tim.lr<-end.time-st.time
tim.lr


#árbol de decisión
st.time<-Sys.time()
fit.dt <- train(price_bin~os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data=train.data, method="rpart", metric=metric, trControl=control, na.action=na.omit)
end.time<-Sys.time()
tim.dt<-end.time-st.time
tim.dt


##K-vecinos más cercanos
st.time<-Sys.time()
fit.knn <- train(price_bin~os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data=train.data, method="knn", 
                 preProcess=c("scale","center"),
                 metric=metric, trControl=control,na.action=na.omit)
end.time<-Sys.time()
tim.knn<-end.time-st.time
tim.knn

#tiempo cada algoritmo (agregar de cada estudiantes)
time.all<-data.frame(Algoritmos=c("LR","DT","KNN"),
                     Andres=c("5.357 secs","1.225 secs","2.987 secs"),
                     Walde=c("LR","DT","KNN"),
                     Daniel=c("LR","DT","KNN"))

library(flextable)
time.all%>% flextable()


##gráfico comparativo (el qu esta mas cercano a 1 es el mejor algoritmo)
list_alg<-list(lr=fit.lr,dt=fit.dt, knn=fit.knn)
all_algt <- resamples(list_alg)
summary(all_algt)
dotplot(all_algt,scales="free")


##predicción con test.data
options(digits = 4)
pred.lr <- predict(fit.lr, test.data)
conf.lr<-confusionMatrix(pred.lr, test.data$price_bin)
conf.lr$table

pred.dt <- predict(fit.dt, test.data)
conf.dt<-confusionMatrix(pred.dt, test.data$price_bin)
conf.dt$table

pred.knn <- predict(fit.knn, test.data)
conf.knn<-confusionMatrix(pred.knn, test.data$price_bin)
conf.knn$table

options(digits = 4)
confmat<-data.frame(Medidas=c(names(conf.lr$overall[1]),names(conf.lr$byClass[c(1,2,5,6,7)])),
                    LR=c(conf.lr$overall[1],conf.lr$byClass[c(1,2,5,6,7)]),
                    DT=c(conf.dt$overall[1],conf.dt$byClass[c(1,2,5,6,7)]),
                    KNN=c(conf.knn$overall[1],conf.knn$byClass[c(1,2,5,6,7)]))

confmat%>% flextable()






#####################
## Comparar algoritmos Regresión
#KNN-Regresión
library(FNN)
library(MASS)
#data(Boston)
#str(Boston)
set.seed(46)
selectrows <- sample(1:nrow(data_ej1),round(0.80*nrow(data_ej1)))
dat.train <- data_ej1[selectrows,]
dat.test <- data_ej1[-selectrows,]


# prepare resampling method
control <- trainControl(method="cv", number=10)
set.seed(7)

vlaptop <- read.csv("C:/Users/alare/Downloads/laptop.csv", sep=";")
dim(laptop)

#Regresión Lineal Múltiple
st.time<-Sys.time()
fitR.lm <- train(price~os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data=dat.train, method="lm", metric="RMSE", trControl=control)
end.time<- Sys.time()
tim.lm<-end.time-st.time
tim.lm

#árbol de decisión
st.time<-Sys.time()
fitR.dt <- train(price~os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data=dat.train, method="rpart", metric="RMSE", trControl=control,na.action=na.omit,tuneLength=5)
end.time<-Sys.time()
tim.dt<-end.time-st.time
tim.dt


##K-vecinos más cercanos
st.time<-Sys.time()
fitR.knn <- train(price~os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data=dat.train, method="knn", metric="RMSE",
                  linout=TRUE, preProcess=c("scale","center"), trControl=control,na.action=na.omit)
end.time<-Sys.time()
tim.knn<-end.time-st.time
tim.knn


#tiempo cada algoritmo (agregar de cada estudiantes)
time.all<-data.frame(Algoritmos=c("LM","DT","KNN"),
                     Andres=c(tim.lr,tim.dt,tim.knn),
                     Walde=c(1,2,3),
                     Daniel=c(1,2,3))


library(flextable)
time.all%>% flextable()


##gráfico comparativo
list_reg<-list(lm=fitR.lm,dt=fitR.dt,knn=fitR.knn)
all_reg <- resamples(list_reg)
summary(all_reg)
dotplot(all_reg,scales="free")


##Predicción
med.reg<-function(obs,pred){
  e = obs-pred
  bias = mean(e)
  mse = mean((e)^2)
  mae = mean(abs(e))
  rmse = sqrt(mse)
  R2 = 1-(sum((e)^2)/sum((obs-mean(obs))^2))
  medidas = data.frame(bias,mse,mae,rmse,R2)
  medidas
}

pred.lm<-predict(fitR.lm,dat.test)
val.lm<-med.reg(dat.test$price,pred.lm)

pred.dt<-predict(fitR.dt,dat.test)
val.dt<-med.reg(dat.test$price,pred.dt)

pred.knn<-predict(fitR.knn,dat.test)
val.knn<-med.reg(dat.test$price,pred.knn)

all.medR<-data.frame(Algoritmos=c("LM","DT","KNN"),rbind(val.lm,val.dt,val.knn))
all.medR%>% flextable()


##gráficos
plot.predlm<-ggplot(dat.test,aes(x=price,y=pred.lm))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("LM")

plot.preddt<-ggplot(dat.test,aes(x=price,y=pred.dt))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("DT")

plot.predknn<-ggplot(dat.test,aes(x=price,y=pred.knn))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("KNN")

library(gridExtra) 
grid.arrange(plot.predlm, plot.preddt,plot.predknn, ncol = 2)

##gráfico comparativo
all.predR<-data.frame(nobs=1:length(dat.test$price),obs=dat.test$price,LM=pred.lm,DT=pred.dt,KNN=pred.knn)

ggplot(data=all.predR[50:150,])+
  geom_line(aes(x=nobs,y=LM,color="LM"))+
  geom_line(aes(x=nobs,y=DT,color="DT"))+
  geom_line(aes(x=nobs,y=KNN,color="KNN"))+
  geom_line(aes(x=nobs,y=obs,color="Obs"))+
  labs(x = " ", y = "price",)+
  scale_color_manual(name = " ",values = c(
    'Obs' = 'darkblue',
    'LM' = 'red',
    'DT' = 'pink',
    'KNN' = 'green')) +
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

