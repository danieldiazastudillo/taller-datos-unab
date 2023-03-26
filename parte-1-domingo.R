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
data_ej1<-laptop
data_ej1$price_bin<-ifelse(data_ej1$price> 80000,1,0)
data_ej1$price_bin<-factor(data_ej1$price_bin)


set.seed(16)
training.samples <- data_ej1$price_bin %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data_ej1[training.samples, ]
test.data <- data_ej1[-training.samples, ]



#Dividir en datos de entrenamiento y prueba
funModeling::freq(data_ej1$price_bin)
funModeling::freq(train.data$price_bin)
funModeling::freq(test.data$price_bin)


#Modelo
tree <- rpart(price_bin ~os+os_bit+graphic_card_gb+weight+warranty+Touchscreen+msoffice, data = train.data, 
              method = "class",cp=0.001)
rpart.plot(tree,fallen.leaves = FALSE)
table(pred=predict(tree, type = "class"),obs=train.data$price_bin)

##identificar el mejor cp para disminuir el error
printcp(tree)
plotcp(tree)
tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)
rpart.plot(tree.pruned, extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE,cex=0.6)
table(pred=predict(tree.pruned, type = "class"),obs=train.data$price_bin)


#Importancia de las variables
library(tibble)
library(forcats)
tree.pruned$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia")


#Predecir#cp=0.001  (accuracy)
pred.diab1<-predict(tree, test.data,type = "class") 
confusionMatrix(pred.diab1,test.data$price_bin)
plot(test.data$price_bin, pred.diab1, 
     main = " ",
     xlab = "Obs",
     ylab = "Pred")


#modelo para el mejor cp
pred.diab2<-predict(tree.pruned, test.data,type = "class") 
confusionMatrix(pred.diab2,test.data$price_bin)
plot(test.data$price_bin, pred.diab2, 
     main = " ",
     xlab = "Obs",
     ylab = "Pred")


#Curva ROC
preds_diab <- bind_cols(
  predict(tree.pruned, newdata = test.data, type = "prob"),
  predicted = predict(tree.pruned, newdata = test.data, type = "class"),
  actual = test.data$price_bin
)

mdl_auc <- auc(actual = test.data$price_bin == "neg", preds_diab$neg)
roc_curve(preds_diab, actual, neg) %>%
  autoplot() +
  labs(
    title = "ROC Curve",
    subtitle = paste0("AUC = ", round(mdl_auc, 4))
    
    