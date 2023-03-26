#####K-means####

##Ejemplo 1: Esperanza de vida y mortalidad infantil
library(ks)
data("unicef")
dim(unicef)
summary(unicef)
boxplot(unicef)
row.names(unicef)

#Preparar los datos
dej1<- scale(unicef)
summary(dej1)
sum(is.na(dej1))
boxplot(dej1)

#Determinar número óptimo de k
library(factoextra)
# Elbow method 
fviz_nbclust(dej1, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette
fviz_nbclust(dej1, kmeans, method = "silhouette")

### Gap statistic
fviz_nbclust(dej1, kmeans, method = "gap_stat")

#ajustar modelo
fit1<-kmeans(dej1, 2) #para 2 grupos
table(fit1$cluster)
funModeling::freq(fit1$cluster)

unicef$clus<-as.vector(fit1$cluster)

fviz_cluster(fit1, data = unicef,geom = c("point"))

library(ggplot2)
#Boxplot
ggplot(unicef, aes(y=`Under-5`,x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "", y = "Under-5")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

ggplot(unicef, aes(y=`Ave life exp`,x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "", y = "Ave life exp")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

## Cluster jerárquico
# matriz de disimilaridad
d <- dist(dej1, method = "euclidean")

# clusters
hc1 <- hclust(d)

# 
fviz_nbclust(dej1, hcut, method = "silhouette")
#gráficar el dendrograma
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 2, border = 2:5)
sub_grp_comp <- cutree(hc1, k = 2)

##############
#Ejemplo 2
#k-means
head(iris)
dim(iris)
unique(iris$Species)
irisScale = scale(iris[,-5])

# determinar número óptimo k
#Elbow
fviz_nbclust(iris[,-5], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

# Average silhouette
fviz_nbclust(iris[,-5], kmeans, method = "silhouette")

### Gap statistic
fviz_nbclust(irisScale, kmeans, method = "gap_stat")

fitK <- kmeans(iris[,-5], 3)
iris$cluster<-fitK$cluster
fviz_cluster(fitK, data = iris[,-c(5,6)],geom = c("point"))

table(Obs =iris$Species,Pred=iris$cluster)
iris$cluster<-factor(iris$cluster)
library(plyr)
iris$clus<-mapvalues(iris$cluster,c("1","2","3"),c("setosa","versicolor" ,"virginica"))
caret::confusionMatrix(iris$Species,iris$clus)
table(iris$Species)

## Cluster jerárquico
# matriz de disimilaridad
d <- dist(irisScale, method = "euclidean")

# clusters
hc1 <- hclust(d)

# gráficar el dendograma
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 3, border = 2:5)

#Evaluar modelo
iris$hclust<-cutree(hc1,k=3)
table(Obs =iris$Species,Pred=iris$hclust)

