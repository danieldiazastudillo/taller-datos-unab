#####K-means####

library(ks)

#Obtener datos laptop
laptop <- read.csv("C:/Users/daniel.diaz/Desktop/taller-datos/laptop.csv", sep=";") # nolint: line_length_linter.
dim(laptop)

# take for laptop just the columns
# os_bit, graphic_card_gb, warranty and price
laptop <- laptop[, c(6, 7, 10, 13)]


summary(laptop)
boxplot(laptop)
row.names(laptop)

#Prepara los datos
dej1 <- scale(laptop)
summary(dej1)
sum(is.na(dej1))
boxplot(dej1)

#Determinar número óptimo de k
library(factoextra)
# Elbow method
fviz_nbclust(dej1, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)

# Average silhouette
fviz_nbclust(dej1, kmeans, method = "silhouette")

### Gap statistic
fviz_nbclust(dej1, kmeans, method = "gap_stat")

#ajustar modelo (gap_stat)
fit1 <- kmeans(dej1, 2) #para 2 grupos
table(fit1$cluster)
funModeling::freq(fit1$cluster)
laptop$clus <- as.vector(fit1$cluster)
fviz_cluster(fit1, data = laptop, geom = c("point"))

#ajustar modelo (elbow)
fit2 <- kmeans(dej1, 3)
table(fit2$cluster)
funModeling::freq(fit2$cluster)
laptop$clus <- as.vector(fit2$cluster)
fviz_cluster(fit2, data = laptop, geom = c("point"))

library(ggplot2)
#Boxplot
ggplot(laptop, aes(y = graphic_card_gb, x = factor(clus))) +
  geom_boxplot(fill = "lightgreen", varwidth = T) +
  labs(x = "", y = "graphic_card_gb") +
  theme(text = element_text(size = 14)) +
  theme_grey(base_size = 16)

ggplot(laptop, aes(y = warranty, x = factor(clus))) +
    geom_boxplot(fill = "lightgreen", varwidth = T) +
    labs(x = "", y = "warranty") +
    theme(text = element_text(size = 14)) +
    theme_grey(base_size = 16)


ggplot(laptop, aes(y = os_bit, x = factor(clus))) +
    geom_boxplot(fill = "lightgreen", varwidth = T) +
    labs(x = "", y = "os_bit") +
    theme(text = element_text(size = 14)) +
    theme_grey(base_size = 16)

ggplot(laptop, aes(y = price, x = factor(clus))) +
    geom_boxplot(fill = "lightgreen", varwidth = T) +
    labs(x = "", y = "price") +
    theme(text = element_text(size = 14)) +
    theme_grey(base_size = 16)

# Cluster jerárquico
# matriz de disimilaridad
d <- dist(dej1, method = "euclidean")

# clusters
hc1 <- hclust(d)

#
fviz_nbclust(dej1, hcut, method = "silhouette")
#graficar el dendograma
plot(hc1, hang = -1, cex = 0.6)
rect.hclust(hc1, k = 2, border = 2:5)
sub_grp_comp <- cutree(hc1, k = 2)
