## Prediction using Unsupervised learning

library(tidyverse)
install.packages("cluster")
library(cluster)
install.packages("reshape2")
library(reshape2)
library(ggplot2)


iris1 = read.csv(file.choose())
iris1
summary(iris1)
class(iris1)
head(iris1)
View(iris1)


## lets visualize the data with ggplot2
## Sepal-Length vs. Sepal-Width

ggplot(iris1)+
  geom_point(aes(x = SepalLengthCm, y = SepalWidthCm ), stroke = 2)+
  facet_wrap(~ Species)+ 
  labs(x = "Sepal Length", y = "Sepal Width")+
  theme_bw()

## Petal-Length vs. Petal-Width

ggplot(iris1)+
  geom_point(aes(x = PetalLengthCm, y = PetalWidthCm ), stroke = 2)+
  facet_wrap(~ Species)+ 
  labs(x = "Petal Length", y = "Petal Width")+
  theme_bw()

## Sepal-Width vs. Pedal-Width

ggplot(iris1)+
  geom_point(aes(x = SepalWidthCm, y = PetalWidthCm ), stroke = 2)+
  facet_wrap(~ Species)+ 
  labs(x = "Sepal Width", y = "Petal Width")+
  theme_dark()

## Sepal-Length vs. Petal-Length
ggplot(iris1)+
  geom_point(aes(x = SepalLengthCm, y = PetalLengthCm, ), stroke = 2)+
  facet_wrap(~ Species)+ 
  labs(x = "Setal Length", y = "Petal Length")+
  theme_dark()

## box plots

ggplot(iris1)+
  geom_boxplot(aes(x = Species, y = SepalLengthCm, fill = Species))+
  theme_dark()
ggplot(iris1)+
  geom_boxplot(aes(x = Species, y = SepalWidthCm, fill = Species))+
  theme_get()
ggplot(iris1)+
  geom_boxplot(aes(x = Species, y = PetalLengthCm, fill = Species))+
  theme_bw()
ggplot(iris1)+
  geom_boxplot(aes(x = Species, y = PetalWidthCm, fill = Species))+
  theme_bw()

##k-means Clustering
# k-means clustering is a method of vector quantization, 
# that aims to partition n observations into k clusters in which each observation belongs to the cluster 
# with the nearest mean (cluster centers or cluster centroid), serving as a prototype of the cluster.
iris1[is.na(iris1)] = 0

set.seed(200)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(iris1[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss

plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

icluster <- kmeans(iris1[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)


ggplot(iris1,aes(x = PetalLengthCm, y = PetalWidthCm, col= Species)) + geom_point()
