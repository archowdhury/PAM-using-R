library(ISLR)
library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)

data(iris)


iris$Petal.Length = log(iris$Petal.Length)
iris$Petal.Width = log(iris$Petal.Width)

gower_dist = daisy(iris[,-5],metric="gower")
summary(gower_dist)
gower_mat = as.matrix(gower_dist)
dim(gower_mat)

sil_width = c(NA)
for(i in 2:10)
{
  pam_fit = pam(gower_dist, diss = TRUE, k=i)
  sil_width[i] = pam_fit$silinfo$avg.width
}

plot(1:10,sil_width,xlab="Number of Clusters",ylab="Silhouette width")
lines(1:10, sil_width)

pam_fit = pam(gower_dist, diss=TRUE, k=3)

iris$cluster = pam_fit$clustering


tsne_obj = Rtsne(gower_dist,is_distance = TRUE)
iris_plot = tsne_obj$Y
iris_plot = data.frame(iris_plot)
names(iris_plot) = c("X","Y")
iris_plot$cluster = as.factor(iris$cluster)

ggplot(aes(x=X, y=Y), data=iris_plot) + geom_point(aes(color=cluster))
