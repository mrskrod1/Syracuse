install.packages("RWeka")
library(RWeka)
#remove columns names and type
zoo_unlabel<-zoo[,c(2:17)]
str(zoo_unlabel)

#R-Kmeans algorithm
model_r<- kmeans(zoo_unlabel, 7)
model_r

#Print Centroids
model_r$centers

#Get cluster assignment
cluster_assignment<- data.frame(zoo, model_r$cluster)
View(cluster_assignment)

#visualize animal types and clusters by specific features,
#red = milk yes, black = milk no.
plot(zoo$type ~ jitter(model_r$cluster, 1),
pch=21, col=as.factor(zoo$milk))

## use PCA in visualization package "cluster" to visualize
#kMeans model. PCA is principal components analysis
install.packages("cluster")
library(cluster)
clusplot(zoo_unlabel, model_r$cluster, color=TRUE, shade=TRUE,
labels=2, lines=0) #plot clusters

#use R's HAC algorithm, which uses Euclidean distance and
# complete linkage by default.
d=dist(as.matrix(zoo_unlabel))
hc=hclust(d)
plot(hc)




