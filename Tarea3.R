# install.packages("clue")
library("stringr")
library("caret")
library("clue")
library("cluster")
 
##### ***** a.csv ***** #####

## Exploratory Analysis
a <- read.csv("a.csv")
names(a)[1] <- "x"
names(a)[2] <- "y"
names(a)[3] <- "class"
# dim(a)
# names(a)
# str(a)
# summary(a)
# hist(a$x)
# plot(a$x, a$y, col=1:3) 

## K means
a.kmeans <- kmeans(a[,1:2], centers = 3)
a.kmeans.confusion.Matrix <- table(a.kmeans$cluster, a$class)
a.kmeans.accuracy.CM <- sum(diag(a.kmeans.confusion.Matrix))/sum(a.kmeans.confusion.Matrix)


## Partitioning Around Medioids (PAM)
a.pam <- pam(a[,1:2], 3)
a.pam.confusion.Matrix <- table(a.pam$clustering, a$class)
a.pam.accuracy.CM <- sum(diag(a.pam.confusion.Matrix))/sum(a.pam.confusion.Matrix)

#### Comparison of kmean's centroids vs pam's medioids
if (a.kmeans.accuracy.CM >= a.pam.accuracy.CM){
  kmeans.vs.pam <- a.kmeans.accuracy.CM
  kmeans.vs.pam.choose <- c("kmeans")
}else{
  kmeans.vs.pam <- a.pam.accuracy.CM
  kmeans.vs.pam.choose <- c("pam")
}


# Medioids are part of the DataSet, centroids aren't necessarily



## Hcluster
a.hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
a.dist_methods <- c("euclidean", "maximum", "manhattan", "binary", "minkowski")

a.num <- a # a copy of the dataframe
a.num$class <-NULL # Delete class column
a.num <- as.matrix(a.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
a.hclust.better.accuracy <- 0
for (i in 1:length(a.dist_methods)){
  for (j in 1:length(a.hclust_methods)){
    a.dist.mat <- dist(a.num, method = a.dist_methods[i]) # distance matrix
    a.cluster <- hclust(a.dist.mat, method = a.hclust_methods[j]) # apply method
    ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
    a.hclust.confusion.Matrix <- table(as.factor(a$class), as.factor(ct))
    a.hclust.accuracy.CM <- sum(diag(a.hclust.confusion.Matrix))/sum(a.hclust.confusion.Matrix)
    if (a.hclust.accuracy.CM > a.hclust.better.accuracy){
      a.better.methods <- c(a.dist_methods[i], a.hclust_methods[j])
      a.hclust.better.accuracy <- a.hclust.accuracy.CM
    }
  }
}

# Comparison hclust vs kmeans.vs.pam
if (a.hclust.better.accuracy > kmeans.vs.pam){
  final.cluster <- a.hclust.better.accuracy
  a.dist.mat <- dist(a.num, method = a.better.methods[1]) # distance matrix
  a.cluster <- hclust(a.dist.mat, method = a.better.methods[2]) # apply method
  a.ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
  plot(a.cluster) # dendrogram
  rect.hclust(a.cluster, k = 3, border = c("cyan"))
  plot(a$x, a$y, col= a.ct, main = "HCluster")
  dendrogram <- as.dendrogram(a.cluster)
  corte <- cut(dendrogram, h=20)$upper
  plot(corte)
}else{
  final.cluster <- kmeans.vs.pam
  if (kmeans.vs.pam.choose[1] == 'pam'){
    plot(a$x, a$y, col = a.pam$clustering, main = "PAM")
    points(a.pam$medoids,
           col=1:3,
           pch = 18,
           cex = 3)
  }else{
    plot(a$x, a$y, col= a.kmeans$cluster, main = "K-means")
    points(a.kmeans$centers[, c("x", "y")],
           col=1:3,
           pch = 19,
           cex = 3)
  }
}


















#ct1 <- cutree(a.cluster, h =15) # h para la altura
#plot(a$x, a$y, col = ct1)
#table(a$class, ct1)


#dendrogram <- as.dendrogram(a.cluster)
#plot(dendrogram)
# Cortar el dendrograma por encima




##### ***** moon.csv ***** #####

## Exploratory Analysis
moon <- read.csv("moon.csv")
dim(moon)
names(moon)
str(moon)
summary(moon)

# dist_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
##### ***** h.csv ***** #####

## Exploratory Analysis
h <- read.csv("h.csv")
dim(h)
names(h)
str(h)
summary(h)

## K means

## Hcluster

## Confussion Matrix
accuracy <- sum(diag(confusionMatrix.PART))/sum(confusionMatrix.PART)





##### ***** s.csv ***** #####

## Exploratory Analysis
s <- read.csv("s.csv")
dim(s)
names(s)
str(s)
summary(s)

## K means

## Hcluster

## Confussion Matrix

##### ***** a_big.csv ***** #####

## Exploratory Analysis
a_big <- read.csv("a_big.csv")
dim(a_big)
names(a_big)
str(a_big)
summary(a_big)




##### ***** help.csv ***** #####
## Exploratory Analysis
help <- read.csv("help.csv")
dim(help)
names(help)
str(help)
summary(help)

## K means

## Hcluster

## Confussion Matrix


##### ***** guess.csv ***** #####

## Exploratory Analysis
guess <- read.csv("guess.csv")
dim(guess)
names(guess)
str(guess)
summary(guess)

## K means

## Hcluster

## Confussion Matrix


##### ***** good_luck.csv ***** #####

## Exploratory Analysis
good_luck <- read.csv("good_luck.csv")
dim(good_luck)
names(good_luck)
str(good_luck)
summary(good_luck)

## K means

## Hcluster

## Confussion Matrix












