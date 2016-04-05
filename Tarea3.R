# install.packages("clue")
# install.packages("Rcmdr")
library("stringr")
library("caret")
library("clue")
library("cluster")
library("rgl")
library("Rcmdr")

# First set the location directory


 
############################ ***** a.csv ***** #######################################

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
a.kmeans.accuracy.CM <- 0
for (i in 1:5){
  a.kmeans <- kmeans(a[,c("x", "y")], centers = 3)
  a.kmeans.confusion.Matrix <- table(a.kmeans$cluster, a$class)
  a.kmeans.accuracy.CM <- c(a.kmeans.accuracy.CM, sum(diag(a.kmeans.confusion.Matrix))/sum(a.kmeans.confusion.Matrix))
}
a.kmeans.accuracy.CM <- max(a.kmeans.accuracy.CM)

## Partitioning Around Medioids (PAM)
a.pam <- pam(a[,1:2], 3)
a.pam.confusion.Matrix <- table(a.pam$clustering, a$class)
a.pam.accuracy.CM <- sum(diag(a.pam.confusion.Matrix))/sum(a.pam.confusion.Matrix)

#### Comparison of kmean's centroids vs pam's medioids
kmeans.vs.pam <- 0
if (a.kmeans.accuracy.CM >= a.pam.accuracy.CM){
  kmeans.vs.pam <- a.kmeans.accuracy.CM
  kmeans.vs.pam.choose <- c("kmeans")
}else{
  kmeans.vs.pam <- a.pam.accuracy.CM
  kmeans.vs.pam.choose <- c("pam")
}


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
dist_methods <- c("euclidean", "maximum", "manhattan", "binary", "minkowski")

a.num <- a # a copy of the dataframe
a.num$class <-NULL # Delete class column
a.num <- as.matrix(a.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
a.hclust.better.accuracy <- 0
for (i in 1:length(dist_methods)){
  for (j in 1:length(hclust_methods)){
    a.dist.mat <- dist(a.num, method = dist_methods[i]) # distance matrix
    a.cluster <- hclust(a.dist.mat, method = hclust_methods[j]) # apply method
    a.ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
    a.hclust.confusion.Matrix <- table(as.factor(a$class), as.factor(a.ct))
    a.hclust.accuracy.CM <- sum(diag(a.hclust.confusion.Matrix))/sum(a.hclust.confusion.Matrix)
    if (a.hclust.accuracy.CM > a.hclust.better.accuracy){
      a.better <- c(dist_methods[i], hclust_methods[j])
      a.hclust.better.accuracy <- a.hclust.accuracy.CM
    }
  }
}

# Comparison hclust vs kmeans.vs.pam
if (a.hclust.better.accuracy > kmeans.vs.pam){
  final.cluster <- a.hclust.better.accuracy
  a.dist.mat <- dist(a.num, method = a.better[1]) # distance matrix
  a.cluster <- hclust(a.dist.mat, method = a.better[2]) # apply method
  a.ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
  plot(a.cluster) # dendrogram
  rect.hclust(a.cluster, k = 3, border = c("cyan"))
  plot(a$x, a$y, col= a.ct, main = "HCluster")
  dendrogram <- as.dendrogram(a.cluster)
  corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
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



################################### END a.csv ##############################################









############################## ***** moon.csv *****  ######################################

## Exploratory Analysis
moon <- read.csv("moon.csv")
names(moon)[1] <- "x"
names(moon)[2] <- "y"
names(moon)[3] <- "class"
dim(moon)
names(moon)
str(moon)
summary(moon)
plot(moon$x, moon$y, col=1:2) 


## K means
moon.kmeans.accuracy.CM <- 0
for (i in 1:5){
  moon.kmeans <- kmeans(moon[,c("x", "y")], centers = 2)
  moon.kmeans.CM <- table(moon.kmeans$cluster, moon$class)
  moon.kmeans.accuracy.CM <- c(moon.kmeans.accuracy.CM, sum(diag(moon.kmeans.CM))/sum(moon.kmeans.CM))
}
moon.kmeans.accuracy.CM <- max(moon.kmeans.accuracy.CM)

### ***** plot kmeans 
plot(moon$x, moon$y, col= moon.kmeans$cluster, main = "K-means")
points(moon.kmeans$centers[, c("x", "y")],
       col=1:2,
       pch = 19,
       cex = 3)
###


## Partitioning Around Medioids (PAM)
moon.pam <- pam(moon[,1:2], 2)
moon.pam.CM <- table(moon.pam$clustering, moon$class)
moon.pam.accuracy.CM <- sum(diag(moon.pam.CM))/sum(moon.pam.CM)

### ***** plot PAM
plot(moon$x, moon$y, col = moon.pam$clustering, main = "PAM")
points(moon.pam$medoids,
       col=1:2,
       pch = 18,
       cex = 3)
###


#### Comparison of kmean's centroids vs pam's medioids
kmeans.vs.pam <- 0
if (moon.kmeans.accuracy.CM >= moon.pam.accuracy.CM){
  kmeans.vs.pam <- moon.kmeans.accuracy.CM
  kmeans.vs.pam.choose <- c("kmeans")
}else{
  kmeans.vs.pam <- moon.pam.accuracy.CM
  kmeans.vs.pam.choose <- c("pam")
}


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
moon.num <- moon # a copy of the dataframe
moon.num$class <-NULL # Delete class column
moon.num <- as.matrix(moon.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
moon.hclust.better.accuracy <- 0
for (i in 1:length(dist_methods)){
  for (j in 1:length(hclust_methods)){
    moon.dist.mat <- dist(moon.num, method = dist_methods[i]) # distance matrix
    moon.cluster <- hclust(moon.dist.mat, method = hclust_methods[j]) # apply method
    moon.ct <- cutree(moon.cluster, k =2) # k to generate 3 clusters
    moon.hclust.CM <- table(as.factor(moon$class), as.factor(moon.ct))
    moon.hclust.accuracy.CM <- sum(diag(moon.hclust.CM))/sum(moon.hclust.CM)
    if (moon.hclust.accuracy.CM > moon.hclust.better.accuracy){
      moon.better <- c(dist_methods[i], hclust_methods[j])
      #moon.cluster.better <- moon.cluster
      #moon.ct.better <- moon.ct
      moon.hclust.better.accuracy <- moon.hclust.accuracy.CM
    }
  }
}

#### ****** plot HCLUST
plot(moon.cluster.better) # dendrogram
rect.hclust(moon.cluster.better, k = 2, border = c("cyan"))
plot(moon$x, moon$y, col= moon.ct.better, main = "HCluster")
dendrogram <- as.dendrogram(moon.cluster.better)
corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
plot(corte)
######





# Comparison hclust vs kmeans.vs.pam
if (moon.hclust.better.accuracy > kmeans.vs.pam){
  final.cluster <- moon.hclust.better.accuracy
  moon.dist.mat <- dist(moon.num, method = moon.better[1]) # distance matrix
  moon.cluster <- hclust(moon.dist.mat, method = moon.better[2]) # apply method
  moon.ct <- cutree(moon.cluster, k =2) # k to generate 3 clusters
  plot(moon.cluster) # dendrogram
  rect.hclust(moon.cluster, k = 2, border = c("cyan"))
  plot(moon$x, moon$y, col= moon.ct, main = "HCluster")
  dendrogram <- as.dendrogram(moon.cluster)
  corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
  plot(corte)
}else{
  final.cluster <- kmeans.vs.pam
  if (kmeans.vs.pam.choose[1] == 'pam'){
    plot(moon$x, moon$y, col = moon.pam$clustering, main = "PAM")
    points(moon.pam$medoids,
           col=1:2,
           pch = 18,
           cex = 3)
  }else{
    plot(moon$x, moon$y, col= moon.kmeans$cluster, main = "K-means")
    points(moon.kmeans$centers[, c("x", "y")],
           col=1:2,
           pch = 19,
           cex = 3)
  }
}



################################### END moon.csv ##############################################









############################### ***** h.csv ***** ###############################
h.clase = function(numero){
  # Selecting 3 clusters
  if(numero < 7.0)
    return(1)
  else if(numero < 11.0)
    return(2)
  else
    return(3)
}


## Exploratory Analysis
h <- read.csv("h.csv")
names(h)[1] <- "x"
names(h)[2] <- "y"
names(h)[3] <- "z"
names(h)[4] <- "class"
for (i in 1:length(h$class)){
  h$class[i] <- h.clase(h$class[i])
}
dim(h)
names(h)
str(h)
summary(h)
plot3d(h$x, h$y, h$z, col = h$class)


## K means
h.kmeans.accuracy.CM <- 0
for (i in 1:5){
  h.kmeans <- kmeans(h[,c("x", "y", "z")], centers = 3)
  h.kmeans.CM <- table(h.kmeans$cluster, h$class)
  # In this case, as a list, must use as.numeric and unlist functions
  h.kmeans.accuracy.CM <- c(h.kmeans.accuracy.CM, sum(diag(as.numeric(unlist(h.kmeans))))/sum(as.numeric(unlist(h.kmeans))))
}
h.kmeans.accuracy.CM <- max(h.kmeans.accuracy.CM)

### ***** plot kmeans 
plot3d(h$x, h$y, h$z, col = h$class, main = "K-means")
# plot3d.points
points(h.kmeans$centers[, c("x", "y", "z")],
       col=1:3,
       pch = 19,
       cex = 3)
###


## Partitioning Around Medioids (PAM)
moon.pam <- pam(moon[,1:2], 2)
moon.pam.CM <- table(moon.pam$clustering, moon$class)
moon.pam.accuracy.CM <- sum(diag(moon.pam.CM))/sum(moon.pam.CM)

### ***** plot PAM
plot(moon$x, moon$y, col = moon.pam$clustering, main = "PAM")
points(moon.pam$medoids,
       col=1:2,
       pch = 18,
       cex = 3)
###


#### Comparison of kmean's centroids vs pam's medioids
kmeans.vs.pam <- 0
if (moon.kmeans.accuracy.CM >= moon.pam.accuracy.CM){
  kmeans.vs.pam <- moon.kmeans.accuracy.CM
  kmeans.vs.pam.choose <- c("kmeans")
}else{
  kmeans.vs.pam <- moon.pam.accuracy.CM
  kmeans.vs.pam.choose <- c("pam")
}


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
moon.num <- moon # a copy of the dataframe
moon.num$class <-NULL # Delete class column
moon.num <- as.matrix(moon.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
moon.hclust.better.accuracy <- 0
for (i in 1:length(dist_methods)){
  for (j in 1:length(hclust_methods)){
    moon.dist.mat <- dist(moon.num, method = dist_methods[i]) # distance matrix
    moon.cluster <- hclust(moon.dist.mat, method = hclust_methods[j]) # apply method
    moon.ct <- cutree(moon.cluster, k =2) # k to generate 3 clusters
    moon.hclust.CM <- table(as.factor(moon$class), as.factor(moon.ct))
    moon.hclust.accuracy.CM <- sum(diag(moon.hclust.CM))/sum(moon.hclust.CM)
    if (moon.hclust.accuracy.CM > moon.hclust.better.accuracy){
      moon.better <- c(dist_methods[i], hclust_methods[j])
      #moon.cluster.better <- moon.cluster
      #moon.ct.better <- moon.ct
      moon.hclust.better.accuracy <- moon.hclust.accuracy.CM
    }
  }
}

#### ****** plot HCLUST
plot(moon.cluster.better) # dendrogram
rect.hclust(moon.cluster.better, k = 2, border = c("cyan"))
plot(moon$x, moon$y, col= moon.ct.better, main = "HCluster")
dendrogram <- as.dendrogram(moon.cluster.better)
corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
plot(corte)
######





# Comparison hclust vs kmeans.vs.pam
if (moon.hclust.better.accuracy > kmeans.vs.pam){
  final.cluster <- moon.hclust.better.accuracy
  moon.dist.mat <- dist(moon.num, method = moon.better[1]) # distance matrix
  moon.cluster <- hclust(moon.dist.mat, method = moon.better[2]) # apply method
  moon.ct <- cutree(moon.cluster, k =2) # k to generate 3 clusters
  plot(moon.cluster) # dendrogram
  rect.hclust(moon.cluster, k = 2, border = c("cyan"))
  plot(moon$x, moon$y, col= moon.ct, main = "HCluster")
  dendrogram <- as.dendrogram(moon.cluster)
  corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
  plot(corte)
}else{
  final.cluster <- kmeans.vs.pam
  if (kmeans.vs.pam.choose[1] == 'pam'){
    plot(moon$x, moon$y, col = moon.pam$clustering, main = "PAM")
    points(moon.pam$medoids,
           col=1:2,
           pch = 18,
           cex = 3)
  }else{
    plot(moon$x, moon$y, col= moon.kmeans$cluster, main = "K-means")
    points(moon.kmeans$centers[, c("x", "y")],
           col=1:2,
           pch = 19,
           cex = 3)
  }
}

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












