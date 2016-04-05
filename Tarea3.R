# install.packages("clue")
# install.packages("Rcmdr")
library("stringr")
library("caret")
library("clue")
library("cluster")
library("rgl")
# First set the location directory
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
dist_methods <- c("euclidean", "maximum", "manhattan", "binary", "minkowski")

 
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
a.kmeans.better.accuracy.CM <- 0
for (i in 1:10){
  a.kmeans <- kmeans(a[,c("x", "y")], centers = 3)
  a.kmeans.CM <- table(a.kmeans$cluster, a$class)
  a.kmeans.accuracy.CM <- sum(diag(a.kmeans.CM))/sum(a.kmeans.CM)
  if (a.kmeans.accuracy.CM > a.kmeans.better.accuracy.CM){
    a.kmeans.better <- a.kmeans
    a.kmeans.better.accuracy.CM <- a.kmeans.accuracy.CM # useless (?)
  }
}
a.kmeans <- a.kmeans.better



## Partitioning Around Medioids (PAM)
a.pam <- pam(a[,1:2], 3)
a.pam.CM <- table(a.pam$clustering, a$class)
a.pam.accuracy.CM <- sum(diag(a.pam.CM))/sum(a.pam.CM)

#### Comparison of kmean's centroids vs pam's medioids
a.kmeans.vs.pam <- 0
if (a.kmeans.accuracy.CM >= a.pam.accuracy.CM){
  a.kmeans.vs.pam <- a.kmeans.accuracy.CM
  a.kmeans.vs.pam.choose <- c("kmeans")
}else{
  a.kmeans.vs.pam <- a.pam.accuracy.CM
  a.kmeans.vs.pam.choose <- c("pam")
}


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster

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
if (a.hclust.better.accuracy > a.kmeans.vs.pam){
  a.final.cluster <- a.hclust.better.accuracy
  a.dist.mat <- dist(a.num, method = a.better[1]) # distance matrix
  a.cluster <- hclust(a.dist.mat, method = a.better[2]) # apply method
  a.ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
  a.dendrogram <- as.dendrogram(a.cluster)
  plot(a.dendrogram) # dendrogram
  rect.hclust(a.cluster, k = 3, border = c("cyan"))
  a.corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
  plot(a.corte)
  plot(a$x, a$y, col= a.ct, main = "HCluster")
}else{
  a.final.cluster <- a.kmeans.vs.pam
  if (a.kmeans.vs.pam.choose[1] == 'pam'){
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
# dim(moon)
# names(moon)
# str(moon)
# summary(moon)
# plot(moon$x, moon$y, col=1:2) 


## K means
moon.kmeans.accuracy.CM <- 0
moon.kmeans.better.accuracy.CM <- 0
for (i in 1:5){
  moon.kmeans <- kmeans(moon[,c("x", "y")], centers = 2)
  moon.kmeans.CM <- table(moon.kmeans$cluster, moon$class)
  # comparing and choosing the best kmeans
  moon.kmeans.accuracy.CM <- sum(diag(moon.kmeans.CM))/sum(moon.kmeans.CM)
  if (moon.kmeans.accuracy.CM > moon.kmeans.better.accuracy.CM){
    moon.kmeans.better <- moon.kmeans
    moon.kmeans.better.accuracy.CM <- moon.kmeans.accuracy.CM # useles (?)
  }
}
moon.kmeans <- moon.kmeans.better


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
moon.kmeans.vs.pam <- 0
if (moon.kmeans.accuracy.CM >= moon.pam.accuracy.CM){
  moon.kmeans.vs.pam <- moon.kmeans.accuracy.CM
  moon.kmeans.vs.pam.choose <- c("kmeans")
}else{
  moon.kmeans.vs.pam <- moon.pam.accuracy.CM
  moon.kmeans.vs.pam.choose <- c("pam")
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
if (moon.hclust.better.accuracy > moon.kmeans.vs.pam){
  moon.final.cluster <- moon.hclust.better.accuracy
  moon.dist.mat <- dist(moon.num, method = moon.better[1]) # distance matrix
  moon.cluster <- hclust(moon.dist.mat, method = moon.better[2]) # apply method
  moon.ct <- cutree(moon.cluster, k =2) # k to generate 3 clusters
  moon.dendrogram <- as.dendrogram(moon.cluster)
  plot(moon.dendrogram) # dendrogram
  rect.hclust(moon.cluster, k = 2, border = c("cyan"))
  moon.corte <- cut(moon.dendrogram, h=20)$upper # $upper to get useful information instead a forest
  plot(moon.corte)
  plot(moon$x, moon$y, col= moon.ct, main = "HCluster")
  
}else{
  moon.final.cluster <- moon.kmeans.vs.pam
  if (moon.kmeans.vs.pam.choose[1] == 'pam'){
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
#dim(h)
#names(h)
#str(h)
#summary(h)
plot3d(h$x, h$y, h$z, col = h$class)


## K means
h.kmeans.accuracy.CM <- 0
h.kmeans.better.accuracy.CM <- 0
for (i in 1:10){
  h.kmeans <- kmeans(h[,c("x", "y", "z")], centers = 3)
  h.kmeans.CM <- table(h.kmeans$cluster, h$class)
  # comparing and choosing the best kmeans
  h.kmeans.accuracy.CM <- sum(diag(h.kmeans.CM))/sum(h.kmeans.CM)
  if (h.kmeans.accuracy.CM > h.kmeans.better.accuracy.CM){
    h.kmeans.better <- h.kmeans
    h.kmeans.better.accuracy.CM <- h.kmeans.accuracy.CM
  }
}
h.kmeans <- h.kmeans.better

### ***** plot kmeans 
rgl.open()
rgl.bg(color = "white") # Setup the background color
plot3d(h$x, h$y, h$z, col = h.kmeans$cluster, main = "K-means")
rgl.spheres(h.kmeans$centers[, c("x", "y", "z")], r = 0.4, color = 1:3) 
rgl.close()
###



## Partitioning Around Medioids (PAM)
h.pam <- pam(h[,1:3], 3)
h.pam.CM <- table(h.pam$clustering, h$class)
h.pam.accuracy.CM <- sum(diag(h.pam.CM))/sum(h.pam.CM)

### ***** plot PAM
rgl.open()
rgl.bg(color = "white") # Setup the background color
plot3d(h$x, h$y, h$z, col = h.pam$clustering, main = "PAM")
rgl.spheres(h.pam$medoids[, c("x", "y", "z")], r = 0.4, color = 1:3) 
rgl.close()
###


#### Comparison of kmean's centroids vs pam's medioids
h.kmeans.vs.pam <- 0
if (h.kmeans.accuracy.CM >= h.pam.accuracy.CM){
  h.kmeans.vs.pam <- h.kmeans.accuracy.CM
  h.kmeans.vs.pam.choose <- c("kmeans")
}else{
  h.kmeans.vs.pam <- h.pam.accuracy.CM
  h.kmeans.vs.pam.choose <- c("pam")
}


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
h.num <- h # a copy of the dataframe
h.num$class <-NULL # Delete class column
h.num <- as.matrix(h.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
h.hclust.better.accuracy <- 0
for (i in 1:length(dist_methods)){
  for (j in 1:length(hclust_methods)){
    h.dist.mat <- dist(h.num, method = dist_methods[i]) # distance matrix
    h.cluster <- hclust(h.dist.mat, method = hclust_methods[j]) # apply method
    h.ct <- cutree(h.cluster, k =3) # k to generate 3 clusters
    h.hclust.CM <- table(as.factor(h$class), as.factor(h.ct))
    h.hclust.accuracy.CM <- sum(diag(h.hclust.CM))/sum(h.hclust.CM)
    if (h.hclust.accuracy.CM > h.hclust.better.accuracy){
      h.better <- c(dist_methods[i], hclust_methods[j])
      h.cluster.better <- h.cluster ##
      h.ct.better <- h.ct ##
      h.hclust.better.accuracy <- h.hclust.accuracy.CM
    }
  }
}

#### ****** plot HCLUST
dendrogram <- as.dendrogram(h.cluster.better)
plot(dendrogram) # dendrogram
rect.hclust(h.cluster.better, k = 3, border = c("cyan"))
corte <- cut(dendrogram, h=16)$upper # $upper to get useful information instead a forest
plot(corte)
rgl.open()
rgl.bg(color = "white") # Setup the background color
plot3d(h$x, h$y, h$z, col = h.ct.better, main = "HCluster")
rgl.close()
######





# Comparison hclust vs kmeans.vs.pam
if (h.hclust.better.accuracy > h.kmeans.vs.pam){
  h.final.cluster <- h.hclust.better.accuracy
  h.dist.mat <- dist(h.num, method = h.better[1]) # distance matrix
  h.cluster <- hclust(h.dist.mat, method = h.better[2]) # apply method
  h.ct <- cutree(h.cluster, k =3) # k to generate 3 clusters
  h.dendrogram <- as.dendrogram(h.cluster.better)
  plot(h.dendrogram) # dendrogram
  rect.hclust(h.cluster.better, k = 3, border = c("cyan"))
  h.corte <- cut(h.dendrogram, h=16)$upper # $upper to get useful information instead a forest
  plot(h.corte)
  rgl.open()
  rgl.bg(color = "white") # Setup the background color
  plot3d(h$x, h$y, h$z, col = h.ct, main = "HCluster")
}else{
  h.final.cluster <- h.kmeans.vs.pam
  if (h.kmeans.vs.pam.choose[1] == 'pam'){
    rgl.open()
    rgl.bg(color = "white") # Setup the background color
    plot3d(h$x, h$y, h$z, col = h.pam$clustering, main = "PAM")
    rgl.spheres(h.pam$medoids[, c("x", "y", "z")], r = 0.4, color = 1:3) 
  }else{
    rgl.open()
    rgl.bg(color = "white") # Setup the background color
    plot3d(h$x, h$y, h$z, col = h.kmeans$cluster, main = "K-means")
    rgl.spheres(h.kmeans$centers[, c("x", "y", "z")], r = 0.4, color = 1:3) 
  }
}

##### ***** s.csv ***** #####

## Exploratory Analysis
s <- read.csv("s.csv")
dim(s)
names(s)
str(s)
summary(s)



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












