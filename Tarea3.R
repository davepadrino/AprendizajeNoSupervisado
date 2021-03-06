# install.packages("clue")
# install.packages("Rcmdr")
library("stringr")
library("caret")
library("clue")
library("cluster")
library("rgl")
# First set the location directory
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
dist_methods <- c("euclidean", "maximum", "manhattan", "minkowski")

############################ functions #######################################
#Set classes after 1
set_class = function(dataset){
  dataset$class <-as.numeric(dataset$class)
  dataset$class <- dataset$class + 1
  return(dataset)
}


# codo de jambu 
c.jambu= function(d){
  mydata <- d
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

# Calculating best kmeans algorithm 2D
k.means2D = function(dataset, centers){
  kmeans.accuracy.CM <- 0
  kmeans.better.accuracy.CM <- 0
  for (i in 1:10){
    kmeans <- kmeans(dataset[,c("x", "y")], centers = centers)
    kmeans.CM <- table(kmeans$cluster, dataset$class)
    kmeans.accuracy.CM <- sum(diag(kmeans.CM))/sum(kmeans.CM)
    if (kmeans.accuracy.CM > kmeans.better.accuracy.CM){
      kmeans.better <- kmeans
      kmeans.better.accuracy.CM <- kmeans.accuracy.CM # useless (?)
    }
  }
  return(kmeans.better)
}


# Calculating best kmeans algorithm 3D
k.means3D = function(dataset, centers){
  kmeans.accuracy.CM <- 0
  kmeans.better.accuracy.CM <- 0
  for (i in 1:10){
    kmeans <- kmeans(dataset[,c("x", "y","z")], centers = centers)
    kmeans.CM <- table(kmeans$cluster, dataset$class)
    kmeans.accuracy.CM <- sum(diag(kmeans.CM))/sum(kmeans.CM)
    if (kmeans.accuracy.CM > kmeans.better.accuracy.CM){
      kmeans.better <- kmeans
      kmeans.better.accuracy.CM <- kmeans.accuracy.CM # useless (?)
    }
  }
  return(kmeans.better)
}


# Comparing kmeans vs PAM
compare.kmeans.pam = function(kmeans.acc, pam.acc){
  kmeans.vs.pam <- 0
  if (kmeans.acc >= pam.acc){
    #a.kmeans.vs.pam <- kmeans.acc
    kmeans.vs.pam <- c("kmeans", kmeans.acc)
    return(kmeans.vs.pam) 
  }else{
    #a.kmeans.vs.pam <- pam.acc
    kmeans.vs.pam <- c("pam", pam.acc)
    return(kmeans.vs.pam) 
  }
}

# Calculating best K for good_luck.csv
good_luck.kmeans.fun <- function(array, good_luck){
  better.kmeans <- 0
  for (i in 2:length(array)){
    good_luck.kmeans <- kmeans(good_luck[,array], centers = i)
    good_luck.kmeans.CM <- table(good_luck.kmeans$cluster, good_luck$class)
    good_luck.kmeans.accuracy <- sum(diag(good_luck.kmeans.CM))/sum(good_luck.kmeans.CM)
    if (good_luck.kmeans.accuracy > better.kmeans){
      better.kmeans <- good_luck.kmeans.accuracy
      fin <- c(better.kmeans, i)
    }
  }
  return(fin)
}


# Calculating each distance method vs each hclust method 
match.hclust = function(matrix, k, dataset){
  better.accuracy <- 0
  for (i in 1:length(dist_methods)){
    for (j in 1:length(hclust_methods)){
      dist.mat <- dist(matrix, method = dist_methods[i]) # distance matrix
      cluster <- hclust(dist.mat, method = hclust_methods[j]) # apply method
      ct <- cutree(cluster, k) # k to generate k clusters
      CM <- table(as.factor(dataset$class), as.factor(ct))
      accuracy.CM <- sum(diag(CM))/sum(CM)
      if (accuracy.CM > better.accuracy){
        better.accuracy <- accuracy.CM
        better <- c(dist_methods[i], hclust_methods[j], accuracy.CM)
      }
    }
  }
  return(better)
}

euc.distance <- function(x1, y1, x2, y2){
  return( sqrt( ( (x1 - x2) ^ 2) + ( (y1 - y2) ^ 2) ) )
} 

do.distance.matrix <- function(dataframe, dist.matrix, cent){
  for (i in 1:nrow(cent))
    dist.matrix[,i] <- euc.distance(cent[i,1], cent[i,2], dataframe[,1], dataframe[,2])
  return(dist.matrix)
}

myKmeans <- function(dataframe, k, nItter, centroids = NULL){
  if(is.null(centroids)){
    rand.centroids <- dataframe[sample(nrow(dataframe), k), ][-3]  
    centroids <- matrix(0, nrow=k, ncol=2)
    centroids[,1] <- rand.centroids[,1]
    centroids[,2] <- rand.centroids[,2]
  }
  cluster <- vector(mode="list", length = nItter)
  center <- vector(mode="list", length = nItter)
  clusters <- vector(mode = "numeric", length = nrow(dataframe) )
  distsToCenters <- matrix(0, nrow = nrow(dataframe), ncol = nrow(centroids))
  n.clust <- vector("numeric", 3)
  n.clust.aux <- vector("numeric", 3)
  for(i in 1:nItter) {
    distsToCenters <- do.distance.matrix(dataframe, distsToCenters,centroids) # distance matrix
    clusters <- apply(distsToCenters, 1, which.min) # clusterize
    centroids <- apply(dataframe, 2, tapply, clusters, mean)
    if (i>1){
      for(j in 1:length(n.clust)){
        n.clust.aux[j] <- length(clusters[clusters == j])
      }
      if((n.clust[1] == n.clust.aux[1]) && (n.clust[2] == n.clust.aux[2]) && (n.clust[3] == n.clust.aux[3])){ # if there are no changes, return list
        cluster <- clusters
        center <- centroids
        return(list(centroids = center, clusters = cluster))
      }else{
        n.clust[1] = n.clust.aux[1]
        n.clust[2] = n.clust.aux[2]
        n.clust[3] = n.clust.aux[3]
      }
    }
  }
}


############################ end functions #######################################


############################ ***** a.csv ***** #######################################

## Exploratory Analysis
a <- read.csv("a.csv")
names(a)[1] <- "x"
names(a)[2] <- "y"
names(a)[3] <- "class"
a <- set_class(a)
# dim(a)
# names(a)
# summary(a)
# plot(a$x, a$y, col=1:3) 

## K means
a.kmeans <- k.means2D(dataset = a, centers =3)
a.kmeans.CM <- table(a.kmeans$cluster, a$class)
a.kmeans.accuracy <- sum(diag(a.kmeans.CM))/sum(a.kmeans.CM)


#####################################################################3
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
a.kmeans.accuracy.CM <- a.kmeans.better.accuracy.CM
#########################################################################

## Partitioning Around Medioids (PAM)
a.pam <- pam(a[,1:2], 3)
a.pam.CM <- table(a.pam$clustering, a$class)
a.pam.accuracy.CM <- sum(diag(a.pam.CM))/sum(a.pam.CM)

#### Comparison of kmean's centroids vs pam's medioids
a.kmeans.vs.pam <- compare.kmeans.pam(a.kmeans.accuracy, a.pam.accuracy.CM)

# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
a.num <- a # a copy of the dataframe
a.num$class <-NULL # Delete class column
a.num <- as.matrix(a.num) # convert into a matrix


#################################################################
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
#################################################################

# Calculating best hclust method 
a.hclust = match.hclust(a.num, k = 3, a)

# Comparison hclust vs kmeans.vs.pam
if (a.hclust[3] > a.kmeans.vs.pam[2]){
  a.final.cluster <- a.hclust[3]
  a.dist.mat <- dist(a.num, method = a.hclust[1]) # distance matrix
  a.cluster <- hclust(a.dist.mat, method = a.hclust[2]) # apply method
  a.ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
  a.dendrogram <- as.dendrogram(a.cluster)
  plot(a.dendrogram) # dendrogram
  rect.hclust(a.cluster, k = 3, border = c("red"))
  a.corte <- cut(a.dendrogram, h=20)$upper # $upper to get useful information instead a forest
  plot(a.corte)
  plot(a$x, a$y, col= a.ct, main = "HCluster")
}else{
  a.final.cluster <- a.kmeans.vs.pam
  if (a.kmeans.vs.pam[1] == 'pam'){
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
moon <- set_class(moon)
# dim(moon)
# names(moon)
# str(moon)
# summary(moon)
# plot(moon$x, moon$y, col=1:2) 

## K means
moon.kmeans <- k.means2D(dataset = moon, centers = 2)
moon.kmeans.CM <- table(moon.kmeans$cluster, moon$class)
moon.kmeans.accuracy <- sum(diag(moon.kmeans.CM))/sum(moon.kmeans.CM)

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
moon.kmeans.vs.pam <- compare.kmeans.pam(moon.kmeans.accuracy, moon.pam.accuracy.CM)


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
moon.num <- moon # a copy of the dataframe
moon.num$class <-NULL # Delete class column
moon.num <- as.matrix(moon.num) # convert into a matrix

# Calculating best hclust method 

moon.hclust = match.hclust(moon.num, 2, moon)

###############################################################################
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
########################################################################################3


# Comparison hclust vs kmeans.vs.pam
if (moon.hclust[3] > moon.kmeans.vs.pam[2]){
  moon.final.cluster <- moon.hclust.better.accuracy
  moon.dist.mat <- dist(moon.num, method = moon.hclust[1]) # distance matrix
  moon.cluster <- hclust(moon.dist.mat, method = moon.hclust[2]) # apply method
  moon.ct <- cutree(moon.cluster, k =2) # k to generate 3 clusters
  moon.dendrogram <- as.dendrogram(moon.cluster)
  plot(moon.dendrogram) # dendrogram
  rect.hclust(moon.cluster, k = 2, border = c("cyan"))
  moon.corte <- cut(moon.dendrogram, h=20)$upper # $upper to get useful information instead a forest
  plot(moon.corte)
  plot(moon$x, moon$y, col= moon.ct, main = "HCluster")
}else{
  moon.final.cluster <- moon.kmeans.vs.pam
  if (moon.kmeans.vs.pam[1] == 'pam'){
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
  # Selecting 6 clusters
  if(numero < 7.0)
    return(1)
  else if(numero < 9.0)
    return(2)
  else if(numero < 11.0)
    return(3)
  else if(numero < 12.0)
    return(4)
  else if(numero < 13.0)
    return(5)
  else
    return(6)
}
# se eligieron 6 clusters por la forma del dataset, a medida que el "espiral" se va "desenrollando" da la impresion que tambien se van separando los puntos del conglomerado inicial
# ademas se eligio un numero entero menor y mas cercano al primer cuartil, mismo para el 3er cuartil


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
summary(h)
plot3d(h$x, h$y, h$z, col = h$class)



## K means ############################

h.kmeans <- k.means3D(dataset = h, centers = 6)
h.kmeans.CM <- table(h.kmeans$cluster, h$class)
h.kmeans.accuracy <- sum(diag(h.kmeans.CM))/sum(h.kmeans.CM)


### ***** plot kmeans 
plot3d(h$x, h$y, h$z, col = h.kmeans$cluster, main = "K-means")
rgl.spheres(h.kmeans$centers[, c("x", "y", "z")], r = 0.4, color = 1:6) 
###



## Partitioning Around Medioids (PAM)
h.pam <- pam(h[,1:3], 6)
h.pam.CM <- table(h.pam$clustering, h$class)
h.pam.accuracy.CM <- sum(diag(h.pam.CM))/sum(h.pam.CM)

### ***** plot PAM
plot3d(h$x, h$y, h$z, col = h.pam$clustering, main = "PAM")
rgl.spheres(h.pam$medoids[, c("x", "y", "z")], r = 0.4, color = 1:6) 
###


#### Comparison of kmean's centroids vs pam's medioids
h.kmeans.vs.pam <- compare.kmeans.pam(h.kmeans.accuracy, h.pam.accuracy.CM)

# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
h.num <- h # a copy of the dataframe
h.num$class <-NULL # Delete class column
h.num <- as.matrix(h.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
h.hclust = match.hclust(h.num, 6, h)

#### ****** plot HCLUST
h.dist.mat <- dist(h.num, method = h.hclust[1]) # distance matrix
h.cluster <- hclust(h.dist.mat, method = h.hclust[2]) # apply method
h.ct <- cutree(h.cluster, k =6) # k to generate 3 clusters
h.dendrogram <- as.dendrogram(h.cluster)
plot(h.dendrogram) # dendrogram
rect.hclust(h.cluster, k = 5, border = c("red"))
plot3d(h$x, h$y, h$z, col = h.ct, main = "HCluster")
######


# Comparison hclust vs kmeans.vs.pam
if (h.hclust[3] > h.kmeans.vs.pam[2]){
  h.final.cluster <- c(h.hclust[3], "HCluster")
  h.dist.mat <- dist(h.num, method = h.hclust[1]) # distance matrix
  h.cluster <- hclust(h.dist.mat, method = h.hclust[2]) # apply method
  h.ct <- cutree(h.cluster, k =5) # k to generate 3 clusters
  h.dendrogram <- as.dendrogram(h.cluster)
  plot(h.dendrogram) # dendrogram
  rect.hclust(h.cluster, k = 5, border = c("red"))
  plot3d(h$x, h$y, h$z, col = h.ct, main = "HCluster")
}else{
  h.final.cluster <- c(h.kmeans.vs.pam, "HClust")
  if (h.kmeans.vs.pam[1] == 'pam'){
    h.final.cluster <- c(h.kmeans.vs.pam, "PAM")
    plot3d(h$x, h$y, h$z, col = h.pam$clustering, main = "PAM")
    rgl.spheres(h.pam$medoids[, c("x", "y", "z")], r = 0.4, color = 1:5) 
  }else{
    h.final.cluster <- c(h.kmeans.vs.pam, "Kmedias")
    plot3d(h$x, h$y, h$z, col = h.kmeans$cluster, main = "K-means")
    rgl.spheres(h.kmeans$centers[, c("x", "y", "z")], r = 0.4, color = 1:5) 
  }
}

if (h.final.cluster[2] == "HCluster"){
  h.hclust[1] # Distancia
  h.hclust[2] # Método de Hclust
  h.hclust[3] #Efectividad
}else{
  h.final.cluster[1]
}


################################### END h.csv ##############################################



############################### ***** s.csv ***** ###############################
s.clase = function(numero){
  # Selecting 2 clusters
  if(numero < 0.0)
    return(1)
  else
    return(2)
} 


## Exploratory Analysis
s <- read.csv("s.csv")
# c.jambu(s)
# summary(s)
names(s)[1] <- "x"
names(s)[2] <- "y"
names(s)[3] <- "z"
names(s)[4] <- "class"
plot3d(s$x, s$y, s$z)
for (i in 1:length(s$class)){
  s$class[i] <- s.clase(s$class[i])
}
#dim(s)
#names(s)
#str(s)
plot3d(s$x, s$y, s$z, col = s$class)

## K means
s.kmeans <- k.means3D(dataset = s, centers = 2)
s.kmeans.CM <- table(s.kmeans$cluster, s$class)
s.kmeans.accuracy <- sum(diag(s.kmeans.CM))/sum(s.kmeans.CM)



### ***** plot kmeans 
plot3d(s$x, s$y, s$z, col = s.kmeans$cluster, main = "K-means")
rgl.spheres(s.kmeans$centers[, c("x", "y", "z")], r = 0.1, color = 1:2) 
###



## Partitioning Around Medioids (PAM)
s.pam <- pam(s[,1:3], 2)
s.pam.CM <- table(s.pam$clustering, s$class)
s.pam.accuracy.CM <- sum(diag(s.pam.CM))/sum(s.pam.CM)

### ***** plot PAM
plot3d(s$x, s$y, s$z, col = s.pam$clustering, main = "PAM")
rgl.spheres(s.pam$medoids[, c("x", "y", "z")], r = 0.1, color = 1:2) 
###


#### Comparison of kmean's centroids vs pam's medioids
s.kmeans.vs.pam <- compare.kmeans.pam(s.kmeans.accuracy, s.pam.accuracy.CM)


# Medioids are part of the DataSet, centroids aren't necessarily

## Hcluster
s.num <- s # a copy of the dataframe
s.num$class <-NULL # Delete class column
s.num <- as.matrix(s.num) # convert into a matrix

# Calculating each distance method vs each hclust method 
s.hclust <- match.hclust(s.num, 2, s)
  

####################################################################

#### ****** plot HCLUST
s.dist.mat <- dist(s.num, method = s.hclust[1]) # distance matrix
s.cluster <- hclust(s.dist.mat, method = s.hclust[2]) # apply method
s.ct <- cutree(s.cluster, k =2) # k to generate 4 clusters
s.dendrogram <- as.dendrogram(s.cluster)
plot(s.dendrogram) # dendrogram
rect.hclust(s.cluster, k = 2, border = c("red"))
plot3d(s$x, s$y, s$z, col = s.ct, main = "HCluster")
#######################################################################################


# Comparison hclust vs kmeans.vs.pam
if (s.hclust[3] > s.kmeans.vs.pam[2]){
  s.final.cluster <- s.hclust.better.accuracy
  s.dist.mat <- dist(s.num, method = s.hclust[1]) # distance matrix
  s.cluster <- hclust(s.dist.mat, method = s.hclust[2]) # apply method
  s.ct <- cutree(s.cluster, k =2) # k to generate 4 clusters
  s.dendrogram <- as.dendrogram(s.cluster.better)
  plot(s.dendrogram) # dendrogram
  rect.hclust(s.cluster.better, k = 2, border = c("red"))
  s.corte <- cut(s.dendrogram, h=0.21)$upper # $upper to get useful information instead a forest
  plot(s.corte)
  rgl.open()
  rgl.bg(color = "white") # Setup the background color
  plot3d(s$x, s$y, s$z, col = s.ct, main = "HCluster")
}else{
  s.final.cluster <- s.kmeans.vs.pam
  if (s.kmeans.vs.pam[1] == 'pam'){
    rgl.open()
    rgl.bg(color = "white") # Setup the background color
    plot3d(s$x, s$y, s$z, col = s.pam$clustering, main = "PAM")
    rgl.spheres(s.pam$medoids[, c("x", "y", "z")], r = 0.2, color = 1:2) 
  }else{
    rgl.open()
    rgl.bg(color = "white") # Setup the background color
    plot3d(s$x, s$y, s$z, col = s.kmeans$cluster, main = "K-means")
    rgl.spheres(s.kmeans$centers[, c("x", "y", "z")], r = 0.2, color = 1:2) 
  }
}

################################### END s.csv ##############################################



################################### ***** guess.csv ***** ###################################

## Exploratory Analysis
guess <- read.csv("guess.csv")
summary(guess)
names(guess)[1] <- "x"
names(guess)[2] <- "y"
plot(guess$x, guess$y) 


c.jambu(guess)


## K means

guess.kmeans <- kmeans(guess[,c("x", "y")], centers = 5)


### ***** plot kmeans 
plot(guess$x, guess$y, col= guess.kmeans$cluster, main = "K-means")
points(guess.kmeans$centers[, c("x", "y")],
       col=1:5,
       pch = 19,
       cex = 3)
###


## Partitioning Around Medioids (PAM)
guess.pam <- pam(guess[,1:2], 5)

### ***** plot PAM
plot(guess$x, guess$y, col = guess.pam$clustering, main = "PAM")
points(guess.pam$medoids,
       col=1:5,
       pch = 18,
       cex = 3)
###


## Hcluster
guess.num <- guess # a copy of the dataframe
guess.num <- as.matrix(guess.num) # convert into a matrix

# Calculating hclust methods
######### Euclidean - Single (Worst)
guess.dist.mat <- dist(guess.num, method = dist_methods[1]) # distance matrix
guess.cluster <- hclust(guess.dist.mat, method = hclust_methods[2]) # apply method
guess.ct <- cutree(guess.cluster, k = 5) # to generate k clusters

dendrogram <- as.dendrogram(guess.cluster)
plot(dendrogram)
rect.hclust(guess.cluster, k = 5, border = c("red"))
plot(guess$x, guess$y, col= guess.ct, main = "HCluster (Euclidean - Single)")

########## Euclidean - Complete (good enought)
guess.dist.mat <- dist(guess.num, method = dist_methods[1]) # distance matrix
guess.cluster <- hclust(guess.dist.mat, method = hclust_methods[3]) # apply method
guess.ct <- cutree(guess.cluster, k = 5) # to generate k clusters

dendrogram <- as.dendrogram(guess.cluster)
plot(dendrogram)
rect.hclust(guess.cluster, k = 5, border = c("red"))
plot(guess$x, guess$y, col= guess.ct, main = "HCluster (Euclidean - Complete)")

############ Euclidean - Average (so so bad)
guess.dist.mat <- dist(guess.num, method = dist_methods[1]) # distance matrix
guess.cluster <- hclust(guess.dist.mat, method = hclust_methods[4]) # apply method
guess.ct <- cutree(guess.cluster, k = 5) # to generate k clusters

dendrogram <- as.dendrogram(guess.cluster)
plot(dendrogram)
rect.hclust(guess.cluster, k = 5, border = c("red"))
plot(guess$x, guess$y, col= guess.ct, main = "HCluster (Euclidean - Average)")

############# Manhattan - Single (BAD)
guess.dist.mat <- dist(guess.num, method = dist_methods[3]) # distance matrix
guess.cluster <- hclust(guess.dist.mat, method = hclust_methods[2]) # apply method
guess.ct <- cutree(guess.cluster, k = 5) # to generate k clusters

dendrogram <- as.dendrogram(guess.cluster)
plot(dendrogram)
rect.hclust(guess.cluster, k = 5, border = c("red"))
plot(guess$x, guess$y, col= guess.ct, main = "HCluster (Manhattan - Single)")


############ Manhattan - Complete
guess.dist.mat <- dist(guess.num, method = dist_methods[3]) # distance matrix
guess.cluster <- hclust(guess.dist.mat, method = hclust_methods[3]) # apply method
guess.ct <- cutree(guess.cluster, k = 5) # to generate k clusters

dendrogram <- as.dendrogram(guess.cluster)
plot(dendrogram)
rect.hclust(guess.cluster, k = 5, border = c("red"))
plot(guess$x, guess$y, col= guess.ct, main = "HCluster (Manhattan - Complete)")

########### Manhattan - Average (so so BAD)
guess.dist.mat <- dist(guess.num, method = dist_methods[3]) # distance matrix
guess.cluster <- hclust(guess.dist.mat, method = hclust_methods[4]) # apply method
guess.ct <- cutree(guess.cluster, k = 5) # to generate k clusters

dendrogram <- as.dendrogram(guess.cluster)
plot(dendrogram)
rect.hclust(guess.cluster, k = 5, border = c("red"))
plot(guess$x, guess$y, col= guess.ct, main = "HCluster (Manhattan - Average)")


################################### END guess.csv ##############################################



################################### ***** help.csv ***** ###################################
# 3 MAS PROBABLE
help.clase = function(numero){
  # Selecting 3 clusters
  if(numero < -1.0)
    return(1)
  else if(numero < 7.0)
    return(2)
  else
    return(3)
} 


## Exploratory Analysis
help <- read.csv("help.csv")
summary(help)
names(help)[1] <- "x"
names(help)[2] <- "y"
names(help)[3] <- "z"
names(help)[4] <- "class"

for (i in 1:length(help$class)){
  help$class[i] <- help.clase(help$class[i])
}

plot3d(help$x, help$y, help$z, col = help$class)


help$class = 1
help$class[help$x < 10] = 2
help$class[help$x > 40] = 3

plot3d(help$x, help$y, help$z, col = help$class)

## K means ############################
help.kmeans <- k.means3D(dataset = help, centers = 3)
help.kmeans.CM <- table(help.kmeans$cluster, help$class)
help.kmeans.accuracy <- sum(diag(help.kmeans.CM))/sum(help.kmeans.CM)


### ***** plot kmeans 
plot3d(help$x, help$y, help$z, col = help.kmeans$cluster, main = "K-means")
rgl.spheres(help.kmeans$centers[, c("x", "y", "z")], r = 0.8, color = 1:3) 
###



## Partitioning Around Medioids (PAM)
help.pam <- pam(help[,1:3], 3)
help.pam.CM <- table(help.pam$clustering, help$class)
help.pam.accuracy.CM <- sum(diag(help.pam.CM))/sum(help.pam.CM)

### ***** plot PAM
plot3d(help$x, help$y, help$z, col = help.pam$clustering, main = "PAM")
rgl.spheres(help.pam$medoids[, c("x", "y", "z")], r = 0.8, color = 1:3) 
###


#### Comparison of kmean's centroids vs pam's medioids

## Hcluster
help.num <- help # a copy of the dataframe
help.num$class <-NULL # Delete class column
help.num <- as.matrix(help.num) # convert into a matrix

# Calculating each distance method vs each hclust method 

help.hclust = match.hclust(help.num, 3, help)

#### ****** plot HCLUST
help.dist.mat <- dist(help.num, method = help.hclust[1]) # distance matrix
help.cluster <- hclust(help.dist.mat, method = help.hclust[2]) # apply method
help.ct <- cutree(help.cluster, k =3) # k to generate 3 clusters
help.dendrogram <- as.dendrogram(help.cluster)
plot(help.dendrogram) # dendrogram
rect.hclust(help.cluster, k = 3, border = c("red"))
plot3d(help$x, help$y, help$z, col = help.ct, main = "HCluster")
######




# Comparison hclust vs kmeans.vs.pam
if (help.hclust[3] > h.kmeans.vs.pam[2]){
  h.final.cluster <- h.hclust.better.accuracy
  h.dist.mat <- dist(h.num, method = h.hclust[1]) # distance matrix
  h.cluster <- hclust(h.dist.mat, method = h.hclust[2]) # apply method
  h.ct <- cutree(h.cluster, k =5) # k to generate 3 clusters
  h.dendrogram <- as.dendrogram(h.cluster.better)
  plot(h.dendrogram) # dendrogram
  rect.hclust(h.cluster.better, k = 5, border = c("cyan"))
  h.corte <- cut(h.dendrogram, h=16)$upper # $upper to get useful information instead a forest
  plot(h.corte)
  rgl.open()
  rgl.bg(color = "white") # Setup the background color
  plot3d(h$x, h$y, h$z, col = h.ct, main = "HCluster")
}else{
  h.final.cluster <- h.kmeans.vs.pam
  if (h.kmeans.vs.pam[1] == 'pam'){
    rgl.open()
    rgl.bg(color = "white") # Setup the background color
    plot3d(h$x, h$y, h$z, col = h.pam$clustering, main = "PAM")
    rgl.spheres(h.pam$medoids[, c("x", "y", "z")], r = 0.4, color = 1:5) 
  }else{
    rgl.open()
    rgl.bg(color = "white") # Setup the background color
    plot3d(h$x, h$y, h$z, col = h.kmeans$cluster, main = "K-means")
    rgl.spheres(h.kmeans$centers[, c("x", "y", "z")], r = 0.4, color = 1:5) 
  }
}




################################### END help.csv ##############################################





################################### ***** good_luck.csv ***** ################################### 

## Exploratory Analysis
good_luck <- read.csv("good_luck.csv")
summary(good_luck)
names(good_luck)[1] <- "a"
names(good_luck)[2] <- "b"
names(good_luck)[3] <- "c"
names(good_luck)[4] <- "d"
names(good_luck)[5] <- "e"
names(good_luck)[6] <- "f"
names(good_luck)[7] <- "g"
names(good_luck)[8] <- "h"
names(good_luck)[9] <- "i"
names(good_luck)[10] <- "j"
names(good_luck)[11] <- "class"
good_luck <- set_class(good_luck)
good_luck.columns <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
plot(good_luck[,1:10], col = good_luck$class, main = "Vista Previa")
## K means

res <- good_luck.kmeans.fun(good_luck.columns, good_luck)

## Partitioning Around Medioids (PAM)
good_luck.pam <- pam(good_luck[,1:10], 2)
good_luck.pam.CM <- table(good_luck.pam$clustering, good_luck$class)
good_luck.pam.accuracy.CM <- sum(diag(good_luck.pam.CM))/sum(good_luck.pam.CM)

plot(good_luck, col = good_luck.pam$clustering, main = "PAM")
points(good_luck.pam$medoids, col=1:2, pch = 18, cex = 3)


good_luck.num <- good_luck # a copy of the dataframe
good_luck.num$class <-NULL # Delete class column
good_luck.num <- as.matrix(good_luck.num) # convert into a matrix

good_luck.hclust = match.hclust(good_luck.num, k = 2, good_luck)

#### ****** plot HCLUST
good_luck.dist.mat <- dist(good_luck.num, method = good_luck.hclust[1]) # distance matrix
good_luck.cluster <- hclust(good_luck.dist.mat, method = good_luck.hclust[2]) # apply method
good_luck.ct <- cutree(good_luck.cluster, k =2) # k to generate k clusters
dendrogram <- as.dendrogram(good_luck.cluster)
plot(dendrogram)
rect.hclust(good_luck.cluster, k = 2, border = c("red"))

plot(good_luck, col= good_luck.ct, main = "HCluster")



################################### END good_luck.csv ##############################################






###################################***** a_big.csv ***** ###################################

## Exploratory Analysis
a_big <- read.csv("a_big.csv")
summary(a_big)
names(a_big)[1] <- "x"
names(a_big)[2] <- "y"
names(a_big)[3] <- "class"
a_big <- set_class(a_big)
plot(a_big$x, a_big$y, col=a_big$class, main = "Vista Previa") 


## K means
a_big.kmeans <- k.means2D(dataset = a_big, centers = 3)
a_big.kmeans.CM <- table(a_big.kmeans$cluster, a_big$class)
a_big.kmeans.accuracy <- sum(diag(a_big.kmeans.CM))/sum(a_big.kmeans.CM)


### ***** plot kmeans 
plot(a_big$x, a_big$y, col= a_big.kmeans$cluster, main = "K-means")
points(a_big.kmeans$centers[, c("x", "y")],
       col=0,
       pch = 19,
       cex = 1)
###

#a_big.sample <- a_big[sample(nrow(a_big), 20, replace = F), ] # taking __% of total dim

prob_vec = vector(mode = "numeric", length = nrow(a_big))
prob_vec[a_big[,"class"] == 1] = 1/sum(a_big[, "class"] == 1)
prob_vec[a_big[,"class"] == 2] = 1/sum(a_big[, "class"] == 2)
prob_vec[a_big[,"class"] == 3] = 1/sum(a_big[, "class"] == 3)

a_big.sample = sample(x = nrow(a_big), size = floor(nrow(a_big) * 0.1), prob = prob_vec, replace = F)
a_big.subset <- a_big[a_big.sample, ]
a_big.subset.kmeans <- myKmeans(a_big.subset, k = 3, nItter = 10)
plot(a_big.subset$x, a_big.subset$y, col = a_big.subset.kmeans$clusters, main = " Kmedias Personalizado ")
points(a_big.subset.kmeans$centroids[,c("x", "y")],
       col = 0,
       pch = 19,
       cex = 2)


a_big.final = myKmeans(dataframe = a_big
                       ,k = 3
                       ,nItter = 10
                       ,centroids = a_big.subset.kmeans$centroids)
a_big.final.CM <- table(a_big.final$cluster, a_big$class)
a_big.final.kmeans.accuracy <- sum(diag(a_big.final.CM))/sum(a_big.final.CM)

plot(a_big$x, a_big$y, col = a_big.final$clusters, main = " Kmedias Personalizado FINAL")
points(a_big.final$centroids[,c("x", "y")],
       col = 0,
       pch = 19,
       cex = 2)


################################### END a_big.csv ##############################################


