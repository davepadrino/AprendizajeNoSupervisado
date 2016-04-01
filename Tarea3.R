# install.packages("clue")
library("stringr")
library("caret")
 
##### ***** a.csv ***** #####

## Exploratory Analysis
a <- read.csv("a.csv")
names(a)[1] <- "x"
names(a)[2] <- "y"
names(a)[3] <- "class"
dim(a)
names(a)
str(a)
summary(a)
hist(a$x)
plot(a$x, a$y, col=1:3) 

## K means
a.kmeans <- kmeans(a[,1:2], centers = 3)
plot(a$x, a$y, col= a.kmeans$cluster)
points(a.kmeans$centers[, c("x", "y")],
       col=1:3,
       pch = 19,
       cex = 2)
table(a.kmeans$cluster, a$class)

## Partitioning Around Medioids (PAM)
a.pam <- pam(a[,1:2], 3)
plot(a$x, a$y, col = a.pam$clustering)
points(a.pam$medoids,
       col=1:3,
       pch = 18,
       cex = 2)

#### Comparison of kmean's centroids vs pam's medioids
a.kmeans$centers
a.pam$medoids # Medioids are part of the DataSet



## Hcluster
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
dist_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

a.num <- a # a copy of the dataframe
a.num$class <-NULL # Delete class column
a.num <- as.matrix(a.num) # convert into a matri

# Calculating each distance method vs each hclust method 
better.accuracy <- 0
for (i in 1:length(dist_methods)){
  for (j in 1:length(hclust_methods)){
    a.dist.mat <- dist(a.num, method = dist_methods[i]) # distance matrix
    a.cluster <- hclust(a.dist.mat, method = hclust_methods[j]) # apply method
    ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
    confusion.Matrix <- table(as.factor(a$class), as.factor(ct))
    accuracy.CM <- sum(diag(confusion.Matrix))/sum(confusion.Matrix)
    if (accuracy.CM > better.accuracy){
      better.methods <- c(dist_methods[i], hclust_methods[j])
      better.accuracy <- accuracy.CM
    }
  }
}



plot(a.cluster) # dendrogram
plot(a$x, a$y, col= ct)


ct1 <- cutree(a.cluster, h =15) # h para la altura
plot(a$x, a$y, col = ct1)
table(a$class, ct1)


dendrogram <- as.dendrogram(a.cluster)
plot(dendrogram)
rect.hclust(a.cluster, k = 3, border = c("cyan"))
# Cortar el dendrograma por encima
corte <- cut(dendrogram, h=1)$upper
plot(corte)



## Confussion Matrix


##### ***** moon.csv ***** #####

## Exploratory Analysis
moon <- read.csv("moon.csv")
dim(moon)
names(moon)
str(moon)
summary(moon)


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












