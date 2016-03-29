install.packages("clue")
library("stringr")

 
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

## Partitioning Around Medioids 
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
a.num <- a # a copy of the dataframe
a.num$class <-NULL # Delete class column
a.num <- as.matrix(a.num) # convert into a matri
a.dist.mat <- dist(a.num) # distance matrix

a.cluster <- hclust(a.dist.mat, method = "single")
plot(a.cluster) # dendrogram
ct <- cutree(a.cluster, k =3) # k to generate 3 clusters
plot(ct)

ct1 <- cutree(a.cluster, h =1) # h para la altura
plot(a$x, a$y, col = cutree(a.cluster, h =1))

table(ct, a$class)

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












