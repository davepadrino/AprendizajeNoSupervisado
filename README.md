# Aprendizaje No Supervisado
## Tarea #3

## David Padrino

# Tasks
## OJO con h y s .csv, seleccionar buen numero de cluster (h: 4; s:2 (casi seguro)) revisar hclust en s.csv
## function comparing kmeans vs pam
## start a_big
## Diferentes valores cada ejecucion de kmeans
## freeze R
## teoria de kmeans, hcust para explicar 


Se utilizó el SO Ubuntu 14.04.
Para el paquete Rcmdr puede dar cierto error con un elemento X11, para ello se recomienda ejecutar en el terminal de Ubuntu

sudo apt-get build-dep r-cran-rgl



install.packages("fpc")







### ***** plot PAM
plot(moon$x, moon$y, col = moon.pam$clustering, main = "PAM")
points(moon.pam$medoids,
       col=1:2,
       pch = 18,
       cex = 3)
###


#### ****** plot HCLUST
plot(moon.cluster.better) # dendrogram
rect.hclust(moon.cluster.better, k = 2, border = c("cyan"))
plot(moon$x, moon$y, col= moon.ct.better, main = "HCluster")
dendrogram <- as.dendrogram(moon.cluster.better)
corte <- cut(dendrogram, h=20)$upper # $upper to get useful information instead a forest
plot(corte)













