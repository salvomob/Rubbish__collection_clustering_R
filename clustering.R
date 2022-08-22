# installazione pacchetti necessari

install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

# loading delle librerie necessarie

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


###################
#                 #
#                 #
#    MALVAGNA     #
#                 #
#                 #
###################


data <- read.table(file="C:/Users/Utente/OneDrive/Desktop/rubbish collection clustering/Mappa/coordinate.txt",header=T,sep="\t")

#best k -> WSS plot Function

wssplot <- function(data, nc=15, seed = 1234)
{
  wss<- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i) $withinss)
  }
  plot(1:nc,wss,type = "b" , xlab = "Numero di Cluster",ylab = "Gruppi di somma di quadrati")
}

#Scelta miglior k per l'algoritmo di clustering

wssplot(data)

#Applicazione del K-means con il miglior valore di k scelto in precedenza

KM = kmeans(data,3)

#valutazione

autoplot(KM,data,frame=T)

KM2 = kmeans(data,4)
autoplot(KM2,data,frame=T)

KM3 = kmeans(data,15)
autoplot(KM3,data,frame=T)


#Procediamo con il dbscan
#Installiamo i pacchetti necessari

install.packages("fpc")
install.packages("mclust")
install.packages("dbscan")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(cluster)

library(fpc)
library(dbscan)
library(factoextra)

#Per un buon clustering, scegliamo il miglior valore di Epsilon

kNNdistplot(data,k = 3)
abline(h = 50,lty = 2)

#DBSCAN (1) FPC

set.seed(123)
f<- fpc::dbscan(data,eps=50,MinPts = 3)
fviz_cluster(f,data,geom = "point")


#coefficienti di silhouette Malvagna

windows()
dis = dist(data,"euclidean")
s = silhouette(f$cluster,dis)
plot(s)
s1= silhouette(KM$cluster,dis)
plot(s1)
s2= silhouette(KM2$cluster,dis)
plot(s2)
s3= silhouette(KM3$cluster,dis)
plot(s3)

###################
#                 #
#                 #
# MOIO ALCANTARA  #
#                 #
#                 #
###################


#K-Means -> scelta di k ottimale e applicazione algoritmo

data2 <- read.table(file="C:/Users/Utente/OneDrive/Desktop/rubbish collection clustering/MappaMoio/coordinate.txt",header=T,sep="\t")
wssplot(data2)

KM_Moio = kmeans(data2,3)
autoplot(KM_Moio,data2,frame=T)
KM_Moio2 = kmeans(data2,4)
autoplot(KM_Moio2,data2,frame=T)


#DBSCAN con scelta del miglior Epsilon

kNNdistplot(data2,k = 3)
abline(h = 100,lty = 2)
set.seed(123)
f1<- fpc::dbscan(data2,eps=100,MinPts = 3)
fviz_cluster(f1,data2,geom = "point")


 #Coefficienti di silhouette di Moio Alcantara

windows()
dis1 = dist(data2,"euclidean")
s_ = silhouette(f1$cluster,dis1)
plot(s_)
s1_ = silhouette(KM_Moio$cluster,dis1)
plot(s1_)
s2_ = silhouette(KM_Moio2$cluster,dis1)
plot(s2_)



########################
#                      #
#                      #
#  ROCCELLA VALDEMONE  #
#                      #
#                      #
########################



#K-Means -> scelta di k ottimale e applicazione algoritmo

data3 <- read.table(file="C:/Users/Utente/OneDrive/Desktop/rubbish collection clustering/MappaRoccella/coordinate.txt",header=T,sep="\t")
wssplot(data3)

KM_Roccella = kmeans(data3,3)
autoplot(KM_Roccella,data3,frame=T)

KM_Roccella2 = kmeans(data3,4)
autoplot(KM_Roccella2,data3,frame=T)

#DBSCAN con scelta del miglior Epsilon

kNNdistplot(data3,k = 3)
abline(h = 55,lty = 2)
set.seed(123)
f1R<- fpc::dbscan(data3,eps=55,MinPts = 3)
fviz_cluster(f1R,data3,geom = "point")


#Coefficienti di silhouette di Roccella

windows()
dis2 = dist(data3,"euclidean")
s_R = silhouette(f1R$cluster,dis2)
plot(s_R)
s1_R = silhouette(KM_Roccella$cluster,dis2)
plot(s1_R)
s2_R = silhouette(KM_Roccella2$cluster,dis2)
plot(s2_R)




############################
#                          #
#                          #
# SANTA DOMENICA VITTORIA  #
#                          #
#                          #
############################


#K-Means -> scelta di k ottimale e applicazione algoritmo

data4 <- read.table(file="C:/Users/Utente/OneDrive/Desktop/rubbish collection clustering/MappaSDV/coordinate.txt",header=T,sep="\t")
wssplot(data4)

KM_SDV = kmeans(data4,3)
autoplot(KM_SDV,data4,frame=T)

KM_SDV2 = kmeans(data4,4)
autoplot(KM_SDV2,data4,frame=T)


#DBSCAN con scelta del miglior Epsilon

kNNdistplot(data4,k = 3)
abline(h=70,lty=2)
set.seed(123)
f1SDV<- fpc::dbscan(data4,eps=75,MinPts = 3)
fviz_cluster(f1SDV,data4,geom = "point")

#Coefficienti di silhouette di Roccella

windows()
dis3 = dist(data4,"euclidean")
s_SDV = silhouette(f1SDV$cluster,dis3)
plot(s_SDV)
s1_SDV = silhouette(KM_SDV$cluster,dis3)
plot(s1_SDV)
s2_SDV = silhouette(KM_SDV2$cluster,dis3)
plot(s2_SDV)


