library(tidyverse)
library(VIM)
library(corrplot)
library(car)
library(EnvStats)
library(mlbench)
library(ggplot2)
library(tidyverse)
library(mice)
library(VIM)
library(forcats)
library(caret)
library(dplyr)
library(grid)
library(class)
library(cluster)
library(factoextra)
library(kohonen)
library(pROC)
library(ROCR)
library(rgl) 
library(useful)
library(dbscan)
library(NbClust)
#read csv file
Data <- read.csv(file="Wells.csv", header=TRUE)
Data %>% dplyr::select(-c("PEF2","Vquartz","Vsh","Vcarb"))%>% glimpse()
#select lithology parameters 
data_litho <- Data %>% dplyr::select(c("Well","GR","DT","RHOB")) %>% drop_na()%>% glimpse()
data_litho <- data_litho[data_litho["Well"] <= 6,] #6 wells are surveyed in this assignment
######################################################
#K-Means Methods
#scaling/ standardizing data
litho.scaled<-scale(data_litho[,c(2:4)])

#determine number of clusters 
  #create our own plot function to look for "Within cluster Sum of square error 'elbow' plot"
  #defaults to 15 as clusters max
wssplot <- function(data, nc=15){                    
  
  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
    kclus <- kmeans(data, centers=k)
    wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}
wssplot(litho.scaled,nc=15)
# "Hartigan's rule is one statistical technique to "find" the elbow
clusFit<-FitKMeans(litho.scaled,max.clusters=30,nstart=20)   #evaluates k using the "Hartigan" rule
PlotHartigan(clusFit)

# run KMeans
set.seed(3000)
kmeans.litho <- kmeans(litho.scaled,3, nstart=10)     
fviz_cluster(kmeans.litho, data =litho.scaled )
#clusters results
kmeans.litho$centers  # the centroids of the final clusers (remember, these are scaled)
kmeans.litho$size #and the size of each cluster

#stored clusters 
#check classification with lithology parameters 
data_litho$Facies <- as.factor(kmeans.litho$cluster)

ggplot(data_litho,aes(x=Facies,y=GR,fill=Facies)) + geom_boxplot() + facet_wrap(~Facies,scale="free") +
  scale_y_continuous(limits = c(0,250))
ggplot(data_litho,aes(x=Facies,y=DT,fill=Facies)) + geom_boxplot() + facet_wrap(~Facies,scale="free") +
  scale_y_continuous(limits=c(40,170))
ggplot(data_litho,aes(x=Facies,y=RHOB,fill=Facies)) + geom_boxplot() + facet_wrap(~Facies,scale="free") +
  scale_y_continuous(limits=c(1.8,3.5))
#to visualize this 4d data in 3d let's use PCA, and then color code base on clusters
pc <- princomp(data_litho[,2:4], cor=TRUE, scores=TRUE)
plot3d(pc$scores[,1:3], size=5, col=data_litho$Facies, main="k-means clusters")
#summarization
data_litho %>% group_by(Facies) %>% summarize(GR=mean(GR),DT=mean(DT),RHOB=mean(RHOB))


##########################################################################
#K Medoids
data_litho_kMedoids <- data_litho %>% dplyr::select(c("Well","GR","DT","RHOB")) %>% drop_na()%>% glimpse()
litho.scaled.kMedoids <-data.frame(scale(data_litho_kMedoids[,c(2:4)]))
#determine number of clusters 
#create our own plot function to look for "Within cluster Sum of square error 'elbow' plot"
#defaults to 15 as clusters max
wssplot1 <- function(data, nc=15){                    
  
  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
    kclus <- pam(data, centers=k)
    wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}
wssplot(litho.scaled.kMedoids,nc=15)
set.seed(1000)
FaciesPAM <- pam(litho.scaled.kMedoids, 3)
FaciesPAM$medoids
print(FaciesPAM)
fviz_cluster(FaciesPAM, data =litho.scaled.kMedoids )
fviz_nbclust(data_litho_kMedoids, pam, method = "silhouette")
PAM <- data.frame(FaciesPAM$medoids)

#Center comparison for PAM
p<-qplot(data=litho.scaled.kMedoids, x=RHOB, y=DT, color=factor(FaciesPAM$clustering))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p <- p+guides(coloPr = g, size = g, shape = g)    #retitle the legend...
p <- p + geom_point(data=PAM, aes(x=RHOB, y=GR), size=5,colour="black")
p
#center visualization for KMeans and comparison
KM<- data.frame(kmeans.litho$centers)
p <- p+ geom_point(data=KM, aes(x=RHOB, y=DT), size=5,colour="blue")
p

data_litho$FaciesPAM <- as.factor(FaciesPAM$clustering)

confusionMatrix(data_litho$FaciesPAM,data_litho$Facies)

############################################################################
#Dbscan
data_litho_Dbscan <- data_litho %>% dplyr::select(c("Well","GR","DT","RHOB")) %>% drop_na()%>% glimpse()
litho.matrix1 <- as.matrix(data_litho_Dbscan[,c(2:4)])
#Fast calculation of the k-nearest neighbor distances in a matrix of points. 
#Can be used to help find a suitable value for the eps neighborhood for DBSCAN. 
kNNdist(litho.matrix1, k=4, search="kd")
kNNdistplot(litho.matrix1, k=4)
abline(h=9, col="red")
#Look for the knee in the plot. Distance ~ 3
db <- dbscan(litho.matrix1, eps = 9, minPts = 4)
pairs(litho.matrix1, col = db$cluster+1L)
hullplot(litho.matrix1, db$cluster)
table(db$cluster)


