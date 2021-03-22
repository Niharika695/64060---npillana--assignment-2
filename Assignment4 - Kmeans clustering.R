library(tidyverse)
library(factoextra)
library(cluster)
library(NbClust)
library(ISLR)
library(ggplot2)

setwd("C:/Users/nihar/Desktop")
Pharmadata<- read.csv("Pharmaceuticals.csv", header = TRUE)
str(Pharmadata)

#To remove any missing value that might be present in the data

P1 <- na.omit(Pharmadata)

# Collecting numerical variables from column 1 to 9 to cluster 21 firms
P1<- Pharmadata[, 3:11]
head(P1)

#Scaling the data using Scale function
set.seed(123)
dataframe<- scale(P1)
head(dataframe)

#Determining optimal clusters using Elbow method

library(ggplot2)
dev.off()
distance<- dist(dataframe, method = "euclidean")# for calculating distance matrix between rows of a data matrix.
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))# Visualizing a distance matrix

#Computing K-means clustering in R for different centers
#Using multiple values of K and examine the differences in results
kmeans <- kmeans(dataframe, centers = 2, nstart = 30)
kmeans1<- kmeans(dataframe, centers = 5, nstart = 30)
kmeans2<- kmeans(dataframe, centers = 6, nstart = 30)
str(kmeans)
kmeans

Plot1<-fviz_cluster(kmeans, data = dataframe)+ggtitle("k=2")
plot2<-fviz_cluster(kmeans1, data = dataframe)+ggtitle("k=5")
plot3<-fviz_cluster(kmeans2, data = dataframe)+ggtitle("k=6")

library(gridExtra)
grid.arrange(Plot1,plot2,plot3, nrow = 2)

#For each k, calculate the total within-cluster sum of square (wss)
set.seed(123)
wss<- function(k){
  kmeans(dataframe, k, nstart =10)$tot.withinss
}# tot.withinss is total within-cluster sum of squares

#Compute and plot wss for k = 1 to k = 10
k.values<- 1:10 

#extract wss for 2-15 clusters
wss_clusters<- map_dbl(k.values, wss)

graphics.off()
plot(k.values, wss_clusters,
 type="b", pch = 16, frame = TRUE, 
xlab="Number of clusters",
ylab="Total within-clusters sum of squares") #The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters k =5.

#k-Means cluster Analysis -  Fit the data with clusters
#Final analysis and Extracting results using 5 clusters

set.seed(123)
final<- kmeans(dataframe, 5, nstart = 25)
print(final)
fviz_cluster(final, data = dataframe)#Visualize the results

P1%>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster)%>% summarise_all("mean")

library(cluster)
clusplot(dataframe,final$cluster, color = TRUE,shade = TRUE, labels = 2,lines = 0)

#b) Interpret the clusters with respect to the numerical variables used in forming the clusters
#Cluster 1 - Row 6,8,12
#Cluster 2 - Row 9,14
#Cluster 3 - Row 2,18
#Cluster 4 - Row 1,3,5,10,16,19,20
#Cluster 5 - Row 4,7,15,13,11,17,21

#Obsercations for each clusters
#cluster1 have highest Market_cap, highest ROE and lowest Leverage
#Cluster2 have the lowest Market_cap and highest Leverage
#Cluster 3 have the high PE_ratio, highest Rev_Growth and Low ROE
#Cluster4 have the highest Market_cap, highest ROE, highest ROA, highest Rev_Growth and low Leverage
#Cluster5 have low Market_cap and high Rev_Growth

#c)Is there a pattern in the clusters with respect to the numerical variables (10 to 12)? (those not used in forming the clusters)
#observations as per the Median_Recommendation
#Cluster1 with highest Market_Cap and Lowest Leverage suggests a Moderate Buy
#Cluster2 with highest leverage and lowest Market_cap suggests a Moderate Sell
#Cluster3 with high PE ratio, Rev_Growth and low ROE suggests a Moderate Buy
#Cluster4 with high Market cap, ROE, ROA, Rev_Growth and low Leverage suggests Hold
#Cluster5 with high Rev_Growth and Low Market_Cap suggests Moderate Sell

#Pattern 
#Cluster 1 and 3 recommends Moderate Buy
#Cluster 2 and 5 recommends Moderate Sell
#Cluster 3 recommends Hold


#d)Provide an appropriate name for each cluster using any or all of the variables in the dataset.
#Cluster1-Growth Cluster
#Cluster2-Leverage Cluster
#Cluster3-Low_ROE Cluster
#Cluster4-Low Market_cap Cluster
#Cluster5-REv_Growth Cluster

