
## Clustering and PCA ##
## setting working directory
setwd("C:/Users/panyidoh/Desktop/Advanced Data Analysis_Project 1/Project 2_3")
install.packages("tidyverse") # data manipulation #
install.packages("cluster")  
install.packages("stats") # clustering algorithms #
install.packages("factoextra") # clustering visualization
install.packages("dendextend") # for comparing two dendrograms
df <- USArrests
View(USArrests)
data=read.csv(file.choose(),header=TRUE)
attach(data)
View(data)
data1=(data [,2:23])
View(data1)
rownames(data1)=data$X
View(rownames(data1))
View(data1)
data2=scale(data1)
class(Slab)
View(data2)
## Dissimilarity matrix ##
d=dist(data2,method="euclidean")

# Hierarchical clustering using Average Linkage #
hc1=hclust(d, method="average")

# Plot the obtained dendrogram #
plot(hc1, cex = 0.5, hang = -5,main="DENDOGRAM FOR COUNT OF STRUCTURE TYPES (2013) ")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k =4)

# Number of members in each cluster
table(sub_grp)
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)
## Cluster plot ##
fviz_cluster(list(data = data2, cluster = sub_grp,size=0.5))+theme(plot.title=element_text(hjust=0.5))
## Optimal number of Clusters ##
fviz_nbclust(data2, FUN = hcut, method = "wss")+theme(plot.title=element_text(hjust=0.5))

fviz_nbclust(data2, kmeans, method = "silhouette")+theme(plot.title=element_text(hjust=0.5))
 
## K means Clustering ##
k4 <- kmeans(data2, centers = 4, nstart = 25)
str(k4)
k4
fviz_cluster(k4, data = data2)


## PCA ##
pc=princomp(data2,cor=TRUE,score=TRUE)
summary (pc)
plot(pc)
plot(pc, type="l")
biplot(pc)
attributes(pc)
pc$loadings
pc$scores



## pca #
library("gridExtra")
# compute variance of each variable
apply(data1, 2, var)
View(data1)
# create new data frame with centered variables
scaled_df <- apply(data1, 2, scale)
head(scaled_df)
# Calculate eigenvalues & eigenvectors
bridge.cov <- cov(scaled_df)
bridge.eigen <- eigen(bridge.cov)
str(bridge.eigen)
summary(bridge.eigen)
plot(bridge.eigen$values,PVE,type="l",main="Importance of Eigen Values",ylab="Proportion of Variance Explained",xlab="Eigen Values")
## Built in function ##
pca_result <- prcomp(data2, scale = TRUE)
names(pca_result)
pca_result$rotation
pca_result$rotation1=-pca_result$rotation
pca_result$rotation1
biplot(pca_result,scale=0)
pca_result$sdev
# variance explained #
(VE <- pca_result$sdev^2)
#proportion of variance explained by each principal component#
VE <- VE / sum(VE)
round(PVE, 2)
library(ggplot2)
plot(PVE, type="l",main="Varience Plot",xlab="Principal Components", ylab="Proportion of Variance explained",col="blue")
summary(pca_result)
plot(bridge.eigen$values,PVE,type="line")
bridge.eigen$values
