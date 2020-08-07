library("ggplot2")
library(viridis)
set.seed(18031)

#Reading the data
rfmm <- read.csv("C:\\Users\\nksay\\Desktop\\Reward Received to Analytics.csv")
head(rfmm)
rfmm$Effort <- as.numeric(as.character((rfmm$Effort)))
rfmm$Support <- as.numeric(as.character((rfmm$Support)))
rfmm$Feedback <- as.numeric(as.character((rfmm$Feedback)))
rfmm$X1.on.1 <- as.numeric(as.character((rfmm$X1.on.1)))
rfmm <-na.omit(rfmm)
aa <- rfmm
str(aa)
head(rfmm)
#rfmm$ï..Leader <- NULL
summary(rfmm)

## Scaling data
rfmm$Effort <- scale(rfmm$Effort)
rfmm$Support <- scale(rfmm$Support)
rfmm$Feedback <- scale(rfmm$Feedback)
rfmm$X1.on.1 <- scale(rfmm$X1.on.1)

## Finding the optimum number of clusters
nocl <- sum((nrow(rfmm[,c(-1,-3,-4,-5)])-1)*var(rfmm[,c(-1,-3,-4,-5)]))
for (i in 2:10) {
  nocl[i] <- kmeans(rfmm[,c(-1,-3,-4,-5)], centers = i, iter.max = 1000)$tot.withinss
}
nocl
plot(1:10, nocl, type="b", xlab = "no. of clusters", ylab = "tot within")

# We can take 4 to 6 cluster. Let's take 4 first and then try with5 and 6
ak <- kmeans(rfmm[,c(-1,-3,-4,-5)], centers = 6, iter.max = 1000, nstart = 25)
a1 <- cbind(aa, ak$cluster)
head(a1)

# Boxplot showing the distribution of the cluster
ggplot(aa, aes(y= Effort, x = as.factor(No..of.Times.Nominated), fill = as.factor(a1$`ak$cluster`))) + geom_boxplot() + xlab("No. of nominations") + scale_fill_discrete(name = "Cluster")

# Another visual representation of the clusters this time more clearly 
ggplot(aa, aes(y= Effort, x = No..of.Times.Nominated, col = as.factor(a1$`ak$cluster`))) + geom_point()+ xlab("No. of nominations") + scale_color_discrete(name = "Cluster")


clus1 <- subset(a1, a1$"ak$cluster"==1) ## This is read as subset of a1 where a1$column name is equal to 1
summary(clus1)## This is cluster 1 of people who have have never been nominated and still have a mid level score of manager recognition
nrow(clus1)
 
clus2 <- subset(a1, a1$`ak$cluster` == 2)
summary(clus2) # Similarly we can name other clusters based on observations
nrow(clus2)

clus3 <- subset(a1, a1$`ak$cluster` == 3)
summary(clus3)
nrow(clus3)

clus4 <- subset(a1, a1$`ak$cluster` == 4)
summary(clus4)
nrow(clus4)

# Writing this to a csv
write.csv(a1, file = "kmeans6")
getwd()
