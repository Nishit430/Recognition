library("ggplot2")
library(viridis)
library(dplyr)
library(nnet)
set.seed(18031)

#Reading the data
rfmm <- read.csv("./Data/Reward Received to Analytics.csv")
head(rfmm)
str(rfmm)
rfmm$No..of.Times.Nominated <- as.numeric(rfmm$No..of.Times.Nominated)
colSums(is.na(rfmm))
aa <- rfmm
str(aa)

## Scaling data
rfmm <- rapply(rfmm, scale, class = c("numeric", "integer"), how = "replace")
str(rfmm)

## Finding the optimum number of clusters
nocl <- sum((nrow(rfmm[,c(-1,-6)])-1)*var(rfmm[,c(-1,-6)]))
for (i in 2:10) {
  nocl[i] <- kmeans(rfmm[,c(-1,-6)], centers = i, iter.max = 1000)$tot.withinss
}
nocl
plot(1:10, nocl, type="b", xlab = "no. of clusters", ylab = "tot within")

# 3 Clusters seem appropriate
ak <- kmeans(rfmm[,c(-1,-6)], centers = 3, iter.max = 1000, nstart = 25)
a1 <- cbind(aa, ak$cluster)
str(a1)

# Box plot showing nominations vs efforts. There doesn't seem to be any correlation. But let's check the  cluster wise
ggplot(aa, aes(y= Effort, x = as.factor(No..of.Times.Nominated), fill = as.factor(a1$No..of.Times.Nominated))) + geom_boxplot() + xlab("No. of nominations") + scale_fill_discrete(name = "")
# Boxplot showing the distribution of the cluster
ggplot(aa, aes(y= Effort, x = as.factor(No..of.Times.Nominated), fill = as.factor(a1$`ak$cluster`))) + geom_boxplot() + xlab("No. of nominations") + scale_fill_discrete(name = "Cluster")


# Checking if no .of nominations for clusters are different or not
tbl = table(a1$`ak$cluster`, a1$No..of.Times.Nominated)
tbl
chisq.test(tbl)
# There is no support that more usage of thanks can actually increase managerial effectiveness

# Another way to test using multinomal regression
# a1$`ak$cluster` <- relevel(as.factor(a1$`ak$cluster`), ref = "3")
# test <- multinom(a1$`ak$cluster` ~ a1$No..of.Times.Nominated)
# summary(test)
# z <- summary(test)$coefficients/summary(test)$standard.errors
# z
# p <- (1 - pnorm(abs(z), 0, 1))*2
# p # all p values are insignificant

clus1 <- subset(a1, a1$"ak$cluster"==1) ## This is read as subset of a1 where a1$column name is equal to 1
summary(clus1)## This is cluster 1 of people who have have never been nominated and still have a mid level score of manager recognition
nrow(clus1)
 
clus2 <- subset(a1, a1$`ak$cluster` == 2)
summary(clus2) # Similarly we can name other clusters based on observations
nrow(clus2)

clus3 <- subset(a1, a1$`ak$cluster` == 3)
summary(clus3)
nrow(clus3)

# Writing this to a csv
write.csv(a1, "./Output/kmeans4.csv")
