library("ggplot2")
library(wesanderson)
library(dplyr)
set.seed(18031)

#Reading the data
rfmm <- read.csv("./Data/ManagerNominations.csv")
head(rfmm)
str(rfmm)
colSums(is.na(rfmm))
aa <- rfmm
str(aa)

## Scaling data
rfmm <- rapply(rfmm, scale, class = c("numeric", "integer"), how = "replace")
str(rfmm)

## Finding the optimum number of clusters
nocl <- sum((nrow(rfmm[,c(-1,-6,-7)])-1)*var(rfmm[,c(-1,-6,-7)]))
for (i in 2:10) {
  nocl[i] <- kmeans(rfmm[,c(-1,-6,-7)], centers = i, iter.max = 1000)$tot.withinss
}
nocl
plot(1:10, nocl, type="b", xlab = "no. of clusters", ylab = "tot within")

# Try 3 or 4 clusters
ak <- kmeans(rfmm[,c(-1,-6,-7)], centers = 4, iter.max = 1000, nstart = 25)
a1 <- cbind(aa, ak$cluster)
str(a1)

# Cluster analysis results
ggplot(aa, aes(y= Effort, x = (a1$X..Nomination*100), fill = as.factor(a1$`ak$cluster`))) + theme(legend.justification = c(1,0), legend.position=c(0.95, 0.05), legend.background = element_blank(), legend.key = element_blank()) + geom_point(size = 4, shape = 21, alpha = 1/4) + xlab("% of nominations") +labs(title = "% Nominations Vs Recognition of Effort",fill = "Cluster") + scale_colour_manual(values=wes_palette(name="Moonrise2")) 
ggsave("Clusterplot.png" , path = "./Output")
# ?rainbow

# way to test using multinomal regression
a1$`ak$cluster` <- relevel(as.factor(a1$`ak$cluster`), ref = "2")
test <- multinom(a1$`ak$cluster` ~ a1$X..Nomination)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p # all p values are insignificant
# There is no support that more usage of thanks can actually increase managerial effectiveness

# Don't run it after the multinomal regression
# a1$`ak$cluster` <- as.numeric(a1$`ak$cluster`)
# t.test(a1[a1$`ak$cluster` %in% c(2),2],a1[a1$`ak$cluster` == 3, 2])

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
