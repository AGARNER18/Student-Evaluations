# Amber Garner
# Nov. 16, 2016
# K-Means clustering Student Evaluations 
# Assumes cluster and fpc packages are installed


# load necessary packages
library(cluster)
library(fpc)

# Load credit vehicle dataset
turk <- read.csv("turkiye-student-prof.csv", header = T, sep = ",")

# View dataset
head(turk)
summary(turk)
str(turk)

# View distribution and labels of Class
table((turk$Class))

# View distribution of instructor id
table(turk$V1)

# create new variable
myturk <- turk

# remove Class
myturk$Class <- NULL
myturk$V1 <- NULL

# verify removal
head(myturk)

#*****plot total distance between clusters and within clusters to find optimal k value
bss <- integer(length(2:15))
for (i in 2:15) bss[i] <- kmeans(myturk, centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters", ylab="Sum of Squares", col="blue")

wss <- integer(length(2:15))
for (i in 2:15) wss[i] <- kmeans(myturk, centers=i)$withinss
lines(1:15, wss, type="b", col="red")

#******K MEANS WITH 3 CLUSTERS*******

# reproduce same output
set.seed(333)

# build cluster model with 3 clusters
kc3 <- kmeans(myturk, 3)

# print results
kc3

# number observations in each cluster
kc3$size

# between sum of squares
kc3$betweenss

# within sum of squares
kc3$tot.withinss

# number iterations
kc3$iter

# cluster centers
kc3$centers

# create csv file with centers
write.csv(kc3$centers, file="kc3center.csv")

# Teacher Effectiveness Dominant Clusters
table(turk$V1, kc3$cluster)

# Course Descriptor to determine accuracy
table(turk$Class, kc3$cluster)

# determine accuracy for Class
((149+233+218+15+274+81+373+104+287+262+82+238+276)/(nrow(myturk)))*100

# plot clustering
clusplot(myturk, kc3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myturk, kc3$cluster)

#******K MEANS WITH 4 CLUSTERS*******

# reproduce same output
set.seed(444)

# build cluster model with 4 clusters
kc4 <- kmeans(myturk, 4)

# print results
kc4


# plot clustering
clusplot(myturk, kc4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myturk, kc4$cluster)

#***K MEANS WITH 5 CLUSTERS*******

# reproduce same output
set.seed(555)

# build cluster model with 5 clusters
kc5 <- kmeans(myturk, 5)

# print results
kc5

# plot clustering
clusplot(myturk, kc5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myturk, kc5$cluster)

#***K MEANS WITH 2 CLUSTERS*******

# reproduce same output
set.seed(222)

# build cluster model with 5 clusters
kc2 <- kmeans(myturk, 2)

# print results
kc2

# plot clustering
clusplot(myturk, kc2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myturk, kc2$cluster)

# get cluster stats for each k value model
d <- dist(myturk, method = "euclidean")
cluster.stats(d, kc3$cluster, alt.clustering = turk$Class)
cluster.stats(d, kc4$cluster, alt.clustering = turk$Class)
cluster.stats(d, kc5$cluster, alt.clustering = turk$Class)
cluster.stats(d, kc2$cluster, alt.clustering = turk$Class)

#*******PAMK ANALYSIS******************
set.seed(1122)

best <- pamk(myturk)
best

# Check distribution of clusters for pamk method
table( turk$V1,best$pamobject[["clustering"]])

# Check accuracy of clusters for pamk method
table( turk$Class,best$pamobject[["clustering"]])
((160+245+245+18+356+83+410+111+287+289+74+232+274)/nrow(myturk))*100

# Plot pamk method 
clusplot(myturk, best$pamobject[["clustering"]], color=T)

# simple plot of pamk method
plotcluster(myturk, best$pamobject[["clustering"]])


