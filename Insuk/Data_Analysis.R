#######################################################################
######################### Data Analysis File ##########################
#######################################################################
library(tidyr)
library(dplyr)
library(data.table)
library(flexclust)
#######################################################################
### Load "train_set.csv" file: 
### Select only numeric & integer columns for the simple linear regression rest
### Set convert cell with NA to 0
### Scale each column to have sd = 1
### For the simplest test, select first 100 rows only. 
###--------------------------------------------------------------------

train_data = read.csv("train_set.csv",header=TRUE)
names(train_data)
View(train_data)
summary(train_data)

#######################################################################
### Exclude the column name "propertyzoningdesc" & "propertycountylandusecode" 
### since they are the only non-numeric columns. 
### Let numeric columns selected dataset be "01" and the interger "02

dim(train_data)
### [1] 90275    26

train_data[is.na(train_data)] <- 0 

train_data$bathroomcnt=as.integer(train_data$bathroomcnt)
train_data$calculatedfinishedsquarefeet=as.numeric(train_data$calculatedfinishedsquarefeet)
train_data$rawcensustractandblock=as.integer(train_data$rawcensustractandblock)
train_data$censustractandblock=as.integer(train_data$censustractandblock)

train_data01<-train_data[, sapply(train_data, class)=="numeric"]
names(train_data01)


train_data02<-train_data[, sapply(train_data, class)=="integer"]


dim(train_data02)
### [1] 90275    15
summary(train_data02)
### "assessmentyear" column can be deleted since it is all assessed in year 
### 2015. Intead of deleting here, I will delete the column after scaling. 
View(train_data02)
summary(train_data01)
#######################################################################


#######################################################################
### Scale columns of 01 dataset
train_data01.scaled = as.data.frame(scale(train_data01))
summary(train_data.scaled)
sapply(train_data01.scaled, sd)
###---------------------------------------------------
### logerror                bathroomcnt          lotsizesquarefeet 
###        1                          1                          1 
###   rawcensustractandblock structuretaxvaluedollarcnt          taxvaluedollarcnt 
###                        1                          1                          1 
###    landtaxvaluedollarcnt                  taxamount        censustractandblock 
###                        1                          1                          1 
###---------------------------------------------------

###----------Dataset assessmentyear column-----------------------------
train_data02_1<-train_data02[, !names(train_data02) %in% c("assessmentyear")]
###----------Combine the price column "logerror" to 02-----------------
train_data_logerror.scaled <- train_data01.scaled %>% select(logerror)
train_data02_final<-cbind(train_data_logerror.scaled,train_data02_1)


#######################################################################
### Combining numeric and integer files for future reference.
### Scaled and save it as "new_scaled_train_data.csv" 
###----------Dataset assessmentyear column-----------------------------
##train_data02_final<-cbind(train_data_logerror.scaled,train_data02_01)
train_data03<-cbind(train_data01.scaled,train_data02_final)
train_data04<-train_data03[,-c(11)]
write.csv(train_data04, file="new_scaled_train_data_01.csv")
#######################################################################


train_data.scaled01 <- train_data01.scaled[1:10000,]
train_data.scaled02 <- train_data02_final.scaled[1:10000,]

plot(train_data.scaled01, col="blue", main="First 100K Rows: Matrix Scatterplot of Conditions")
plot(train_data.scaled02, col="blue", main="First 100 Rows: Matrix Scatterplot of Conditions")


model01=lm(logerror ~ ., data = train_data.scaled01)
model02=lm(logerror ~ bathroomcnt, data = train_data.scaled01)
summary(model02)

plot(train_data.scaled01[,1], train_data.scaled01[,9])
abline(model01)

View(train_data03)


### Small LINEAR REGRESSION TEST ###

wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}

wssplot(train_data.scaled01)

d = dist(train_data.scaled01)
fit.single = hclust(d, method = "single")
fit.complete = hclust(d, method = "complete")
fit.average = hclust(d, method = "average")

clusters.average = cutree(fit.average, k = 3)

table(clusters.average)

aggregate(train_data.scaled01, by = list(cluster = clusters.average), median)

par(mfrow = c(1, 1))
plot(fit.average, hang = -1, main = "Dendrogram of Average Linkage\n5 Clusters")
rect.hclust(fit.average, k = 3)
