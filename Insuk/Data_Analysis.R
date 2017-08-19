#######################################################################
######################### Data Analysis File ##########################
#######################################################################

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

### Exclude the column name "propertyzoningdesc" & "propertycountylandusecode" 
### since they are the only non-numeric columns. 

dim(train_data)
### [1] 90275    26

train_data01<-train_data[, sapply(train_data, class)=="numeric"]
names(train_data01)

###--------------------------------------------------------------------
### [1] "logerror"                   "bathroomcnt"                "lotsizesquarefeet"         
### [4] "rawcensustractandblock"     "structuretaxvaluedollarcnt" "taxvaluedollarcnt"         
### [7] "landtaxvaluedollarcnt"      "taxamount"                  "censustractandblock" 
###--------------------------------------------------------------------

dim(train_data01)
###--------------------------------------------------------------------
### [1] 90275     9
###--------------------------------------------------------------------

train_data02<-train_data[, sapply(train_data, class)=="integer"]
names(train_data02)
###--------------------------------------------------------------------
### [1] "month"                        "bedroomcnt"                  
### [3] "calculatedfinishedsquarefeet" "hashottuborspa"              
### [5] "latitude"                     "longitude"                   
### [7] "propertylandusetypeid"        "regionidcity"                
### [9] "regionidcounty"               "regionidzip"                 
### [11] "roomcnt"                      "yearbuilt"                   
### [13] "fireplaceflag"                "assessmentyear"              
### [15] "taxdelinquencyflag"   
###--------------------------------------------------------------------

dim(train_data02)

### [1] 90275    15
summary(train_data01)

train_data03<-cbind(train_data01,train_data02)

train_data03[is.na(train_data03)] <- 0 

new_train_data.scaled = as.data.frame(scale(train_data03))
sapply(new_train_data.scaled, sd)

write.csv(new_train_data.scaled, file="new_scaled_train_data.csv")

train_data03[is.na(train_data01)] <- 0 

View(train_data01)
library(flexclust)

sapply(train_data01, sd)
###---------------------------------------------------
###     logerror                bathroomcnt          lotsizesquarefeet 
### 1.610788e-01               1.004271e+00               1.150426e+05 
###    rawcensustractandblock structuretaxvaluedollarcnt          taxvaluedollarcnt 
###              2.050549e+05               2.090147e+05               5.548834e+05 
###  landtaxvaluedollarcnt                  taxamount        censustractandblock 
###           4.004943e+05               6.838824e+03               4.939707e+12 
###---------------------------------------------------

train_data.scaled = as.data.frame(scale(train_data01))
View(train_data.scaled)
summary(train_data.scaled)

sapply(train_data.scaled, sd)
###---------------------------------------------------
### logerror                bathroomcnt          lotsizesquarefeet 
###        1                          1                          1 
###   rawcensustractandblock structuretaxvaluedollarcnt          taxvaluedollarcnt 
###                        1                          1                          1 
###    landtaxvaluedollarcnt                  taxamount        censustractandblock 
###                        1                          1                          1 
###---------------------------------------------------

train_data.scaled01 <- train_data.scaled[1:100,]


plot(train_data.scaled01, col="blue", main="First 100 Rows: Matrix Scatterplot of Conditions")

model01=lm(logerror ~ bathroomcnt+lotsizesquarefeet+rawcensustractandblock+structuretaxvaluedollarcnt+
             taxvaluedollarcnt+landtaxvaluedollarcnt+taxamount+censustractandblock , data = train_data.scaled)
summary(model01)

### Small LINEAR REGRESSION TEST ###



View(train_data.scaled01)
summary(train_data.scaled01)
sapply(train_data.scaled01, sd)
names(train_data.scaled01)

plot(train_data.scaled, col="blue", main="Matrix Scatterplot of Conditions")

model01=lm(logerror ~ bathroomcnt, data = train_data.scaled)

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
