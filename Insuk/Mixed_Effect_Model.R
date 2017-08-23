###############################################################################
##################### Mixed Effect Model Fitting ##############################
###############################################################################
### This program predicted the logerror of 201610, 201611, 201612, 201701 and
### 201702 using two basic models: multi-linear regression (LM) and the mixed-
### effective model (LME). The general description of modeling methodology can
### be summarized as following:
###
###      1. Prepare datasets: 0.1K, 1K, 10K randomly selected data as well
###         monthly filtered datasets on Jan, Feb, Oct, Nov, and Dec. Merge
###         Oct, Nov, and Dec into one for the 2016 logerror prediction and compare
###         and Jan and Feb into one for the 2017 logerror prediction.
###      2. Select the most contributed eight out of 23 features (~65% contributions).
###      3. Use LM and LME method for the first attempting modeling with preliminary
###         selected 8 features with randomly 100 selected dataset. Estimate the predicted
###         logerror values and compare to the actual data.
###      4. Find specific contributed features in specific months. For 2016 OCT, NOV,
###         and DEC, use the merged dataset. The LM model with AIC & BIC & stepwise method
###         are used to determine the special features.
###      5. Modify LM and LME models by including new feaures from step 5 with eight
###         features from step 2 and compared to the actual logerror values.
###      7. Repeat step 3 thru 6 with 1K and 10K randomly selected datasets.
###      8. Similar to Step 5, find special features in Jan and Feb for 2017 predictions.
###      9. Repeat 6 with modified LME model with the step 8 findings.
###
#################################################################################
#################################################################################
library(data.table)
library(MASS)
library(stats)
library(caret)
library(dplyr)
library(mgcv)
library(nlme)
library(lme4)
library(ggpmisc)
library(cluster)
library(fpc)
#################################################################################
###################### Step 1. DATA SET PREPARATION #############################
##-----------------------------------------------------------------------------##
##### 1. Load the cleaned data "train_set_nonscaled.csv" via random forest and
#####    tree methods.
train_data = read.csv("../../train_set_nonscaled.csv",header=TRUE)
Data02<-as.data.frame(train_data)

##### 2. Make datasets by month, OCT, NOV, and DEC for 2016 logerror comparison
#####    Do same to with Jan and Feb for 2017 prediction.
Data_10 <- Data02 %>% filter(month==10)
Data_11 <- Data02 %>% filter(month==11)
Data_12 <- Data02 %>% filter(month==12)
Data_2016 <- rbind(Data_10,Data_11,Data_12)

Data_Jan <- Data02 %>% filter(month==1)
Data_Feb <- Data02 %>% filter(month==2)
Data_2017 <- rbind(Data_Jan,Data_Feb)

##### 3. Randomly select 0.1K, 1K, 10K, 100K data sets
#####    These sets will be used for the modeling and testing.
Data05 <- Data02 %>% sample_n(100, replace=T)
Data06 <- Data02 %>% sample_n(1000, replace = T)
Data07 <- Data02 %>% sample_n(10000, replace = T)
Data08 <- Data02 %>% sample_n(100000, replace = T)
##-----------------------------------------------------------------------------##


#################################################################################
###################### Step 2. FIND MOST CONTRIBUTED FEATURES ###################
##-----------------------------------------------------------------------------##
important_feature = read.table("../../Important_feature.txt",header=FALSE)
important_feature

#1    structuretaxvaluedollarcnt 0.085605
#2                     taxamount 0.083288
#3  calculatedfinishedsquarefeet 0.082269
#4             taxvaluedollarcnt 0.081811
#5         landtaxvaluedollarcnt 0.080706
#6                      latitude 0.078395
#7             lotsizesquarefeet 0.077873
#8                     longitude 0.077280

##-----------------------------------------------------------------------------##

#################################################################################
###################### Step 2. MODELING USING LM AND LME ########################
##-----------------------------------------------------------------------------##
##### 1. Model, Predict, and Compare using LM method.
#####    Generate the plot in PDF format. The plot contains information of comparison
#####    result of slope, intercept, and R^2 value.
model_8f_lm.predict = lm(logerror~(taxamount*structuretaxvaluedollarcnt*calculatedfinishedsquarefeet*lotsizesquarefeet/taxvaluedollarcnt/landtaxvaluedollarcnt)^100/(latitude*longitude), data=test_data_04)
names(train_data_04)

predict_8f_lm=predict(model_8f_lm.predict, Data02, interval = "prediction") #Construct prediction invervals
predict_8f_lm<-as.data.frame(predict_8f_lm)
result_8f_lm=as.data.table(cbind(Data02[,1:2],'fit'=predict_8f_lm[,1]))
model_8f_lm<-lm(fit~logerror,data=result_8f_lm)

pdf('./Multipl_Linear_Regression/Result_MLR_8feature_10000.pdf')
plot(result_8f_lm$logerror,result_8f_lm$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
#main = "W/ 8 Features: Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
    main = "Random 10K Sample W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
text(-1,0.8, paste("y =", round(model_8f_lm$coef[1],2), "+", round(model_8f_lm$coef[2],2), "x","\n","R^2=",
                 formatC(summary(model_8f_lm)[[9]], format = "e", digits = 2)), pos=4, cex=1.3,col="red")
abline(0,1,col="red",lwd = 3)
abline(0.2,1,col="red",lty=2,lwd = 3)
abline(-0.2,1,col="red",lty=2,lwd = 3)
abline(model_8f_lm,lwd=3,col="blue")

se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model[[12]][2])/nrow(model[[12]][2])
  ym<-sum(model[[12]][1])/nrow(model[[12]][1])
  sdym <- sqrt((sum(model_8f_lm[[12]][1]))^2/nrow(model_8f_lm[[12]][1])-ym^2)
  a1<-(ym+sdym)-b1*xm
  a2<-(ym-sdym)-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}
se.lines(model_8f_lm)
dev.off()

##### 2. Model, Predict, and Compare using LME method.
#####    Generate the plot in PDF format. The plot contains information of comparison
#####    result of slope, intercept, and R^2 value.

model_8feature.predict = lme(logerror~taxamount+structuretaxvaluedollarcnt+calculatedfinishedsquarefeet+lotsizesquarefeet+taxvaluedollarcnt+landtaxvaluedollarcnt+latitude+longitude, random=~1|taxamount/structuretaxvaluedollarcnt/calculatedfinishedsquarefeet/lotsizesquarefeet/taxvaluedollarcnt/landtaxvaluedollarcnt/latitude/longitude, data=Data02,method="ML")

predict_8feature=predict(model_8feature.predict, Data08, interval = "prediction")
predict_8feature<-as.data.frame(predict_8feature)
result_8feature=as.data.table(cbind(Data08[,1:2],'fit'=predict_8feature[,1]))
model_8feature<-lm(fit~logerror,data=result_8feature)
summary(model_8feature)

pdf('./Multipl_Linear_Regression/Result_MEM_8feature_100000.pdf')
plot(result_8feature$logerror,result_8feature$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
##    main = "W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
     main = "Random Sample 100K W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
text(-1,0.8, paste("y =", formatC(model_8feature$coef[1],format = "e", digits = 2), 
                   "+", formatC(model_8feature$coef[2],format = "e", digits = 2), "x","\n","R^2=",
                   formatC(summary(model_8feature)[[9]], format = "e", digits = 2)), pos=4, cex=1.3,col="red")
abline(0,1,col="red",lwd = 3)
abline(0.2,1,col="red",lty=2,lwd = 3)
abline(-0.2,1,col="red",lty=2,lwd = 3)
abline(model_8feature,lwd=3,col="blue")


se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model[[12]][2])/nrow(model[[12]][2])
  ym<-sum(model[[12]][1])/nrow(model[[12]][1])
  sdym <- sqrt((sum(model_8feature[[12]][1]))^2/nrow(model_8feature[[12]][1])-ym^2)
  a1<-(ym+3*sdym)-b1*xm
  a2<-(ym-3*sdym)-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}
dev.off()

##### 3. Repeat with 1K and 10K radomly selected data.
##### 4. Make a prediction any radomly selected 100K dataset as well.
##-----------------------------------------------------------------------------##

#################################################################################
###################### Step 3. FIND UNIQUE FEATURES IN OCT, NOV, & DEC ##########
##-----------------------------------------------------------------------------##
##### 1. Use AIC & BIC STEP function to find the unique but common features
#####    had during transactions in OCT, NOV, and DEC. Then slecte features to
#####    to modify the model.
train_data_04<-read.csv("../../train_set_nonscaled_02_K5.csv",header=TRUE)
Data02<-as.data.frame(train_data_04)
Data02 <- Data02 %>% select(-c(1,2))
test_data_04 <- Data02 %>%  filter(logerror>-0.5 & logerror<=0.5)
Data06 <- test_data_04 %>% sample_n(1000, replace = T)
model10.empty = lm(logerror~1,data=Data06)
model10.full = lm(logerror~.,data=Data06)
scope10 = list(lower = formula(model10.empty), upper = formula(model10.full))

forwardAIC10 = step(model10.empty, scope10, direction = "forward", k = 2)
backwardAIC10 = step(model10.full, scope10, direction = "backward", k = 2)
bothAIC.empty10 = step(model10.empty, scope10, direction = "both", k = 2)
bothAIC.full10 = step(model10.full, scope10, direction = "both", k = 2)

forwardBIC10 = step(model10.empty, scope10, direction = "forward", k = log(nrow(test_data_04)))
backwardBIC10 = step(model10.full, scope10, direction = "backward", k = log(nrow(test_data_04)))
bothBIC.empty10 = step(model10.empty, scope10, direction = "both", k = log(nrow(test_data_04)))
bothBIC.full10 = step(model10.full, scope10, direction = "both", k = log(nrow(test_data_04)))

summary(forwardAIC10)
summary(backwardAIC10)
summary(bothAIC.empty10)
summary(bothAIC.full10)
summary(forwardBIC10)
summary(backwardBIC10)
summary(bothBIC.empty10)
summary(bothBIC.full10)

##### 2. It finds five following features that can be added to modify the model.

calculatedfinishedsquarefeet  0.0108358  0.0007928  13.668  < 2e-16 ***
  taxamount                    -0.0175995  0.0010099 -17.426  < 2e-16 ***
  taxvaluedollarcnt             0.0063197  0.0017312   3.651 0.000262 ***
  taxdelinquencyflag            0.0027024  0.0003078   8.780  < 2e-16 ***
  propertylandusetypeid         0.0009657  0.0003738   2.584 0.009778 ** 
  hashottuborspa               -0.0019337  0.0003144  -6.150 7.77e-10 ***
  month                         0.0013648  0.0003040   4.490 7.13e-06 ***
  yearbuilt                     0.0019834  0.0003607   5.499 3.84e-08 ***
  landtaxvaluedollarcnt         0.0063443  0.0012778   4.965 6.89e-07 ***
  sqftperrm                    -0.0020870  0.0004428  -4.714 2.44e-06 ***
  roomcnt                      -0.0024739  0.0006194  -3.994 6.50e-05 ***  

##-----------------------------------------------------------------------------##

#################################################################################
###################### Step 4. MODELING USING MODIFIED LM AND LME ###############
##-----------------------------------------------------------------------------##
##### 1. Model, Predict, and Compare using LM method.
#####    Generate the plot in PDF format. The plot contains information of comparison
#####    result of slope, intercept, and R^2 value.

model_lm_OCT.predict = lm(logerror~((calculatedfinishedsquarefeet+taxamount+taxvaluedollarcnt+hashottuborspa/regionidzip)+(bathroomcnt-taxdelinquencyflag*propertylandusetypeid*fireplaceflag))^1000, data=Data06)

predict_lm_OCT=predict(model_lm_OCT.predict, train_data_04, interval = "prediction") #Construct prediction invervals
predict_lm_OCT<-as.data.frame(predict_lm_OCT)
result_lm_OCT=as.data.table(cbind(train_data_04[,1:2],'fit'=predict_lm_OCT[,1]))
model_lm_OCT<-lm(fit~logerror,data=result_lm_OCT)
summary(model_lm_OCT)

pdf('./Multipl_Linear_Regression/Result_MLR_2016_1000.pdf')
plot(result_lm_OCT$logerror,result_lm_OCT$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
##    main = "Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
     main = "Random Sample 1K W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
text(-1,0.8, paste("y =", formatC(model_lm_OCT$coef[1],format = "e", digits = 2), 
                   "+", formatC(model_lm_OCT$coef[2],format = "e", digits = 2), "x","\n","R^2=",
                   formatC(summary(model_lm_OCT)[[9]], format = "e", digits = 2)), pos=4, cex=1.3,col="red")
abline(0,1,col="red",lwd = 3)
abline(0.2,1,col="red",lty=2,lwd = 3)
abline(-0.2,1,col="red",lty=2,lwd = 3)
abline(model_lm_OCT,lwd=3,col="blue")

se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model[[12]][2])/nrow(model[[12]][2])
  ym<-sum(model[[12]][1])/nrow(model[[12]][1])
  sdym <- sqrt((sum(model[[12]][1]))^2/nrow(model[[12]][1])-ym^2)
  a1<-(ym+2*sdym)-b1*xm
  a2<-(ym-2*sdym)-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}
se.lines(model_lm_OCT)
dev.off()

##### 2. Model, Predict, and Compare using LME method.
#####    Generate the plot in PDF format. The plot contains information of comparison
#####    result of slope, intercept, and R^2 value.

calculatedfinishedsquarefeet  0.0108358  0.0007928  13.668  < 2e-16 ***
  taxamount                    -0.0175995  0.0010099 -17.426  < 2e-16 ***
  taxvaluedollarcnt             0.0063197  0.0017312   3.651 0.000262 ***
  taxdelinquencyflag            0.0027024  0.0003078   8.780  < 2e-16 ***
  propertylandusetypeid         0.0009657  0.0003738   2.584 0.009778 ** 
  hashottuborspa               -0.0019337  0.0003144  -6.150 7.77e-10 ***
  month                         0.0013648  0.0003040   4.490 7.13e-06 ***
  yearbuilt                     0.0019834  0.0003607   5.499 3.84e-08 ***
  landtaxvaluedollarcnt         0.0063443  0.0012778   4.965 6.89e-07 ***
  sqftperrm                    -0.0020870  0.0004428  -4.714 2.44e-06 ***
  roomcnt 

model_OCT.predict = lme(logerror~calculatedfinishedsquarefeet+taxamount+taxvaluedollarcnt+taxdelinquencyflag+hashottuborspa+propertylandusetypeid+bathroomcnt+regionidzip+hashottuborspa+landtaxvaluedollarcnt+level, random=~1|calculatedfinishedsquarefeet/taxamount/taxvaluedollarcnt/taxdelinquencyflag/hashottuborspa/propertylandusetypeid/bathroomcnt/regionidzip/hashottuborspa/landtaxvaluedollarcnt/level, data=Data06,method="ML")

predict_OCT=predict(model_OCT.predict, Data02, interval = "prediction") #Construct prediction invervals
predict_OCT<-as.data.frame(predict_OCT)
result_OCT=as.data.table(cbind(Data02[,1:2],'fit'=predict_OCT[,1]))
dim(result_OCT)
dim(Data02)
model_OCT<-lm(fit~logerror,data=result_OCT)
summary(model_OCT)

pdf('./Multipl_Linear_Regression/Result_MEM_2016_100000.pdf')

plot(result_OCT$logerror,result_OCT$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
     main = "Random Sample 100K:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
text(-1,0.8, paste("y =", formatC(model_OCT$coef[1],format = "e", digits = 2), 
                   "+", formatC(model_OCT$coef[2],format = "e", digits = 2), "x","\n","R^2=",
                   formatC(summary(model_OCT)[[9]], format = "e", digits = 2)), pos=4, cex=1.3,col="red")
abline(0,1,col="red",lwd = 3)
abline(0.2,1,col="red",lty=2,lwd = 3)
abline(-0.2,1,col="red",lty=2,lwd = 3)
abline(model_OCT,lwd=3,col="blue")

se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model[[12]][2])/nrow(model[[12]][2])
  ym<-sum(model[[12]][1])/nrow(model[[12]][1])
  sdym <- sqrt((sum(model[[12]][1]))^2/nrow(model[[12]][1])-ym^2)
  a1<-(ym+2*sdym)-b1*xm
  a2<-(ym-2*sdym)-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}
se.lines(model_OCT) 
dev.off()

##### 3. Repeat with 1K and 10K radomly selected data.
##### 4. Make a prediction any radomly selected 100K dataset as well.
##-----------------------------------------------------------------------------##

#################################################################################
###################### Step 8. 2017 PREDICTION  #################################
##-----------------------------------------------------------------------------##
##### 1. Use AIC & BIC STEP function to find the unique but common features
#####    had during transactions in JAN and FEB. Then slecte features to
#####    to modify the model.

model10.empty = lm(logerror~1,data=Data_2017)
model10.full = lm(logerror~.,data=Data_2017)
scope10 = list(lower = formula(model10.empty), upper = formula(model10.full))

forwardAIC10 = step(model10.empty, scope10, direction = "forward", k = 2)
backwardAIC10 = step(model10.full, scope10, direction = "backward", k = 2)
bothAIC.empty10 = step(model10.empty, scope10, direction = "both", k = 2)
bothAIC.full10 = step(model10.full, scope10, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC10 = step(model10.empty, scope10, direction = "forward", k = log(nrow(Data02)))
backwardBIC10 = step(model10.full, scope10, direction = "backward", k = log(nrow(Data02)))
bothBIC.empty10 = step(model10.empty, scope10, direction = "both", k = log(nrow(Data02)))
bothBIC.full10 = step(model10.full, scope10, direction = "both", k = log(nrow(Data02)))

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC10)
summary(backwardAIC10)
summary(bothAIC.empty10)
summary(bothAIC.full10)
summary(forwardBIC10)
summary(backwardBIC10)
summary(bothBIC.empty10)
summary(bothBIC.full10)

##### 2. It finds one following feature that can be added to modify the model.

#hashottuborspa               -2.993e-02  1.071e-02  -2.796  0.00518 **

##### 3. Model, Predict, and Compare using LME method.
#####    Generate the plot in PDF format. The plot contains information of comparison
#####    result of slope, intercept, and R^2 value.
model_2017.predict = lme(logerror~taxamount+structuretaxvaluedollarcnt+calculatedfinishedsquarefeet+lotsizesquarefeet+taxvaluedollarcnt+landtaxvaluedollarcnt+latitude+longitude+hashottuborspa, random=~1|taxamount/structuretaxvaluedollarcnt/calculatedfinishedsquarefeet/lotsizesquarefeet/taxvaluedollarcnt/landtaxvaluedollarcnt/latitude/longitude/hashottuborspa, data=Data06,method="ML")

predict_2017=predict(model_2017.predict, Data02, interval = "prediction") #Construct prediction invervals
predict_2017<-as.data.frame(predict_2017)
result_2017=as.data.table(cbind(Data02[,1:2],'fit'=predict_2017[,1]))
##dim(result_2017)
##dim(Data02)
model_2017<-lm(fit~logerror,data=result_2017)
summary(model_2017)

pdf('./Multipl_Linear_Regression/Result_MEM_2017.pdf')
plot(result_2017$logerror,result_2017$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
##    main = "W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
main = "Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
text(-1,0.8, paste("y =", formatC(model_2017$coef[1],format = "e", digits = 2),
"+", formatC(model_2017$coef[2],format = "e", digits = 2), "x","\n","R^2=",
formatC(summary(model_2017)[[9]], format = "e", digits = 2)), pos=4, cex=1.3,col="red")
abline(0,1,col="red",lwd = 3)
abline(0.2,1,col="red",lty=2,lwd = 3)
abline(-0.2,1,col="red",lty=2,lwd = 3)
abline(model_2017,lwd=3,col="blue")


se.lines<-function(model){
    b1<-coef(model)[2]+summary(model)[[4]][4]
    b2<-coef(model)[2]-summary(model)[[4]][4]
    xm<-sum(model[[12]][2])/nrow(model[[12]][2])
    ym<-sum(model[[12]][1])/nrow(model[[12]][1])
    sdym <- sqrt((sum(model_2017[[12]][1]))^2/nrow(model_2017[[12]][1])-ym^2)
    a1<-(ym+3*sdym)-b1*xm
    a2<-(ym-3*sdym)-b2*xm
    abline(a1,b1,lty=2)
    abline(a2,b2,lty=2)
}
#se.lines(model_8feature)
dev.off()
##-----------------------------------------------------------------------------##
names(train_data_01)
train_data_00 = read.csv("../../train_set_nonscaled_01.csv",header=TRUE)
data01_cluster <- kmeans(Data02[,2:10], 3, nstart=20)
clusplot(Data02[,2:10], data01_cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


train_data_01<-train_data_00 %>%  select(-c(1,2))
train_data_01<-as.data.frame(scale(train_data_01))
train_data_00<-train_data_00 %>%  select(c(1,2))
data_cluster <- kmeans(train_data_01, 5, nstart=20)

levels<-data_cluster$cluster
train_data_04 <- cbind(train_data_00,train_data_01, level=levels)
write.csv(train_data_04,"../../train_set_nonscaled_02_K5.csv")
View(train_data_04)

train_data_03 <- train_data_04
train_data_04 <- train_data_04 %>% select(-c(1))
names(train_data_04)
df<-data.table(train_data_04)
sparse_matrix <- sparse.model.matrix(logerror ~ ., data = df)[,-1]
output_vector = df[,logerror] == "Marked"

bst <- xgboost(data = test_data_04, label = train_data_04, nrounds = 25, objective = "binary:logistic")

, max_depth = 4,
                             eta = 1, nthread = 2, nrounds = 100,objective = "binary:logistic")
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
head(importance)

y_pred <- predict(bst, sparse_matrix)
names <- dimnames(sparse_matrix)[[2]]
bst.plot.importance(importance_matrix[1:10,])
model <- xgb.dump(xgb, with.stats = T)
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst, data = sparse_matrix, label = output_vector)

importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)

xgb.plot.importance(importance_matrix = importance)


install.packages('xgboost')
library(xgboost)
install.packages('readr')
library(readr)
library(stringr)
library(caret)
library(car)
library(Matrix)
install.packages('drat')
library(drat)

names(train_data_04)

#clusplot(train_data_02, data01_cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#dim(train_data_02)
#medians = apply(train_data_02,2,median)
#mads = apply(train_data_02,2,mad)
#train_data_02 = scale(train_data_02,center=medians,scale=mads)
#h = (nrow(train_data_02))-1
#train_data.dist = dist(train_data_02)
#train_data.hclust = hclust(train_data.dist,method="complete")
#?hclust
#plot(train_data.hclust,labels=train_data_02$yearbuilt,main='Default from hclust')

