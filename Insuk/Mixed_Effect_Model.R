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
library(xgboost)
library(lme4)
library(nlme)
#################################################################################
###################### Step 1. DATA SET PREPARATION #############################
##-----------------------------------------------------------------------------##
##### 1. Load the cleaned data "train_set_nonscaled.csv" via random forest and
#####    tree methods.
train_data = read.csv("../../train_set_nonscaled.csv",header=TRUE)
Data02<-as.data.frame(train_data)

train_data_00 = read.csv("../../train_set_nonscaled_01.csv",header=TRUE)
data01_cluster <- kmeans(Data02[,2:10], 3, nstart=20)
clusplot(Data02[,2:10], data01_cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#train_data_01<-train_data_00 %>%  select(-c(1,2))
#train_data_01<-as.data.frame(scale(train_data_01))
#train_data_00<-train_data_00 %>%  select(c(1,2))
data_cluster <- kmeans(Data02[,3:24], 5, nstart=20)

levels<-data_cluster$cluster
Data03 <- cbind(Data02,level=levels)
write.csv(train_data_04,"../../train_set_nonscaled_02_K5.csv")
train_data_04<-read.csv("../../train_set_nonscaled_02_K5.csv",header=TRUE)
Data02<-as.data.frame(train_data_04)
Data02 <- Data02 %>% select(-c(1,2))
test_data_04 <- Data02 %>%  filter(logerror>-0.5 & logerror<=0.5)
Data06 <- test_data_04 %>% sample_n(1000, replace = T)

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
Data05 <- Data02 %>% filter(logerror>-1.0 & logerror<=1.0) %>% sample_n(500, replace=T)
Data06 <- Data02 %>% filter(logerror>-1.0 & logerror<=1.0) %>% sample_n(1000, replace = T)
Data07 <- Data02 %>% filter(logerror>-1.0 & logerror<=1.0) %>% sample_n(5000, replace = T)
Data08 <- Data03 %>% filter(logerror>-1.0 & logerror<=1.0) %>% sample_n(100000, replace = T)
##-----------------------------------------------------------------------------##
View(Data06)
dim(Data06)
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
model_8f_lm.predict = lm(logerror~(taxamount*structuretaxvaluedollarcnt*calculatedfinishedsquarefeet*lotsizesquarefeet/taxvaluedollarcnt/landtaxvaluedollarcnt)^100/(latitude*longitude), data=Data05)
#names(train_data_04)

predict_8f_lm=predict(model_8f_lm.predict, Data02, interval = "prediction") #Construct prediction invervals
predict_8f_lm<-as.data.frame(predict_8f_lm)
result_8f_lm=as.data.table(cbind(Data02[,1:2],'fit'=predict_8f_lm[,1]))
model_8f_lm<-lm(fit~logerror,data=result_8f_lm)
summary(model_8f_lm)

pdf('./Multipl_Linear_Regression/Result_MLR_8feature_500_Final.pdf')
plot(result_8f_lm$logerror,result_8f_lm$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
#main = "W/ 8 Features: Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
    main = "Random 500 Sample W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
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
?lme
model_8feature.predict<-lme(logerror~(taxamount+structuretaxvaluedollarcnt+lotsizesquarefeet+taxvaluedollarcnt), random=~1|taxamount/structuretaxvaluedollarcnt/lotsizesquarefeet/taxvaluedollarcnt, data=Data02, method="ML")

model_8feature.predict = lme(logerror~taxamount+structuretaxvaluedollarcnt+taxvaluedollarcnt+landtaxvaluedollarcnt, random=~1|taxamount/structuretaxvaluedollarcnt/taxvaluedollarcnt/landtaxvaluedollarcnt, data=Data05, method="ML")

model_8feature.predict = lme(logerror~taxamount+structuretaxvaluedollarcnt+calculatedfinishedsquarefeet+lotsizesquarefeet+taxvaluedollarcnt+landtaxvaluedollarcnt+latitude+longitude, random=~1|taxamount/structuretaxvaluedollarcnt/calculatedfinishedsquarefeet/lotsizesquarefeet/taxvaluedollarcnt/landtaxvaluedollarcnt/latitude/longitude, data=Data05, method="ML")

predict_8feature=predict(model_8feature.predict, Data02, interval = "prediction")
predict_8feature<-as.data.frame(predict_8feature)
result_8feature=as.data.table(cbind(Data02[,1:2],'fit'=predict_8feature[,1]))
model_8feature<-lm(fit~logerror,data=result_8feature)
(model_8feature)
class(predict_8feature$predict_8feature)
mean(abs(predict_8feature$predict_8feature))
pdf('./Multipl_Linear_Regression/Result_MEM_8feature_5000_Final.pdf')
plot(result_8feature$logerror,result_8feature$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
##    main = "MEM W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
     main = "Random Sample 5000 W/ 8 Features:\n MEM Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
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
model10.empty = lm(logerror~1,data=Data03)
model10.full = lm(logerror~.,data=Data03)
scope10 = list(lower = formula(model10.empty), upper = formula(model10.full))

forwardAIC10 = step(model10.empty, scope10, direction = "forward", k = 2)
backwardAIC10 = step(model10.full, scope10, direction = "backward", k = 2)
bothAIC.empty10 = step(model10.empty, scope10, direction = "both", k = 2)
bothAIC.full10 = step(model10.full, scope10, direction = "both", k = 2)

forwardBIC10 = step(model10.empty, scope10, direction = "forward", k = log(nrow(Data03)))
backwardBIC10 = step(model10.full, scope10, direction = "backward", k = log(nrow(Data03)))
bothBIC.empty10 = step(model10.empty, scope10, direction = "both", k = log(nrow(Data03)))
bothBIC.full10 = step(model10.full, scope10, direction = "both", k = log(nrow(Data03)))

summary(forwardAIC10)
summary(backwardAIC10)
summary(bothAIC.empty10)
summary(bothAIC.full10)
summary(forwardBIC10)
summary(backwardBIC10)
summary(bothBIC.empty10)
summary(bothBIC.full10)

##### 2. It finds five following features that can be added to modify the model.

##  structuretaxvaluedollarcnt   -3.518e-07  1.502e-07  -2.343  0.01913 *  
##  taxvaluedollarcnt             3.885e-07  1.497e-07   2.595  0.00946 ** 
##  landtaxvaluedollarcnt        -3.581e-07  1.498e-07  -2.391  0.01682 *  
##  taxamount                    -3.409e-06  2.648e-07 -12.874  < 2e-16 ***
##  calculatedfinishedsquarefeet  1.200e-05  1.142e-06  10.505  < 2e-16 ***
##  hashottuborspa               -1.454e-02  3.529e-03  -4.119 3.81e-05 ***
##  longitude                     5.384e-09  2.820e-09   1.910  0.05619 .  
##  propertylandusetypeid         3.201e-04  1.179e-04   2.715  0.00662 ** 
##  regionidcounty                3.566e-06  1.566e-06   2.277  0.02276 *  
##  regionidzip                  -3.549e-07  1.461e-07  -2.430  0.01512 *  
##  yearbuilt                     6.994e-05  3.056e-05   2.288  0.02212 *  
##  taxdelinquencyflag            2.424e-02  3.886e-03   6.238 4.46e-10 ***
##  bathroomcnt                  -1.939e-03  9.295e-04  -2.086  0.03698 *  
##  censustractandblock           1.241e-14  5.678e-15   2.186  0.02880 *  
##  level                         1.151e-03  5.174e-04   2.224  0.02616 * 

##-----------------------------------------------------------------------------##

#################################################################################
###################### Step 4. MODELING USING MODIFIED LM AND LME ###############
##-----------------------------------------------------------------------------##
##### 1. Model, Predict, and Compare using LM method.
#####    Generate the plot in PDF format. The plot contains information of comparison
#####    result of slope, intercept, and R^2 value.

model_lm_OCT.predict = lm(logerror~((calculatedfinishedsquarefeet+taxamount+taxvaluedollarcnt+hashottuborspa/regionidzip/longitude)+(bathroomcnt-taxdelinquencyflag*propertylandusetypeid*fireplaceflag*yearbuilt*level))^1000, data=Data06)

predict_lm_OCT=predict(model_lm_OCT.predict, Data03, interval = "prediction") #Construct prediction invervals
predict_lm_OCT<-as.data.frame(predict_lm_OCT)
result_lm_OCT=as.data.table(cbind(Data03[,1:2],'fit'=predict_lm_OCT[,1]))
model_lm_OCT<-lm(fit~logerror,data=result_lm_OCT)
summary(model_lm_OCT)

pdf('./Multipl_Linear_Regression/Result_MLR_2016_100000_Cluster.pdf')
plot(result_lm_OCT$logerror,result_lm_OCT$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
##    main = "Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
     main = "Clustered Random Sample 10K W/ 8 Features:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
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

##  structuretaxvaluedollarcnt   -3.518e-07  1.502e-07  -2.343  0.01913 *  
##  taxvaluedollarcnt             3.885e-07  1.497e-07   2.595  0.00946 ** 
##  landtaxvaluedollarcnt        -3.581e-07  1.498e-07  -2.391  0.01682 *  
##  taxamount                    -3.409e-06  2.648e-07 -12.874  < 2e-16 ***
##  calculatedfinishedsquarefeet  1.200e-05  1.142e-06  10.505  < 2e-16 ***
##  hashottuborspa               -1.454e-02  3.529e-03  -4.119 3.81e-05 ***
##  longitude                     5.384e-09  2.820e-09   1.910  0.05619 .  
##  propertylandusetypeid         3.201e-04  1.179e-04   2.715  0.00662 ** 
##  regionidcounty                3.566e-06  1.566e-06   2.277  0.02276 *  
##  regionidzip                  -3.549e-07  1.461e-07  -2.430  0.01512 *  
##  yearbuilt                     6.994e-05  3.056e-05   2.288  0.02212 *  
##  taxdelinquencyflag            2.424e-02  3.886e-03   6.238 4.46e-10 ***
##  bathroomcnt                  -1.939e-03  9.295e-04  -2.086  0.03698 *  
##  censustractandblock           1.241e-14  5.678e-15   2.186  0.02880 *  
##  level                         1.151e-03  5.174e-04   2.224  0.02616 * 

##-----------------------------------------------------------------------------##

model_OCT.predict = lme(logerror~(structuretaxvaluedollarcnt*calculatedfinishedsquarefeet*taxamount*taxvaluedollarcnt*taxdelinquencyflag-hashottuborspa*propertylandusetypeid*bathroomcnt*regionidzip*landtaxvaluedollarcnt+regionidcounty)/yearbuilt, random=~1|calculatedfinishedsquarefeet/taxamount/taxvaluedollarcnt/taxdelinquencyflag/propertylandusetypeid/bathroomcnt/regionidzip/hashottuborspa/landtaxvaluedollarcnt/structuretaxvaluedollarcnt/regionidcounty/yearbuilt, data=Data05)
?lme
predict_OCT=predict(model_OCT.predict, Data03, interval = "prediction") #Construct prediction invervals
predict_OCT<-as.data.frame(predict_OCT)
result_OCT=as.data.table(cbind(Data03[,1:2],'fit'=predict_OCT[,1]))
dim(result_OCT)
dim(Data02)
model_OCT<-lm(fit~logerror,data=result_OCT)
summary(model_OCT)

pdf('./Multipl_Linear_Regression/Result_MEM_2016_1000_Cluster.pdf')

plot(result_OCT$logerror,result_OCT$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
     main = "Clustered Random Sample 1K:\n Predicted Logerror Vs. Logerror",cex=1.3,cex.lab=1.3)
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

model.test = lmer(logerror~taxamount*structuretaxvaluedollarcnt*calculatedfinishedsquarefeet*lotsizesquarefeet+(1+latitude|longitude)+(1+hashottuborspa|bathroomcnt|bedroomcnt),data=Data05)
?lmer
?lme
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

###################
# 2 function for xgboost
###################

# implementing the XGBoost

#amo.fairobj2 <- function(preds, dtrain) {

ggbImp <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2
  
  return(list(grad = grad, hess = hess))
  
}

# custom MAE Metric for XGBoost

amm_mae <- function(preds, dtrain) {
  
  labels <- xgboost::getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab, epreds)
  
  return(list(metric = "amm_mae", value = err))
  
}


## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

###################
# load data
###################

train <- Data06 %>% select(c(1,2))

prop<- Data06

#sub <- read.csv("~/Desktop/Machine Learning Project/sample_submission.csv")


## Data Preparation
prop$hashottuborspa <- ifelse(prop$hashottuborspa == 'true', 1, 0)
prop$fireplaceflag <- ifelse(prop$fireplaceflag == 'true', 1, 0)
prop$taxdelinquencyflag <- ifelse(prop$taxdelinquencyflag == 'Y', 1, 0)
prop$propertycountylandusecode <- as.numeric(as.factor(prop$propertycountylandusecode))
prop$propertyzoningdesc <- as.numeric(as.factor(prop$propertyzoningdesc))

### Convert the data frame to data table
setDT(prop)
setDT(train)

setkey(prop, parcelid)
setkey(train, parcelid)

# join training to properties
training <- prop[train]

###################
#### set up xgboost 
###################

target <- training$logerror 

dtrain <- training[, !c('logerror', 'parcelid', 'level'), with=FALSE]

feature_names <- names(dtrain)

dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target, missing=NA)

dtest <- xgb.DMatrix(data=as.matrix( prop[,feature_names,with=FALSE]), missing=NA)

####################
# Set up Cross-validation
####################

# Set up cross-validation scheme (3-fold)
foldsCV <- createFolds(target, k=5, list=TRUE, returnTrain=FALSE)

# Set xgboost parameters. These are not necessarily the optimal parameters.
# Further grid tuning is needed. 


param <- list(booster = "gbtree", subsample = 0.7, max_depth = 2, colsample_bytree = 0.7, eta = 0.05, min_child_weight = 100)


#param <- list(booster = "gbtree"
##, objective = amo.fairobj2
#, subsample = 0.7
#, max_depth = 2
#, colsample_bytree = 0.7
#, eta = 0.05
#, min_child_weight = 100)



# mplement cross-validation for the xgboost
xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                 nrounds=30,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 100,
                 print_every_n = 5)


# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration

########################
# The best model for now
########################

xgb <- xgb.train(params = param
                 , data = dtrain
                 # , watchlist = list(train = dtrain)
                 , nrounds = 30
                 , verbose = 1
                 , print_every_n = 5
                 , feval = amm_mae)

###############
# Results of the 
###############

# Showing the Feature Importance
importance_matrix <- xgb.importance(feature_names,model=xgb)
xgb.plot.importance(importance_matrix[1:10,])

# Predict
preds <- predict(xgb,Data03)

# For now, use same predictions for each time period. 
##results <- data.table(parcelid=prop$parcelid, '201610'=preds, '201611'=preds, '201612'=preds, '201710'=preds,'201711'=preds,'201712'=preds)

## Display results
##write.csv(x = results, file = '/Users/mofuoku/Desktop/Machine Learning Project/submission2.csv', quote = FALSE, row.names = FALSE)



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

