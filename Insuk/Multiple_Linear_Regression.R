library(MASS)
library(stats)
library(caret)
library(dplyr)
library(mgcv)
library(nlme)
########################################################
train_data = read.csv("train_set_nonscaled.csv",header=TRUE)
View(train_data)
names(train_data)
summary(train_data)
Data02<-as.data.frame(train_data)
colnames(Data02)[colSums(is.na(Data02)) > 0]

plot(Data02$lotsizesquarefeet,Data02$logerror)
plot(Data02$structuretaxvaluedollarcnt,Data02$logerror)
plot(Data02$taxvaluedollarcnt,Data02$logerror)
plot(Data02$landtaxvaluedollarcnt,Data02$logerror)
plot(Data02$taxamount,Data02$logerror)
plot(Data02$calculatedfinishedsquarefeet,Data02$logerror)
plot(Data02$month,Data02$logerror)
plot(Data02$hashottuborspa,Data02$logerror)
plot(Data02$latitude,Data02$logerror)
plot(Data02$longitude,Data02$logerror)
plot(Data02$propertylandusetypeid,Data02$logerror)
plot(Data02$regionidcity,Data02$logerror)
plot(Data02$regionidcounty,Data02$logerror)
plot(Data02$regionidzip,Data02$logerror)
plot(Data02$regionidcity,Data02$logerror)
plot(Data02$roomcnt,Data02$logerror)
plot(Data02$yearbuilt,Data02$logerror)
plot(Data02$taxdelinquencyflag,Data02$logerror)
plot(Data02$rawcensustractandblock,Data02$logerror)
plot(Data02$bathroomcnt,Data02$logerror)
plot(Data02$censustractandblock,Data02$logerror)
plot(Data02$bedroomcnt,Data02$logerror)
plot(Data02$fireplaceflag,Data02$logerror)


model11.empty = glm(lotsizesquarefeet~1,data=Data02)
model11.full = glm(lotsizesquarefeet~.,data=Data05)
scope11 = list(lower = formula(model11.empty), upper = formula(model11.full))
forwardAIC = step(model11.empty, scope11, direction = "forward", k = 2)
summary(forwardAIC)
######################################################

ncol(Data03)    #[1] 23
summary(Data03)
names(Data03)
colnames(Data03)[colSums(is.na(Data03)) > 0]
### There are total of 89670 NA's on "censustractandblock"
### column
Data04<-train_data[,-c(22)]  
names(Data04)


########################################################
### Reading Data of cleaned and scaled data ############
### There should be total of 90275 rows and 24 columns #
###--------------------------------------------------###
Data01 = read.csv("new_scaled_train_data_01.csv",header=TRUE) #[1] 90275    24
Data02<-as.data.frame(Data01)
Data03<-Data02[,-c(1)] # Delete the first column "X" 
ncol(Data03)    #[1] 23
summary(Data03)
names(Data03)
colnames(Data03)[colSums(is.na(Data03)) > 0]
### There are total of 89670 NA's on "censustractandblock"
### column
Data04<-Data03[,-c(23)]  
names(Data04)
###--------------------------------------------------###

#################### Test with first 1000 rows #########################
Data05 <- Data02[1:1000,]

Data05<-Data05[, !names(Data05) %in% c("hashottuborspa","taxdelinquencyflag","fireplaceflag")]
train_data01<-train_data[, !names(train_data) %in% c("assessmentyear")]
train_data01<-train_data[, !names(train_data) %in% c("assessmentyear")]

model1000.empty = glm(logerror~1,data=Data05)
model1000.full = glm(logerror~.,data=Data05)
scope1000 = list(lower = formula(model1000.empty), upper = formula(model1000.full))

forwardAIC = step(model1000.empty, scope1000, direction = "forward", k = 2)
backwardAIC = step(model1000.full, scope1000, direction = "backward", k = 2)
bothAIC.empty = step(model1000.empty, scope1000, direction = "both", k = 2)
bothAIC.full = step(model1000.full, scope1000, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model1000.empty, scope1000, direction = "forward", k = log(nrow(Data05)))
backwardBIC = step(model1000.full, scope1000, direction = "backward", k = log(nrow(Data05)))
bothBIC.empty = step(model1000.empty, scope1000, direction = "both", k = log(nrow(Data05)))
bothBIC.full = step(model1000.full, scope1000, direction = "both", k = log(nrow(Data05)))

#In this case, all procedures yield the model with only the Murder, HS.Grad,
#Frost, and Population variables intact.

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
summary(backwardAIC)
summary(bothAIC.empty)
summary(bothAIC.full)

summary(forwardBIC)
summary(backwardBIC)
summary(bothBIC.empty)
summary(bothBIC.full)


confidence1000=predict(forwardAIC, Data05, interval = "confidence") #Construct confidence intervals
predict1000=predict(forwardAIC, Data05, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict1000)
result=as.data.table(cbind(Data05[,1:2],'fit'=predict01[,1]))
plot(result$logerror,result$fit,ylim=c(-1,1),xlim=c(-1,1))

confidence1000=predict(backwardAIC, Data05, interval = "confidence") #Construct confidence intervals
predict1000=predict(backwardAIC, Data05, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict1000)
result=as.data.table(cbind(Data05[,1:2],'fit'=predict01[,1]))
plot(result$logerror,result$fit,ylim=c(-1,1),xlim=c(-1,1))

confidence1000=predict(bothAIC.empty, Data05, interval = "confidence") #Construct confidence intervals
predict1000=predict(bothAIC.empty, Data05, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict1000)
result=as.data.table(cbind(Data05[,1:2],'fit'=predict01[,1]))
plot(result$logerror,result$fit,ylim=c(-1,1),xlim=c(-1,1))

confidence1000=predict(bothAIC.full, Data05, interval = "confidence") #Construct confidence intervals
predict1000=predict(bothAIC.full, Data05, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict1000)
result=as.data.table(cbind(Data05[,1:2],'fit'=predict01[,1]))
plot(result$logerror,result$fit,ylim=c(-1,1),xlim=c(-1,1))

confidence1000=predict(forwardBIC, Data05, interval = "confidence") #Construct confidence intervals
predict1000=predict(forwardBIC, Data05, interval = "prediction") #Construct prediction invervals

names(Data05)

?lme
model.predict<-lme(logerror~taxamount*(structuretaxvaluedollarcnt+landtaxvaluedollarcnt+
                                         taxvaluedollarcnt+lotsizesquarefeet+bedroomcnt), 
                   random=~1|taxamount/structuretaxvaluedollarcnt/landtaxvaluedollarcnt/
                     bedroomcnt/taxvaluedollarcnt/lotsizesquarefeet, data=Data05, method="ML")

predict01=predict(model.predict, Data05, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict01)
result=as.data.table(cbind(Data05[,1:2],'fit'=predict01[,1]))
plot(result$logerror,result$fit,ylim=c(-1,1),xlim=c(-1,1))
summary(model.predict)
########################### Result Comparison ###############################
model01<-lm(fit~logerror,data=result)

model02<-lm(logerror~1,data=result)

se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model01[[12]][2])/nrow(model01[[12]][2])
  ym<-sum(model01[[12]][1])/nrow(model01[[12]][1])
  sdym <- sqrt((sum(model01[[12]][1]))^2/nrow(model01[[12]][1])-ym^2)
  a1<-(ym+sdym)-b1*xm
  a2<-(ym-sdym)-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}
abline(0,1,col="red")
abline(0.3,1,col="red",lty=2)
abline(-0.3,1,col="red",lty=2)
abline(model01)
se.lines(model01)
summary(model01)


ks.test(result$logerror,result$fit)


########################### Entire Data #####################################
#################### Test with first 1000 rows #########################
Data05 <- Data04[1:1000,]

model1000.empty = lm(logerror~1,data=Data02)
model1000.full = lm(logerror~.,data=Data02)
scope1000 = list(lower = formula(model1000.empty), upper = formula(model1000.full))

forwardAIC = step(model1000.empty, scope1000, direction = "forward", k = 2)
backwardAIC = step(model1000.full, scope1000, direction = "backward", k = 2)
bothAIC.empty = step(model1000.empty, scope1000, direction = "both", k = 2)
bothAIC.full = step(model1000.full, scope1000, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model1000.empty, scope1000, direction = "forward", k = log(nrow(Data02)))
backwardBIC = step(model1000.full, scope1000, direction = "backward", k = log(nrow(Data02)))
bothBIC.empty = step(model1000.empty, scope1000, direction = "both", k = log(nrow(Data02)))
bothBIC.full = step(model1000.full, scope1000, direction = "both", k = log(nrow(Data02)))

#In this case, all procedures yield the model with only the Murder, HS.Grad,
#Frost, and Population variables intact.

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
summary(backwardAIC)
summary(bothAIC.empty)
summary(bothAIC.full)

summary(forwardBIC)
summary(backwardBIC)
summary(bothBIC.empty)
summary(bothBIC.full)


confidence1000=predict(forwardAIC, Data02, interval = "confidence") #Construct confidence intervals
predict1000=predict(forwardAIC, Data02, interval = "prediction") #Construct prediction invervals
View(predict1000)
predict1000[,1]
dim(predict1000)
newdata[,1]

confidence1000=predict(forwardBIC, Data02, interval = "confidence") #Construct confidence intervals
predict1000=predict(forwardBIC, Data02, interval = "prediction") #Construct prediction invervals


model.predict=glm(logerror~calculatedfinishedsquarefeet/taxamount, data=Data02)
predict01=predict(model.predict, Data02, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict01)
result=as.data.table(cbind(Data02[,1:2],'fit'=predict01[,1]))
View(result)
plot(result$logerror,result$fit)

########################### Result Comparison ###############################
model01<-lm(fit~logerror,data=result)


se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model01[[12]][2])/nrow(model01[[12]][2])
  ym<-sum(model01[[12]][1])/nrow(model01[[12]][1])
  a1<-ym-b1*xm
  a2<-ym-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}

abline(model01)
se.lines(model01)
summary(model01)


ks.test(result$logerror,result$fit)
