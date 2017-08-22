library(data.table)
library(MASS)
library(stats)
library(caret)
library(dplyr)
library(mgcv)
library(nlme)
library(lme4)
########################################################
train_data = read.csv("../../train_set_nonscaled.csv",header=TRUE)
View(train_data)
names(Data02)
summary(train_data)
Data02<-as.data.frame(train_data)
colnames(Data02)[colSums(is.na(Data02)) > 0]
View(Data02)
Data_OCT <- Data02 %>% filter(month==10)
Data_NOV <- Data02 %>% filter(month==11)
Data_DEC <- Data02 %>% filter(month==12)
Data_2016 <- rbind(Data_OCT,Data_NOV,Data_DEC)

##plot(Data02$lotsizesquarefeet,Data02$logerror)
##plot(Data02$structuretaxvaluedollarcnt,Data02$logerror)
##plot(Data02$taxvaluedollarcnt,Data02$logerror)
##plot(Data02$landtaxvaluedollarcnt,Data02$logerror)
##plot(Data02$taxamount,Data02$logerror)
##plot(Data02$calculatedfinishedsquarefeet,Data02$logerror)
##plot(Data02$month,Data02$logerror)
##plot(Data02$hashottuborspa,Data02$logerror)
##plot(Data02$latitude,Data02$logerror)
##plot(Data02$longitude,Data02$logerror)
##plot(Data02$propertylandusetypeid,Data02$logerror)
##plot(Data02$regionidcity,Data02$logerror)
##plot(Data02$regionidcounty,Data02$logerror)
##plot(Data02$regionidzip,Data02$logerror)
##plot(Data02$regionidcity,Data02$logerror)
##plot(Data02$roomcnt,Data02$logerror)
##plot(Data02$yearbuilt,Data02$logerror)
##plot(Data02$taxdelinquencyflag,Data02$logerror)
##plot(Data02$rawcensustractandblock,Data02$logerror)
##plot(Data02$bathroomcnt,Data02$logerror)
##plot(Data02$censustractandblock,Data02$logerror)
##plot(Data02$bedroomcnt,Data02$logerror)
##plot(Data02$fireplaceflag,Data02$logerror)

############ Generalized Linear Model FOR RANDOM 1000 DATA ######################
#################################################################################


####################### Random 10000 DATA ########################################
###---------------------------------------------------------------------------###

Data05 <- Data02 %>% sample_n(10000, replace=T)

#### STEP ####
model11.empty = lm(logerror~1,data=Data_NOV)
model11.full = lm(logerror~.,data=Data_NOV)
scope11 = list(lower = formula(model11.empty), upper = formula(model11.full))


forwardAIC = step(model11.empty, scope11, direction = "forward", k = 2)
backwardAIC = step(model11.full, scope11, direction = "backward", k = 2)
bothAIC.empty = step(model11.empty, scope11, direction = "both", k = 2)
bothAIC.full = step(model11.full, scope11, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model11.empty, scope11, direction = "forward", k = log(nrow(Data05)))
backwardBIC = step(model11.full, scope11, direction = "backward", k = log(nrow(Data05)))
bothBIC.empty = step(model11.empty, scope11, direction = "both", k = log(nrow(Data05)))
bothBIC.full = step(model11.full, scope11, direction = "both", k = log(nrow(Data05)))

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
summary(backwardAIC)
summary(bothAIC.empty)
summary(bothAIC.full)

summary(forwardBIC)
summary(backwardBIC)
summary(bothBIC.empty)
summary(bothBIC.full)

confidenceFA=predict(forwardAIC, Data_OCT, interval = "confidence") #Construct confidence intervals
predictFAIC=predict(forwardAIC, Data_OCT, interval = "prediction") #Construct prediction invervals
predictFAIC<-as.data.frame(predictFAIC)
resultFAIC=as.data.table(cbind(Data_OCT[,1:2],'fit'=predictFAIC[,1]))

model01<-lm(fit~logerror,data=resultFAIC)
summary(model01)

model10 = lm(logerror~(structuretaxvaluedollarcnt+taxamount+calculatedfinishedsquarefeet+taxvaluedollarcnt+landtaxvaluedollarcnt)^10, data=Data05)

model11 = lm(logerror~(calculatedfinishedsquarefeet+propertylandusetypeid*bathroomcnt+taxamount^10), data=Data05)

predict10=predict(model10, Data_OCT, interval = "prediction")
predict11=predict(model11, Data_NOV, interval = "prediction")
predict12=predict(model10, Data_DEC, interval = "prediction")

result10=as.data.table(cbind(Data_OCT[,1:2],'fit'=predict10[,1]))
result11=as.data.table(cbind(Data_NOV[,1:2],'fit'=predict11[,1]))
result12=as.data.table(cbind(Data_DEC[,1:2],'fit'=predict12[,1]))

model01<-lm(fit~logerror,data=result10)
model02<-lm(fit~logerror,data=result11)
model03<-lm(fit~logerror,data=result12)

summary(model01)
summary(model02)
summary(model03)

pdf('./Multipl_Linear_Regression/Result_MLR_OCT.pdf') 
plot(result10$logerror,result10$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror",
     main = "Using Random 10K Sample: FBIC Predicted Logerror Vs. Logerror")
points(result11$logerror,result11$fit,pch=25)
abline(0,1,col="red")
abline(0.1,1,col="red",lty=2)
abline(-0.1,1,col="red",lty=2)
abline(model01)
abline(model02)
se.lines<-function(model){
  b1<-coef(model)[2]+summary(model)[[4]][4]
  b2<-coef(model)[2]-summary(model)[[4]][4]
  xm<-sum(model[[12]][2])/nrow(model[[12]][2])
  ym<-sum(model[[12]][1])/nrow(model[[12]][1])
  sdym <- sqrt((sum(model[[12]][1]))^2/nrow(model[[12]][1])-ym^2)
  a1<-(ym+sdym)-b1*xm
  a2<-(ym-sdym)-b2*xm
  abline(a1,b1,lty=2)
  abline(a2,b2,lty=2)
}
se.lines(model01)

dev.off()

ks.test(result$logerror,result$fit)
