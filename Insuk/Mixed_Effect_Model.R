
library(data.table)
library(MASS)
library(stats)
library(caret)
library(dplyr)
library(mgcv)
library(nlme)
library(lme4)
########################################################
train_data = read.csv("train_set_nonscaled.csv",header=TRUE)
View(train_data)
names(train_data)
summary(train_data)
Data02<-as.data.frame(train_data)
colnames(Data02)[colSums(is.na(Data02)) > 0]

#################### Test with Random 1000 rows #########################
##Data05 <- Data02[1:1000,]

Data05 <- Data02 %>% sample_n(1000)
Data06 <- Data02 %>% sample_n(1000)
View(Data06)
View(Data05)
Data05<-Data05[, !names(Data05) %in% c("hashottuborspa","taxdelinquencyflag","fireplaceflag")]

names(Data05)


?lme

model.predict<-lme(logerror~taxamount*(structuretaxvaluedollarcnt+lotsizesquarefeet+taxvaluedollarcnt), random=~1|taxamount/structuretaxvaluedollarcnt/lotsizesquarefeet/taxvaluedollarcnt, data=Data05, method="ML")

#model.predict<-lmer(logerror~(taxamount)*((1|structuretaxvaluedollarcnt)+(1|landtaxvaluedollarcnt)+
#                                           taxvaluedollarcnt+lotsizesquarefeet+bedroomcnt+propertylandusetypeid+roomcnt), data=Data05)
predict01=predict(model.predict, Data05, interval = "prediction") #Construct prediction invervals
predict01<-as.data.frame(predict01)
result=as.data.table(cbind(Data06[,1:2],'fit'=predict01[,1]))


model01<-lm(fit~logerror,data=result)
summary(model01)

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


pdf('Result_MEM_04.pdf') 
plot(result$logerror,result$fit,ylim=c(-1,1),xlim=c(-1,1),ylab="Predicted logerror", xlab="logerror")
abline(0,1,col="red")
abline(0.1,1,col="red",lty=2)
abline(-0.1,1,col="red",lty=2)
abline(model01)
se.lines(model01)

dev.off()

ks.test(result$logerror,result$fit)

