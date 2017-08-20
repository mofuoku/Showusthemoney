library(MASS)
library(stats)
library(caret)
########################################################
train_data = read.csv("train_set.csv",header=TRUE)



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
Data05 <- Data04[1:1000,]
model1000.empty = lm(logerror~1,data=Data05)
model1000.full = lm(logerror~.,data=Data05)
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

#plot(forwardAIC)
#influencePlot(forwardAIC)
#vif(forwardAIC)
#avPlots(forwardAIC)
#confint(forwardAIC)

forwardAIC$fitted.values #Returns the fitted values.
forwardAIC$fitted.values
newdata = Data03[1002:1100,]


predict(forwardAIC, newdata, interval = "confidence") #Construct confidence intervals
predict(forwardAIC, newdata, interval = "prediction") #Construct prediction invervals



### An alternative way to select feature stepwise

train(logerror~., data=Data05, 
      method="glmStepAIC", k=2,
      trControl=trainControl(method="none"))


### Why caret:
train(logerror~., data=Data05, 
      method="glmStepAIC", k=2,
      trControl=trainControl(method="none"), preProc=c('center', 'scale'))