
library(randomForest)
library(corrplot)






train_set_clean <- read.csv('train_set_clean.csv', header = TRUE, stringsAsFactors = FALSE)


summary(train_set_clean)

# Devide Data into Training and Validation Sets
set.seed(0)
train <- sample(1:nrow(train_set_clean),nrow(train_set_clean)*0.7)

test <- -train
trainset <- train_set_clean[train,]
logerr_train <- train_set_clean$logerror[train]
testset <- train_set_clean[test]



#################
# corrplot of numeric variables

numeric_train <- trainset[,1:7]
correlations <- cor(numeric_train,use="everything")
corrplot(correlations,method="circle",type="lower",sig.level = 0.01,insig="blank")

#'landtaxvaluedollarcnt','taxvaluedollarcnt','structuretaxvalue','taxamount'，‘calculatedfinishedsquarefeet' are higly correlated with each other, but little correlationship with the logerror

pairs(numeric_train)



#########################
# Consider only the categorical variables.
categorical_train <- trainset[,c(1,8:ncol(trainset))]
categorical_train_idexlude <- categorical_train[,-c(5,6,7,8,9,10,15,17)]
pairs(categorical_train[1:10000])


#We can use stepwise regression to help automate the variable selection process.
#Here we define the minimal model, the full model, and the scope of the models
#through which to search:
model.empty=lm(logerror~1, data= categorical_train_idexlude)
# The model with an intercept ONLY.
model.full = lm(logerror ~ ., data = categorical_train_idexlude) #The model with ALL variables.
scope = list(lower=formula(model.empty),upper=formula(model.full))

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty,scope, direction="forward", k=2)
backwardAIC = step(model.full,scope, direction="backward",k=2)
bothAIC.empty=step(model.empty,scope,direction = "both",k=2)
bothAIC.full = step(model.full, scope, direction = "both", k=2)


# Summary
summary(bothAIC.)






#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty,)



#########################
### XGBoost##############
#########################














