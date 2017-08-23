############################################################
# Zillow Prize: Zillow’s Home Value Prediction (Zestimate) #
############################################################
# 1. Data preparation
# 2. Handling missingness
# 3. Machine Learning preparation
# 4. Parameter Tuning with Cross Validation
# 5. Fitting all training set with Best parameters
# 6. Make prediction and submission

# Import libraries
library(data.table)
library(xgboost)

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

train <- read.csv("~/Desktop/Machine Learning Project/train_2016_v2.csv")


prop<- read.csv("~/Desktop/Machine Learning Project/properties_2016.csv")

sub <- read.csv("~/Desktop/Machine Learning Project/sample_submission.csv")


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


dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]

feature_names <- names(dtrain)

dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target, missing=NA)

dtest <- xgb.DMatrix(data=as.matrix( prop[,feature_names,with=FALSE]), missing=NA)

####################
# Set up Cross-validation
####################

# Set up cross-validation scheme (3-fold)
foldsCV <- createFolds(target, k=3, list=TRUE, returnTrain=FALSE)

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
preds <- predict(xgb,dtest)
 
# For now, use same predictions for each time period. 
results <- data.table(parcelid=prop$parcelid, '201610'=preds, '201611'=preds, '201612'=preds, '201710'=preds,'201711'=preds,'201712'=preds)

## Display results
write.csv(x = results, file = '/Users/mofuoku/Desktop/Machine Learning Project/submission2.csv', quote = FALSE, row.names = FALSE)

