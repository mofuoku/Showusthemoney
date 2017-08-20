train_data = read.csv("train_set.csv",header=TRUE)
View(train_data)

train_data = read.csv("new_scaled_train_data_02.csv",header=TRUE)
names(train_data)
as.data.frame(train_data)
train_data06<-train_data[,-c(1,18)]
names(train_data06)

Data01 = read.csv("new_scaled_train_data_01.csv",header=TRUE)
names(Data01)
Data02<-as.data.frame(Data01)
names(Data02)
ncol(Data02)
Data03<-Data02[,-c(1)]
names(Data03)
ncol(Data03)
Data04<-Data03[,-c(22,23)]
names(Data04)


model001=lm(logerror~.,data=Data03)
model001
predictions001 <- data.frame(predict(model001, Data03))
head(predictions001)
summary(predictions001)
plot(predictions001)
names(Data04)

Data02 <- inner_join(train_data,Data02)
model001=lm(logerror~.,data=Data02)
predictions001 <- data.frame(predict(model001, train_data07))
summary(predictions02)

 
train_data07 <- train_data06 %>% filter(train_data06$yearbuilt>0)
train_data06_100 <- train_data06[1:1000,]
plot(train_data06_100, col="blue", main="First 100 Rows: Matrix Scatterplot of Conditions")
model03=lm(logerror~train_data06_100$yearbuilt, data = train_data06_100)
model03

plot(train_data06_100$logerror,train_data06_100$yearbuilt)
abline(model03)

hist(train_data07$yearbuilt)

train_data08 <- train_data07 %>% select(logerror,latitude,longitude,yearbuilt)


model01=lm(longitude ~ ., data = train_data08)
model01
predictions01 <- data.frame(predict(model01, train_data07))
predictions01
summary(predictions01)

train_data09 <- train_data07 %>% select(logerror,month,bathroomcnt,bedroomcnt,hashottuborspa,yearbuilt)
model02=lm(logerror ~ ., data = train_data07)
model02
predictions02 <- data.frame(predict(model02, train_data07))
predictions02
summary(predictions02)

train_data10 <- train_data07 %>% select(1:4)
names(train_data10)
model03=lm(logerror ~ ., data = train_data10)
model03
predictions03 <- data.frame(predict(model03, train_data07))
summary(predictions03)

train_data11 <- train_data07 %>% select(1:8)
names(train_data11)
model04=lm(logerror ~ ., data = train_data10)
predictions04 <- data.frame(predict(model04, train_data07))
summary(predictions04)

ncol(train_data07)
train_data12 <- train_data07 %>% select(1:12)
model05=lm(logerror ~ ., data = train_data12)
predictions05 <- data.frame(predict(model05, train_data07))
summary(predictions05)

train_data13 <- train_data07 %>% select(1:16)
model06=lm(logerror ~ ., data = train_data13)
predictions06 <- data.frame(predict(model06, train_data07))
summary(predictions06)


train_data11 <- train_data07 %>% select(logerror,propertylandusetypeid,rawcensustractandblock,regionidcity,regionidcounty,yearbuilt)
model03=lm(logerror ~ ., data = train_data10)
model03
predictions03 <- data.frame(predict(model03, train_data07))
predictions03
summary(predictions03)


set.seed(0)
km.train = kmeans(train_data08_100, centers = 3, nstart = 100)
km.train

plot(train_data08_100, col = km.train$cluster,
     main = paste("Best K-Means Attempt out of 100\n WCV: ",
                  round(km.train$tot.withinss, 4)))



hist(train_data09$bathroomcnt)

summary(train_data09$bathroomcnt)
model02=lm(logerror ~ ., data = train_data09)
model02
predictions <- data.frame(predict(model02, train_data07))
predictions

plot(predictions)

plot(train_data09$yearbuilt, train_data09$logerror)
abline(model02)

