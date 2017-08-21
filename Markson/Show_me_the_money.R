
## Read "train_2016_v2" file to analyze the frequency of transactions by month in year 2016. The original 
## contains information of parceid, logerror, transactiondate in the format of year-month-day.
## The parameter "logerror" defines the log of ratio of estiamted price by Zillow to the actual 
## price. 

train_data = read.csv('/Users/mofuoku/Desktop/Machine Learning Project/train_2016_v2.csv',header=TRUE)
View(train_data)
library(tidyr)
library(dplyr)
library(data.table)

############################# Count the number of transition by month ###############################
##-------------------------------------------------------------------------------------------------##
## Convert the transactiondate to datetime format and create a column "V2" with only year and month.
## Days were excluded in counting. Table table "counts" contains column of"transactiondate and V2.
counts <- data.table(train_data$transactiondate)
counts <- data.table(train_data$transactiondate, format(strptime(counts$V1, '%Y-%m-%d'), '%Y-%m'))
View(counts)

## By selecting only V2 column, a table "counts3" is generated with the column of 
## Year-Month and Frequency of transaction. 
counts2<- counts %>% select(V2)
counts3<-table(counts2$V2)

## Plot the bar plot to present the frequency of transaction. The plot shows that 
## most transactions were made in June and Agusut, least in December and November. 
barplot(counts3)
order(counts3)
####[1] 12 11 10  2  1  3  4  9  7  5  8  6

## The table "train_data2" contains information of the original train_data with transaction date in 
## format of year-month. 
train_data2 <-cbind(train_data,counts) 
train_data2 <- train_data2 %>% select(parcelid,logerror,transactiondate,V2)
View(train_data2)

############################## Test the accuracy of price over time #################################
##-------------------------------------------------------------------------------------------------##
## Select column of logerror and V2 and plot the distribution. 
train_data2 %>% select(logerror,V2)
train_data3 <- data.table("parcelid"=train_data$parcelid,"logerror"=train_data$logerror, 
                          "Month"=format(strptime(counts$V1, '%Y-%m-%d'), '%m'))

plot(train_data3$logerror,train_data3$Month,xlab = "logerror = log(Zetimate/actural Price)", ylab = "Month", 
     main = "Month vs. log(error)")

## Summarize the distribution of logerror by month
train_data3 %>% group_by(Month) %>% summarize('max'=max(logerror),'min'=min(logerror),
                                              'mean'=mean(logerror),'STD'=sd(logerror)) 

## Summarize the distribution of logerror by month in the condition of 
## 0 <= logerror < 1.2 that is for Zetimated value is overestimated within 20% 
train_data3 %>% filter(logerror>=0 & logerror<=log10(1.2)) %>% group_by(Month) %>% 
  summarize('count'=n(),'mean'=mean(logerror),'STD'=sd(logerror))

## for Zetimated value is overestimated more than 20%
train_data3 %>% filter(logerror>log10(1.2)) %>% group_by(Month) %>% 
  summarize('count'=n(),'max'=max(logerror),'mean'=mean(logerror),'STD'=sd(logerror))

## for Zetimated value is underestimated within 20%
train_data3 %>% filter(logerror<0 & logerror>=log10(0.8)) %>% group_by(Month) %>% 
  summarize('count'=n(),'mean'=mean(logerror),'STD'=sd(logerror))

## for Zetimated value is underestimated less than 20%
train_data3 %>% filter(logerror<log10(0.8)) %>% group_by(Month) %>% 
  summarize('count'=n(),'min'=min(logerror),'mean'=mean(logerror),'STD'=sd(logerror))

## for Zetimated/actual price ratio within 20%
train_data3 %>% filter(logerror>=log10(0.8) & logerror<=log10(1.2)) %>% select(parcelid,logerror,Month)

## The threshold logerror should be made. 

####################################################################################################
library(data.table)
property_data = fread("properties_2016.csv",header=TRUE,sep=',')
View(property_data)

### property data contains 58 columns and there are only 5 parameters that do not have 
### missing data. 
colnames(property_data)[colSums(is.na(property_data)) == 0]
##[1] "parcelid"                  "hashottuborspa"            "propertycountylandusecode"
##[4] "propertyzoningdesc"        "fireplaceflag"             "taxdelinquencyflag"  

## Daith of 58 columns will be divided into three different data sets. This process 
## is to reduce the size of data. 
## Since "parcelid" is the common parameter between the property and train data, this parameter 
## should be in all three data sets and can be used as the join parameter. 
property_data_01 <- data.table(property_data[,1])
View(property_data_01)
property_data_02 <- data.table(property_data[,2:20])
property_data_03 <- data.table(property_data[,21:40])
property_data_04 <- data.table(property_data[,41:58])

property_data_11 <- cbind(property_data_01,property_data_02)
property_data_12 <- cbind(property_data_01,property_data_03)
property_data_13 <- cbind(property_data_01,property_data_04)

data01 <- inner_join(train_data3,property_data_11)
data02 <- inner_join(train_data3,property_data_12)
data02 <- inner_join(train_data3,property_data_13)

View(data01)
