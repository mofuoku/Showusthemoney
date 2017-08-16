train_data = read.csv("train_2016_v2.csv",header=TRUE)
View(train_data)
library(tidyr)

library(data.table)
property_data = fread("properties_2016.csv",header=TRUE,sep=',')
View(property_data)

property_data[rowSums(is.na(property_data))<length(property_data),]

property_data_filter <- property_data[!(rowSums(is.na(property_data))),]

### Delete Rows with at least one NA####
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

property_data_filter <- delete.na(property_data)
dim(property_data_filter)
### [1]  0 58: No Rows ###

property_data_zip <- property_data %>% grouping('regionidzip')
