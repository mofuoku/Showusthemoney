
##### Import all the necesary library needed to do the analysis
library(zoo)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(curl) 
library(corrplot)
library(plyr)
library(Hmisc)
library(mice)
library(VIM)
library(lattice)

properties <- fread(input = '/Users/mofuoku/Desktop/Machine Learning Project/properties_2016.csv',
                    na.strings = "",
                    colClasses = list(character=c("parcelid", "airconditioningtypeid", "architecturalstyletypeid",
                                                  "buildingclasstypeid", "decktypeid", "fips", "hashottuborspa",
                                                  "heatingorsystemtypeid", "pooltypeid10", "pooltypeid2", "pooltypeid7",
                                                  "propertycountylandusecode", "propertylandusetypeid", 
                                                  "propertyzoningdesc", "rawcensustractandblock", "regionidcity", 
                                                  "regionidcounty", "regionidneighborhood", "regionidzip", "storytypeid", 
                                                  "typeconstructiontypeid", "fireplaceflag", "assessmentyear",
                                                  "taxdelinquencyflag", "censustractandblock")))

# Display the properties data and its structure
View(Properties)
str(properties)

train <- fread(input = '/Users/mofuoku/Desktop/Machine Learning Project/train_2016_v2.csv',
               na.strings = "",
               colClasses = list(character=c("parcelid")))

# Display the properties training data and its structure
View(train)
str(train)

sample_submission <- fread('/Users/mofuoku/Desktop/Machine Learning Project/sample_submission.csv')

View(train)

## Note that the transaction date on the training data end on 2016-12-30
## but the sample_submission start on 2016-10 ...such there is some overlap
## Then we use the training data to train to October 2016 and use the rest as testing data
## indeed we need a plot of distribution of the training data to uncover this fuzyness:

train %>% 
  mutate(year_month = make_date(year=data.table::year(transactiondate),
                                month=data.table::month(transactiondate))) %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month, y=n)) +
  geom_bar(stat="identity", 
           color="red", fill="blue") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-15"))), size=1)



## We also look at the data so as the know the
train %>% 
  filter(logerror %between% c(quantile(train$logerror, .005), 
                              quantile(train$logerror, .995))) %>%
  ggplot(aes(x=logerror)) +
  geom_histogram(aes(y=..density..), bins=50,
                 color="black", fill="blue", alpha=.5) + 
  geom_density(alpha = .2, fill = "blue")

### Issues with missing values
## Some data are completely missing while there are some with just a few:
##
missing_data <- properties %>% summarize_each(funs(sum(is.na(.))/n()))

missing_data <- gather(missing_values, key="feature", value="missing_pct")

#
#Display the missing data via graphical output
properties %>% 
  filter(parcelid %in% train$parcelid) %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  ggplot(aes(x=reorder(feature, missing_pct), y=missing_pct)) +
  geom_bar(stat="identity",
           color="black", fill="blue", alpha=.5) +
  coord_flip()
# Lot of features cannot be used using the graphical output:

# How to deal with missing data
################# Need to work on this:
feature <- (properties %>% 
              filter(parcelid %in% train$parcelid) %>%
              summarise_all(funs(sum(is.na(.))/n())) %>%
              gather(key="feature", value="missing_pct") %>%
              filter(missing_pct < .75) %>%
              select(feature))$feature

feature

## inner join the properites data to the training data
train_data <- train %>% 
  mutate(year_month = make_date(year=data.table::year(transactiondate),
                                month=data.table::month(transactiondate))) %>% 
  select(-transactiondate) %>%
  inner_join(properties %>% select(feature), by="parcelid") 

View(train_data)

# Missingness still exist (WIP)
# 
mice_plot <- aggr(train_data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#md.pattern(train_data)


###################################################################################


