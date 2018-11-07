# Introduction to the "Google Analytics Customer Revenue Prediction".
  library(tidyverse)
  library(tictoc)
  library(jsonlite)
  library(magrittr)
  library(dplyr)
  library(naniar)
  library(reshape2)
  library(ggplot2)
  library(Matrix)
  library(xgboost)

# Function to visualize the dataset upload
load_info <- function(file){
  
  message(paste("Starting the upload..."))
  tic()
  train <- read.csv(file, header = TRUE)
  message(paste("------------------"))
  message(paste("End of the upload..."))
  toc() 
  message(paste("------------------"))
  
  return(train)
}

# Function to remove NAs
remove_nas <- function(y, list){
  
  is_na_val <- function(x) x %in% list
  y <- mutate_if(y, is.factor,is.character)
  y <- mutate_if(y, is.logical,is.character)
  y <- y %>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))
  y[is.na(y)] <- 0
  
  return(y)
  
}

# Function to clean the JSON format
clean_json <- function(data){
  
  message(paste("Starting the transformation for JSON format..."))
  
  library(jsonlite)
  flatten_json <- . %>% 
    str_c(., collapse = ",") %>% 
    str_c("[", ., "]") %>% 
    fromJSON(flatten = T)
  
  parse <- . %>% 
    bind_cols(flatten_json(.$device)) %>%
    bind_cols(flatten_json(.$geoNetwork)) %>% 
    bind_cols(flatten_json(.$trafficSource)) %>% 
    bind_cols(flatten_json(.$totals)) %>% 
    select(-device, -geoNetwork, -trafficSource, -totals)
  
  tic()
  train <- parse(data)
  message(paste("------------------"))
  message(paste("End of the transformation."))
  toc()
  message(paste("------------------"))
  return(train)

}

# Function to remove na in variables some specific variables
remove_subconts <- function(y, list){
  
  is_na_val <- function(x) x %in% list
  y <- mutate_if(y, is.factor,is.character)
  y <- mutate_if(y, is.logical,is.character)
  y <- y %>% mutate_all(funs(ifelse(!is_na_val(.), NA, .)))
  y[is.na(y)] <- "other"
  
  
  
  return(y)
  
}

# Function to clean
trash_info <- function(x) {  
  trash_list <- c()
  for (header in colnames(x)) {
    if (nrow(unique(x[paste(as.character(header))])) == 1) {
      trash_list <- c(trash_list,as.character(header))
    }
  }
  return(trash_list)
}

list_not_set <- c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown","(none)")


# Once defined the functions, we are going to upload the datasets and make use of the functions above to clean de JSON format. 
# Loading dataset and cleaninng 
train <- load_info(".../train.csv")
test <- load_info(".../test.csv")

# Cleaning the JSON format
train <- clean_json(train)  
test <- clean_json(test)  

# Once we have the dataset uploaded and ready to work on it, it's time to visualize the variables.
png("g1.png")
g1 <- head(train,200000) %>% 
  is.na %>% melt %>%
  ggplot(data = .,aes(y = Var1,x = Var2)) +
  geom_raster(aes(fill = value)) + coord_flip() +
  scale_fill_grey(name = "",labels = c("Present","Missing")) +
  labs(x = "Observation",y = "Variables")
print(g1)
dev.off()

knitr::include_graphics("g1.png")

# Cleaning trash
train.tash <- trash_info(train)
test.tash <- trash_info(test)

trash.list <- names(train) %in% trash_info(train)
trash.list.test <- names(test) %in% trash_info(test)

train <- train[!trash.list]
test <- test[!trash.list.test]

train <- remove_nas(train,list_not_set)
test <- remove_nas(test,list_not_set)

# Defining the dataset as a tibble
train.tib <- as.tibble(train)
test.tib <- as.tibble(test)

print(train.tib, n=7, width=Inf)


# Creating master table
MT.train <- select(train.tib,
                   
#############################################################################
##     add here new variables, they are already clean (NA -> 0)            ##
#############################################################################

                   
                   visitNumber,
                   medium,
                   isTrueDirect,
                   hits,
                   pageviews,
                   bounces, 
                   newVisits,
                   country,
                   operatingSystem,
                   deviceCategory,
                   browser,
                   subContinent, 
                   date,
                   transactionRevenue)



MT.test <- select(test.tib,
                  
#############################################################################
##     add here new variables, they are already clean (NA -> 0)            ##
#############################################################################

                  
                   visitNumber,
                   medium,
                   isTrueDirect,
                   hits,
                   pageviews,
                   bounces, 
                   newVisits,
                   country,
                   subContinent,
                   operatingSystem,
                   deviceCategory,
                   browser,
                   date)

###########################################################################

############################################################################

##      Aqui estic seleccionant els subcontinents mes rellevants         ##

###########################################################################


 
 MT.train$aux_ind <- 0
 MT.test$aux_ind <- 0

 MT.train$aux_ind[MT.train$subContinent == "Northern America"] <- 1
 MT.train$aux_ind[MT.train$subContinent == "South America"] <- 1
 MT.train$aux_ind[MT.train$subContinent == "Eastern Asia"] <- 1
 
 MT.test$aux_ind[MT.test$subContinent == "Northern America"] <- 1
 MT.test$aux_ind[MT.test$subContinent == "South America"] <- 1
 MT.test$aux_ind[MT.test$subContinent == "Eastern Asia"] <- 1


 
 MT.train$subContinent[MT.train$aux_ind == 0] <- "other"
 MT.test$subContinent[MT.test$aux_ind == 0] <- "other"

 MT.train <- select(MT.train, -aux_ind)
 MT.test <- select(MT.test, -aux_ind)




#############################################################################

MT.train <- mutate(MT.train,
                   
#############################################################################
##     convert new varibles into your format                               ##
#############################################################################

                   transactionRevenue = as.double(transactionRevenue),
                   
                   visitNumber = as.integer(visitNumber),
                   medium = as.factor(medium),
                   isTrueDirect = as.integer(isTrueDirect),
                   hits = as.integer(hits),
                   pageviews = as.integer(pageviews),
                   bounces = as.integer(bounces),
                   newVisits = as.integer(newVisits),
                   country = as.factor(country),
                   subContinent = as.factor(subContinent),
                   operatingSystem = as.factor(operatingSystem),
                   deviceCategory=as.factor(deviceCategory),
                   browser=as.factor(browser),
                   date = as.Date(as.character(date),"%Y%m%d"))


MT.test <- mutate(MT.test,
                  
#############################################################################
##     convert new varibles into your format                               ##
#############################################################################
                   
                   visitNumber = as.integer(visitNumber),
                   medium = as.factor(medium),
                   isTrueDirect = as.integer(isTrueDirect),
                   hits = as.integer(hits),
                   pageviews = as.integer(pageviews),
                   bounces = as.integer(bounces),
                   newVisits = as.integer(newVisits),
                   country = as.factor(country),
                   subContinent = as.factor(subContinent),
                   operatingSystem = as.factor(operatingSystem),
                   deviceCategory=as.factor(deviceCategory),
                   browser=as.factor(browser),
                   date = as.Date(as.character(date),"%Y%m%d"))


#############################################################################
##     Conversion of transaction revenue                                   ##
#############################################################################

MT.train$transaction[MT.train$transactionRevenue > 0] <- 1
MT.train$transaction[MT.train$transactionRevenue == 0] <- 0
MT.train <- select(MT.train , -transactionRevenue)

# Logistic Regression
# Subsamples
set.seed(100)
ind<-sample(2, nrow(MT.train), replace = T, prob=c(0.7, 0.3))
train1<-MT.train[ind==1,]
test2<-MT.train[ind==2,]

#Logistic function
logis<-glm(transaction ~ medium + deviceCategory + hits + pageviews + subContinent + newVisits + isTrueDirect + visitNumber, data=train1, family="binomial")
summary(logis)

#Predictions
p1<-predict(logis, train1, type = "response")
head(p1)

#Classification error:
pred1<-ifelse(p1>0.1, 1, 0)
tab1<-table(Predicted= pred1, Actual=train1$transaction)
tab1
