UBCustomer<- read.csv("UniversalBank.CSV", header = TRUE)
str(UBCustomer)

#Excluding the ID and ZIP Code columns using subset
UBCust<- UBCustomer[-c(1,5)]
library(dplyr)
library(fastDummies)
UBCustomer_dummy<- dummy_cols(UBCustomer, select_columns = c("Personal.Loan"))

#UBCustomer_dummy <- dummy_cols(UBCustomer %>% select = (-personal.loan))
summary(UBCustomer_dummy)
#Remove Education column and using mutate which adds new variable and preserves the old one

UBCustomer_dummy<- UBCustomer_dummy %>% select(-Education) %>% mutate(Personal.Loan = UBCustomer$Personal.Loan)
summary(UBCustomer_dummy)
#to check null values present
apply(UBCustomer,2,function(x){any(is.na(x))})
library(dplyr)
set.seed(15)
# dividing the data between training(60) and validation(40) sets
library(caret)
UBCustomer_datapartition_train <- createDataPartition(UBCustomer_dummy$Personal.Loan, p=0.6, list = FALSE)
UBCustomer_train <- UBCustomer_dummy[UBCustomer_datapartition_train,]
UBCustomer_Valid <- UBCustomer_dummy[-UBCustomer_datapartition_train,]

UBCustomer_datapartition_test <- createDataPartition(UBCustomer_dummy$Personal.Loan, p=0.4, list = FALSE)
UBCustomer_train_test <- UBCustomer_dummy[UBCustomer_datapartition_test,]
UBCustomer_Valid_test <- UBCustomer_dummy[-UBCustomer_datapartition_test,]

#checking summary 
summary(UBCustomer_train)
summary(UBCustomer_Valid)
summary(UBCustomer_train_test)

#Normalizing numeric data
train.norm.df <-UBCustomer_train
Valid.norm.df <-UBCustomer_Valid
train.norm.test.df<- UBCustomer_Valid_test

#preprocess is a caret data helps in tranformation on a training data which can be applied to test data
norm.values<-preProcess(UBCustomer_train[, 1:12], method = c("center", "scale"))

train.norm.df[, 1:13]<- predict(norm.values,UBCustomer_train[, 1:13])#normalizing 13 columns
Valid.norm.df[, 1:13]<- predict(norm.values,UBCustomer_Valid[, 1:13])
train.norm.test.df[, 1:13]<- predict(norm.values,UBCustomer_Valid_test[, 1:13])
test.norm.df <-predict(norm.values,UBCustomer_train_test[, 1:13])

summary(train.norm.df)
var(train.norm.df[, 1:13])
summary(Valid.norm.df)
var(Valid.norm.df[, 1:13])

# performing K = 1
library(FNN)
library(class)
library(gmodels)

KNN_test<- knn(train.norm.df[, 1:13], test.norm.df, cl = train.norm.df[, 3], k=1, prob = TRUE)
summary(KNN_test)

row.names(UBCustomer_train)[attr(KNN_test,"KNN_test.index")]

#cross table 
Test <- UBCustomer_train_test$Personal.Loan
predicted <- KNN_test
CrossTable(x=Test, y =KNN_test, prop.chisq = FALSE)

#prediction for choice of k
library(caret)
#Normalizing the values before we do prediction

norm.values <- preProcess(UBCustomer_Valid_test[, 1:13], method = c("center", "scale"))
train.norm.test.df[, 1:13] <- predict(norm.values, UBCustomer_Valid_test[, 1:13] )
test.norm.df[, 1:13]<- predict(norm.values,UBCustomer_train_test[, 1:13])
summary(train.norm.test.df)
summary(test.norm.df)


knn.new <- knn(train.norm.test.df[, 1:13],test.norm.df, cl = train.norm.test.df[, 3], k = 7 )

#creating data partition for(training-50% valid-30% test-20%)

UBCustomer2 <- createDataPartition(UBCustomer_dummy$Personal.Loan, p=0.5, list = FALSE)
UBCustomer2_train_df <- UBCustomer_dummy[UBCustomer2,]
UBCustomer2_test_df<- UBCustomer_dummy[-UBCustomer2,]

UBCustomer2_valid <- createDataPartition(UBCustomer2_test_df$Personal.Loan, p=0.6, list = FALSE)
UBCustomer2_v.train_df <- UBCustomer2_test_df[UBCustomer2_valid,]
UBCustomer2_v.test_df<- UBCustomer2_test_df[-UBCustomer2_valid,]

#Installing the caret library
library(caret)

train2.norm.df<- UBCustomer2_train_df
valid2.norm.df<- UBCustomer2_test_df
traval2.norm.df<- UBCustomer2_v.test_df

#normalizing the values
norm.values<- preProcess(UBCustomer2_train_df[, 1:13], method=c("center", "scale"))

train2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_train_df[, 1:13] )#normalizing 13 columns for 2nd partition before running KNN
traval2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_v.test_df[, 1:13] )
valid2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_test_df[, 1:13] )
test2.norm.df<- predict(norm.values, UBCustomer2_v.train_df[, 1:13])
traval2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_v.test_df[, 1:13] )

summary(train2.norm.df)
var(train2.norm.df[, 1:13])
var(train2.norm.df[, 1:2])

#DataModeling with best model is k = 4
KNN2 <- knn(train = train2.norm.df[, 1:13], test = test2.norm.df, cl=train2.norm.df$Personal.Loan, k=4, prob = TRUE)

#performing crosstable for KNN2
library(gmodels)

Test_label <- UBCustomer2_v.train_df$Personal.Loan
predicted_label <- KNN2
CrossTable(x=Test_label, y =KNN2, prop.chisq = FALSE)

row.names(UBCustomer_train)[attr(KNN2,"KNN2.index")]


