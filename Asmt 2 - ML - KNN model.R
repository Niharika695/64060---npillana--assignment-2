# collecting data
#Exploring and preparing data
#Training model on data
#Evaluating model performance
#improving model performance

#Importing the UniversalBank data

UBCustomer <- read.csv("UniversalBank.CSV", header = TRUE)

#using str to know the structure of dataset
str(UBCustomer)
summary(UBCustomer)

#Excluding the ID and ZIP Code columns using subset
library(dplyr)
UBCustomer<- UBCustomer[-c(1,5)]
summary(UBCustomer)

apply(UBCustomer,2,function(x){any(is.na(x))})

#Transforming the categorical variables personal loan column and Education and dummyfying them 

UBCustomer$Personal.Loan<- as.factor(UBCustomer$Personal.Loan)#converting personal loan column from categorical to numeric
UBCustomer$Education<- as.factor(UBCustomer$Education)#converting Education column
UBCustomer$Mortgage<- as.factor(UBCustomer$Mortgage)
UBCustomer$CreditCard<-as.factor(UBCustomer$CreditCard)
UBCustomer$Family<- as.factor(UBCustomer$Family)
UBCustomer$Securities.Account<- as.factor(UBCustomer$Securities.Account)
UBCustomer$Online<- as.factor(UBCustomer$Online)
UBCustomer$Experience<- as.factor(UBCustomer$Experience)
UBCustomer$Income<- as.factor(UBCustomer$Income)
UBCustomer$CD.Account<- as.factor(UBCustomer$CD.Account)

summary(UBCustomer$CD.Account)

#Converting categorical to dummy first

library(fastDummies)
library(dplyr)

#UBCustomer_dummy<- dummy_cols(UBCustomer, select_columns = c("Personal.Loan"))

UBCustomer_dummy <- dummy_cols(UBCustomer %>% select(-Personal.Loan))
#summary(UBCustomer_dummy)

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
summary(UBCustomer_train)

UBCustomer_datapartition_test <- createDataPartition(UBCustomer$Personal.Loan, p=0.4, list = FALSE)

UBCustomer_train_test <- UBCustomer_dummy[UBCustomer_datapartition_test,]
UBCustomer_Valid_test <- UBCustomer_dummy[-UBCustomer_datapartition_test,]

summary(UBCustomer_train)
summary(UBCustomer_Valid)
summary(UBCustomer_train_test)

#Normalizing numeric data using preprocess

train.norm.df <- UBCustomer_train
Valid.norm.df <- UBCustomer_Valid
train.norm.test.df<- UBCustomer_Valid_test

norm.values<-preProcess(UBCustomer_train[, 1:12], method = c("center", "scale"))

train.norm.df[, 1:12]<- predict(norm.values,UBCustomer_train[, 1:12])
Valid.norm.df[, 1:12]<- predict(norm.values,UBCustomer_Valid[, 1:12])
train.norm.test.df[, 1:12]<- predict(norm.values,UBCustomer_Valid_test[, 1:12])
test.norm.df <-predict(norm.values,UBCustomer_train_test[, 1:12])
summary(test.norm.df)
var(test.norm.df[ ,1:12])
summary(train.norm.df)
var(train.norm.df[, 1:12])
summary(Valid.norm.df)
var(Valid.norm.df[, 1:12])

#Inserting sample data

Sample <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, 
                     Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1, Education_1 = 0, Education_2 = 1,Education_3 = 0)


# Setting k = 1 using KNN

library(class)
library(FNN)

apply(test.norm.df,2,function(x){any(is.na(x))})
KNN<- knn(train = train.norm.df[, 1:12],test = test.norm.df, cl = train.norm.df[, 12], k=1, prob = TRUE)
summary(train.norm.df[, 1:13])
summary(test.norm.df)
summary(train.norm.df[, 3])

row.names(UBCustomer_train)[attr(KNN,"KNN.index")]

library(caret)

# setting k =4 using KNN

KNN_test4<- knn(train.norm.df[, 1:13], test.norm.df, cl = train.norm.df[, 3], k=4, prob = TRUE)
summary(KNN_test4)


#Confusion matrix for KNN
library(gmodels)

Test <- UBCustomer_train_test$Personal.Loan
predicted <- KNN
CrossTable(x=Test, y =KNN, prop.chisq = FALSE)


#We will use performance on the validation set to determine k, value of k from 1 to 14

accuracy.dataframe <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

#compute knn for different k on validation.

for(i in 1:20) {
  knn.pred<- knn(train.norm.df[, 1:13],Valid.norm.df[, 1:13], cl =  train.norm.df[, 3], k = i)
accuracy.dataframe[i, 2]<- confusionMatrix(knn.pred, Valid.norm.df[, 3])$overall[1]
}
accuracy.dataframe

summary(knn.pred)
        
## before we predict test set, we should combine training and validation sets and normalize data

norm.values <- preProcess(UBCustomer_Valid_test[, 1:13], method = c("center", "scale"))

train.norm.test.df[, 1:13] <- predict(norm.values, UBCustomer_Valid_test[, 1:13])

test.norm.df[, 1:13]<- predict(norm.values,UBCustomer_train_test[, 1:13])
summary(train.norm.test.df)
summary(test.norm.df)
summary(train.norm.test.df)
 
# predict for the test set

knn.new <- knn(train.norm.test.df[, 1:13],test.norm.df, cl = train.norm.test.df[, 3], k = 9 )
row.names(UBCustomer_Valid_test)[attr(KNN_test, "nn.index")]


#Splitting dataset  into training, validation, and test sets (50% : 30% : 20%)        
library(caret)
UBCustomer2_train <- createDataPartition(UBCustomer_dummy$Personal.Loan, p=0.5, list = FALSE)

UBCustomer2_train_df <- UBCustomer_dummy[UBCustomer2_train,]
UBCustomer2_test_df<- UBCustomer_dummy[-UBCustomer2_train,]

UBCustomer2_valid <- createDataPartition(UBCustomer2_test_df$Personal.Loan, p=0.6, list = FALSE)

UBCustomer2_v.train_df <- UBCustomer2_test_df[UBCustomer2_valid,]
UBCustomer2_v.test_df<- UBCustomer2_test_df[-UBCustomer2_valid,]

library(caret)
#copy the original data

train2.norm.df<- UBCustomer2_train_df
valid2.norm.df<- UBCustomer2_test_df
traval2.norm.df<- UBCustomer2_v.test_df

#preprocess to normalize

norm.values<- preProcess(UBCustomer2_train_df[, 1:13], method=c("center", "scale"))

train2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_train_df[, 1:13] )
valid2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_test_df[, 1:13] )
traval2.norm.df[, 1:13]<- predict(norm.values,UBCustomer2_v.test_df[, 1:13] )
test2.norm.df<- predict(norm.values, UBCustomer2_v.train_df[, 1:13])

summary(train2.norm.df)
var(train2.norm.df[, 1:13])
summary(valid2.norm.df)
var(valid2.norm.df[, 1:13])


#DataModeling with best model is k = 4
KNN2 <- knn(train = train2.norm.df[, 1:13], test = test2.norm.df, cl=train2.norm.df$Personal.Loan, k=4, prob = TRUE)

#Confusion matrix for KNN2
library(gmodels)

Test_label <- UBCustomer2_v.train_df$Personal.Loan
predicted_label <- KNN2
CrossTable(x=Test_label, y =KNN2, prop.chisq = FALSE)

library(knitr)
