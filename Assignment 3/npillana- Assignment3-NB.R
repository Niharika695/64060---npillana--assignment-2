# Setting the current directory

setwd("C:/Users/nihar/Desktop")
#Installing packages
library(dplyr)
library(gmodels)
library(e1071)
library(caret)

#Step 1 - collecting data
UniversalBank<- read.csv("UniversalBank.csv", header = TRUE)
str(UniversalBank)

#Converting to data frame as naiveBayes formula interface handles data frames or arrays only

UB_col<-select(UniversalBank,c(10,13,14))

#UB_col

#Step 2 - exploring and preparing the data

#Data preparation - creating training(60%) and Validation(40%) datasets

set.seed(15)

UB_Train<-createDataPartition(UB_col$Personal.Loan, p=0.6, list = TRUE)
Train <- UB_col[UB_Train, ]
Valid <- UB_col[-UB_Train, ]

#Feature Scaling
train_scale<- scale(train[,1:14])
test_scale<- scale(Valid[,1:14])

summary(Train)
NROW(Train)
NROW(Valid)
summary(UB_col$CreditCard)
#Step 3 - Navie Bayes classifier 

NB<- naiveBayes(Personal.Loan~UB_col$Online+UB_col$CreditCard, data = Train)



#Step 4 - evaluating model performance
#Step 5 - improving model performance
