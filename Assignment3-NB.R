#Installing the libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(e1071)
library(caret)
library(gmodels)
library(reshape2)

#Setting up the working directory

setwd("C:/Users/nihar/Desktop")

#Loading the universalbank dataset
UniversalBank<- read.csv("UniversalBank.csv", header = TRUE)
head(UniversalBank)

#partitioning data between training set(60) and validation set(40)

set.seed(15)
split<- createDataPartition(UniversalBank$Personal.Loan, p=0.6, list = FALSE)
train<- UniversalBank[1:3000, ]
Valid<- UniversalBank[3001:5000, ]
NROW(train)
NROW(Valid)

#Feature Scaling: Scale is a default method to scale the columns of a numeric matrix
train_scale<- scale(train[,1:14])
Valid_scale<- scale(Valid[,1:14])

# a)creating pivot table using melt() and cast(). "melt" data so that each row is a unique id-variable combination. 
#Then "cast" the melted data into any shape.

Melt.UBbank<- melt(train, id=c("CreditCard","Personal.Loan"), variable = "Online")
cast.UBbank<-dcast(Melt.UBbank, CreditCard+Personal.Loan~Online)
cast.UBbank[,c(1:2,14)]

#b)This is the probability of loan acceptance (Loan = 1) conditional on having a bank credit card (CC = 1) and being an active user of online banking services (Online = 1)
# is 54/(54+477)= 54/531 = 0.101
table(train[,c(13,14,10)])

#c) Create two separate pivot tables for the training data. One will have Loan (rows) as a function of Online (columns) and the other will have Loan (rows) as a function of CC.

Melt.UBbankc1 = melt(train, id=c("Online"), variable = "Personal.Loan") # Loan (rows) as a function of Online (columns)
Melt.UBbankc2 = melt(train, id=c("CreditCard"), variable = "Personal.Loan") # Loan (rows) as a function of CC

recast.UBbankc1=dcast(Melt.UBbankc1, Online~Personal.Loan) #cast the melted data
recast.UBbankc2=dcast(Melt.UBbankc2, CreditCard~Personal.Loan)

#d)Compute the following quantities [P(A | B) means "the probability of A given B"]:

table(train[,c(14,10)])#credit card against personal.loan
table(train[,c(13,10)])
table(train[,c(13,14)])#online against CC
table(train[,c(13)])#Online
table(train[,c(14)])#credit card
table(train[,c(10)])#personal.loan

#Calculating values based on above outputs
#i. P(CC = 1 | Loan = 1) (the proportion of credit card holders among the loan acceptors) = 89/(89+220) = 89/309 = 0.288
#ii. P(Online = 1 | Loan = 1) = 193/(193+116) = 193/309 = 0.624
#iii. P(Loan = 1) (the proportion of loan acceptors) = 309/(309+2691) = 309/3000 = 0.103
#iv. P(CC = 1 | Loan = 0) = 770/(770+1921) = 770/2691 = 0.286
#v. P(Online = 1 | Loan = 0) = 1617/(1617+1074) = 1617/2691 = 0.6
#vi. P(Loan = 0) = 2691/(2691+309) = 2691/3000 = 0.897


#e)Use the quantities computed above to compute the naive Bayes probability P(Loan = 1 | CC = 1, Online = 1).

#p(loan=0) = 2691
#p(loan=1) = 309
#p(cc,loan = 1)=89
#p(cc=1,loan=0)=770
#p(cc=0,loan=1)=220
#p(online,loan = 1)=193
#p(online=1,loan=0)=1617
#p(online=0,loan=1)=116

# P(Loan = 1 | CC = 1, Online = 1) = ((89/89+220)*(220/220+89)*(309/309+2691))/((89/89+220)*(220/220+89)*(309/309+2691))+((770/770+1921)*(1617/1617+1074)*(2691/2691+309))
# =((89/309)*(220/2141)*(309/3000))/((89/309)*(220/2141)*(309/3000))+((770/2691)*(1617/2691)*(2691/3000))
# =(0.288*0.711*0.103)/((0.288*0.711*0.103)+(0.286*0.6*0.897)) = 0.021/0.021+0.00046 = 0.021/0.02146 =0.978


# f)Compare this value with the one obtained from the pivot table in (B). Which is a more accurate estimate?
#the value obtained from pivot table is 0.101~0.978 . Both the vales are approximately equal.


#Implementing Naive Bayes Model
set.seed(120)
NB_model<- naiveBayes(Personal.Loan~CreditCard+Online, data = train)
NB_model

# the Navie Bayes out is nearly equal to the output derived from e.
#0.103~0.978





