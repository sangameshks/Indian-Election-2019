##################################################
## Project: Indian Election 2020
## Script purpose: Prediction on Indian Election 2019
## Date: 28/.3/2020
## Author: Sangamesh K S
## SessionInfo:R version 3.6.3 (2020-02-29)

##################################################

# Loading data
df<- read.csv("LS_2.0.csv")

#  Loading Required library -----------------------------------------------

library(rpart)
library(randomForest)
library(caret)
library(ROSE)
library(forcats)

# Data Prep ---------------------------------------------------------------

# Identifing NA values
colSums(is.na(df))
# Finding the mean of AGE
mean(df$AGE)
# Replacing NA's with zero
df$AGE<-fct_explicit_na(as.factor(df$AGE),"0")
# Finding nrows 
n = nrow(df)
# Removing name col
df<- df[-3]
# creating dummyvar
dmy <- dummyVars(WINNER~., data = df)
trsf <- data.frame(predict(dmy, newdata = df))
# Adding WINNER col
trsf$WINNER<-df$WINNER

# creating training  and testing data -------------------------------------

set.seed(123)
trainIndex = sample(1:n, size = round(0.75*n), replace=FALSE)
train = trsf[trainIndex ,]
test = trsf[-trainIndex ,]

# Running Model Rpart-----------------------------------------------------------

rprt_model <- rpart(as.factor(WINNER)~.,data = train)
rprt_pred <- predict(rprt_model,newdata = test,type = "class")
# ROC curve
roc.curve(rprt_pred,test$WINNER)
# Confusion Matrix
confusionMatrix(rprt_pred,as.factor(test$WINNER))

plot(rprt_model)

# Running Model Random Forest ---------------------------------------------


rand_model<-randomForest(as.factor(WINNER)~.,data = train)
rand_pred<- predict(rand_model,newdata= test,type="class")

roc.curve(rand_pred,test$WINNER)
# Confusion Matrix
confusionMatrix(rand_pred,as.factor(test$WINNER))

# Cross Validation--------------------------------------------------------

# Define training control -rpart
train.control <- trainControl(method="cv", number=10)
# Train the model
model <- train(as.factor(WINNER)~.,data = train, method = "rpart",
               trControl = train.control)

# Define training control -RandomForest
train.control <- trainControl(method="cv", number=10)
# Train the model
model <- train(as.factor(WINNER)~.,data = train, method = "randomForest",
               trControl = train.control)