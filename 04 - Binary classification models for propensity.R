#--------------------------------------------------------------------------------------------------
#iLab1 code - Propensity scoring
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#Binary classification models for each of the events/programs
#--------------------------------------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(lubridate)
library(sqldf)
library(randomForest)
library(DMwR)
library(caret)
library(e1071)
library(pdp)

#--------------------------------------------------------------------------------------------------
#Modelling for RG
#--------------------------------------------------------------------------------------------------

#setwd("/home/rstudio-user/")

rg_base <- fread("rg_base.csv")

#--------------------------------------------------------------------------------------------------
#Simple model
set.seed(1287)

#Obtain indices for train-test split
train_indices <- sample(seq_len(nrow(rg_base)), size = floor(0.7*nrow(rg_base)))

#Set string and characters as factors
rg_base$status <- as.factor(rg_base$status)
rg_base$donor <- as.factor(rg_base$donor)
rg_base$pay_type <- as.factor(rg_base$pay_type)

#Set all NAs to zero
rg_base[is.na(rg_base)] <- 0

#Split into train and test based on the random indices
train <- rg_base[train_indices,]
test <- rg_base[-train_indices,]

#Simple logistic regression model on train

names(train)

logit <- glm(status ~ . - last_3 - dist_events
             - total_donated - total_count - total_amt_rg - count_rg
             ,data = train[,-1], family = binomial())

summary(logit)

#Predict for train dataset
pred <- predict(logit, newdata = train[,-1], type = "response")

#Obtain AUC score from the model
pred_ROCR <- ROCR::prediction(pred, train$status)

auc_ROCR <- ROCR::performance(pred_ROCR, measure = "auc")@y.values[[1]]


#Convert log odds to binary prediction
pred_b <- ifelse(pred > 0.5, 1, 0)

#Confusion matrix stats
glm_cm <- table(predicted = pred_b,true = train$status)[2:1,2:1]

confusionMatrix(glm_cm)

#Obtain predictions and get confusion matrix stats for test data
pred <- predict(logit, newdata = test[,-1], type = "response")

#Convert log odds to binary prediction
pred <- ifelse(pred > 0.5, 1, 0)

#Confusion matrix stats
glm_cm <- table(predicted = pred,true = test$status)[2:1,2:1]

confusionMatrix(glm_cm)

#--------------------------------------------------------------------------------------------------

#Random forest on unSMOTEd data
rand <- randomForest(status ~ avg_inc + pay_ratio + pay_type + fail_ratio
                     + dist_events + count_rg + dist_cards + participated_events
                     + tenure + total_amt_rg, 
                     data = train, 
                     importance = TRUE, ntree = 100, keep.forest = T)

importance(rand)

varImpPlot(rand, type = 2)

rand #OOB error estimate = 0.18%

#ROC-AUC metrics
pre <- predict(rand, newdata = train, type = "prob")

pred_ROCR <- ROCR::prediction(pre[,2], train$status)

auc_ROCR <- ROCR::performance(pred_ROCR, measure = "auc")@y.values[[1]]


#Confusion matrix stats
train$pred <- rand$predicted

rf_cm <- table(predicted = train$pred,true = train$status)[2:1,2:1]

confusionMatrix(rf_cm)

#On test
#Confusion matrix stats
test$pred <- predict(rand, newdata = test, type = "response")

rf_cm <- table(predicted = test$pred,true = test$status)[2:1,2:1]

confusionMatrix(rf_cm)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#SMOTE to improve class balance
new_train <- SMOTE(status ~ ., data = as.data.frame(train), 
                   perc.over = 1600, perc.under = 100)

gc()

#--------------------------------------------------------------------------------------------------
#Logistic regression on SMOTE data
names(new_train)

logit <- glm(status ~ . - last_3 - dist_events
             - total_donated - total_count - total_amt_rg - count_rg
             ,data = new_train[,-1], family = binomial())

summary(logit)

#Obtain predictions train data
pred <- predict(logit, newdata = new_train[,-1], type = "response")

#Obtain ROC-AUC scores
pred_ROCR <- ROCR::prediction(pred, new_train$status)

auc_ROCR <- ROCR::performance(pred_ROCR, measure = "auc")@y.values[[1]]

#Convert log odds to binary predictions
pred_b <- ifelse(pred > 0.5, 1, 0)

#Confusion matrix stats
glm_cm <- table(predicted = pred_b,true = new_train$status)[2:1,2:1]

confusionMatrix(glm_cm)

#Perform the same process to obtain goodness-of-fit metrics for test
pred <- predict(logit, newdata = test[,-1], type = "response")

#Convert log odds predictions to binary
pred <- ifelse(pred > 0.5, 1, 0)

#Confusion matrix stats
glm_cm <- table(predicted = pred,true = test$status)[2:1,2:1]

confusionMatrix(glm_cm)

#--------------------------------------------------------------------------------------------------
#Random forest on SMOTEd data
rand <- randomForest(status ~ . - last_3 - dist_events
                     - total_donated - total_count - total_amt_rg - count_rg
                     , data = new_train[,-1], 
                     importance = TRUE, ntree = 100, keep.forest = T)

importance(rand)

varImpPlot(rand, type = 2)

rand #OOB error estimate = 0.12%

#ROC-AUC metrics
pre <- predict(rand, newdata = new_train, type = "prob")

pred_ROCR <- ROCR::prediction(pre[,2], new_train$status)

auc_ROCR <- ROCR::performance(pred_ROCR, measure = "auc")@y.values[[1]]


#Confusion matrix stats
new_train$pred <- rand$predicted

rf_cm <- table(predicted = new_train$pred,true = new_train$status)[2:1,2:1]

confusionMatrix(rf_cm)


#Test confusion matrix
test$pred <- predict(rand, newdata = test, type = "response")

rf_test_cm <- table(predicted = test$pred,true = test$status)[2:1,2:1]

confusionMatrix(rf_test_cm)

#--------------------------------------------------------------------------------------------------
#Partial dependence plots

#1: count_rg
partial(rand, pred.var = "count_rg", plot = T, prob = T, which.class = 2)

#2: dist_cards
partial(rand, pred.var = "dist_cards", plot = T, prob = T, which.class = 2)

#3: yrs_donated
partial(rand, pred.var = "yrs_donated", plot = T, prob = T, which.class = 2)