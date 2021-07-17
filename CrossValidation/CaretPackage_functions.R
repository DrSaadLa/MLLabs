###*************************************************************************
###*************************************************************************
###                                                                      ***
###                        CARET PACKAGE OVERVIEW                        ***
###                                                                      ***
###*************************************************************************
###*************************************************************************

library(caret)
library(dslabs)

##****************************************************************
##                 CreateDataPartition Function                 **
##****************************************************************
# Get the Arguments
args(createDataPartition)

# Splitting Data Using Different ways

set.seed(234)
data("mtcars", package = 'datasets')
y <- mtcars$mpg                      # Setting y for simplicity

# Sampling one time
set.seed(234)
ind_1 <- createDataPartition(y, times = 1, p = 0.7, list = FALSE)
str(ind_1)
head(ind_1)

# Repeating Splitting 3 times 
set.seed(234)
ind_3 <- createDataPartition(y, times = 3, p = 0.7, list = FALSE)
str(ind_3)
head(ind_3)

# If you need the results as a list set the argument list = TRUE 
set.seed(234)
ind_3_list <- createDataPartition(y, times = 3, p = 0.7, list = TRUE)
str(ind_3_list)
head(ind_3_list)

##****************************************************************
##                        Train Function                        **
##****************************************************************
##*                  Example 01:
##*                  ----------

data(Boston, package = 'MASS')

# Splitting the data
set.seed(234)
index <- createDataPartition(Boston$medv, p = 0.75, list = FALSE)
training <- Boston[index, ]
testing <- Boston[-index, ]

train_lm <- train(medv~ . , method = "lm", data = training)
train_rpart <- train(medv~ . , method = "rpart", data = training)
train_knn <- train(medv~ . , method = "knn", data = training)
train_knn <- train(medv~ . , method = "nnet", data = training)

# Checking the models 
summary(train_lm)
summary(train_rpart)
summary(train_knn)
##****************************************************************
##*                  Example 02:
##*                  ----------
# Loading data
library(dslabs)
data("mnist_27")

train_glm <- train(y ~ . , method = "glm", data = mnist_27$train)
train_knn <- train(y ~ . , method = "knn", data = mnist_27$train)
train_lda <- train(y ~ . , method = "lda", data = mnist_27$train)
train_qda <- train(y ~ . , method = "qda", data = mnist_27$train)

summary(train_glm)
summary(train_knn)
names(train_knn)

##****************************************************************
##           Prediction with Predict.train() function           **
##****************************************************************
##*            Linear Model Predictions
##*            ------------------------

pred_lm <- predict(train_lm ,  newdata = testing, type = "raw")
pred_rpart <- predict(train_rpart ,  newdata = testing, type = "raw")

##****************************************************************
##*            GLM and KNN Predictions
##*            ------------------------

pred_glm <- predict(train_glm, newdata = mnist_27$test, type = 'raw')
pred_knn <- predict(train_knn, newdata = mnist_27$test, type = 'raw')


##///////////////////////////////////////////////////////////////
##                          Self-study                         //
##                      Model Evaluation                       //
##///////////////////////////////////////////////////////////////

# Evaluate linear Model by predicting on the training and testing set
library(Metrics)

pred_lm_train <- predict(train_lm ,  newdata = training, type = "raw")
pred_lm_test <- predict(train_lm ,  newdata = testing, type = "raw")

mae(training$medv, pred_lm_train)
mae(testing$medv, pred_lm_test)

rmse(training$medv, pred_lm_train)
rmse(testing$medv, pred_lm_test)

# Regression Tree Model 

pred_rpart_train <- predict(train_rpart ,  newdata = training, type = "raw")
pred_rpart_test <- predict(train_rpart ,  newdata = testing, type = "raw")

mae(training$medv, pred_rpart_train)
mae(testing$medv, pred_rpart_test)

rmse(training$medv, pred_rpart_train)
rmse(testing$medv, pred_rpart_test)

# Evaluating logistic and knn algorithms using:
# Confusion Matrix 

confusionMatrix(pred_glm, reference = mnist_27$test$y)
confusionMatrix(pred_knn, reference = mnist_27$test$y)
# Or just the accuracy 
confusionMatrix(pred_glm, 
                reference = mnist_27$test$y)$overall[['Accuracy']]

confusionMatrix(pred_knn, 
                reference = mnist_27$test$y)$overall[['Accuracy']]

##===============================================================
##                     Important Functions                     ==
##===============================================================

# Info about the model you train 

getModelInfo("knn")
getModelInfo("glm")


# Or quick Lookup 
modelLookup("knn")

##****************************************************************
##                          Resampling                          **
##                          Self-study                          **
##****************************************************************
library(AppliedPredictiveModeling)
data(twoClassData, package = "AppliedPredictiveModeling")

str(predictors)
str(classes)

# Set the seed to reproduce the results
set.seed(234)

trainingRows <- createDataPartition(classes, p = 0.8, 
                                    list = FALSE)
trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]

testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]

# Checking the splits

str(trainPredictors)
str(testPredictors)

length(trainClasses)
length(testClasses)

# Repeated Splits
set.seed(234)
repSplits <- createDataPartition(trainClasses, p = 0.8, 
                                 times = 3)
str(repSplits)
# Or you can do that using createResample as follows 

# Bootstrapping 
bootSampling <- createResample(classes, times = 3, list = TRUE)
str(bootSampling)

# Cross-validation Splitting 
set.seed(4)
cvSplits <- createFolds(trainClasses, k = 10, 
                        returnTrain = TRUE) 
str(cvSplits)

# Repeated cross-validation 

repCV <- createMultiFolds(classes, k = 5, times = 2)
str(repCV)

###*************************************************************************
###*************************************************************************
###                                                                      ***
###                           CROSS VALIDATION                           ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
library(tictoc)
data("mtcars")
set.seed(234)
tic()
lm_10cv <- train(mpg ~ wt + hp, data = mtcars, 
                 method = "lm", 
                 trControl = trainControl(
                   method = "cv", 
                   number = 10, 
                   verboseIter = TRUE    # for showing the results while estimating
                 )
                 )
toc()
names(lm_10cv)
print(lm_10cv) # Equivalent to print.train()


# Example 02: 
# ----------
# Here we use a different syntax, instead of writing a long code, 
# we separate some parts 
data(Boston, package = "MASS")
set.seed(234)
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     verboseIter = TRUE)
tic()
boston_CV <- train(medv ~ . , 
                      data = Boston, 
                      method = "lm", 
                      trControl = ctrl)
toc()
print(boston_CV)

##***************************************************************
##                  Repeated Cross-Validation                  **
##***************************************************************
# Boston Example
set.seed(45)
tic()
boston_repCV <- train(medv ~ . , data = Boston, 
                 method = "lm", 
                 trControl = trainControl(
                   method = "repeatedcv", 
                   number = 10, 
                   repeats = 10, 
                   verboseIter = TRUE    
                 )
)
toc()

print(boston_repCV)
print(boston_CV)

# Predict on the entire data set
pred <- predict(boston_CV, newdata = Boston)
head(pred)

##****************************************************************
##      YOU CAN TRAIN OTHER MODELS USING DIFFERENT METHODS      **
##****************************************************************
##*                 Recursive Partitioning or RPART
##*                 -------------------------------

set.seed(45)
tic()
boston_rpart <- train(medv ~ . , data = Boston, 
                      method = "rpart", 
                      trControl = trainControl(
                        method = "cv", 
                        number = 10, 
                        verboseIter = TRUE    
                      )
)
toc()
print(boston_rpart)

##*                 Random Forest 
##*                 --------------
set.seed(45)
tic()
boston_rf <- train(medv ~ . , data = Boston, 
                      method = "rf", 
                      trControl = trainControl(
                        method = "cv", 
                        number = 10, 
                        verboseIter = TRUE    
                      )
)
toc()
print(boston_rf)

##*                 Bagging (Bootstrap Aggregation) 
##*                 ------------------------------
set.seed(45)
tic()
boston_knn <- train(medv ~ . , data = Boston, 
                   method = "knn", 
                   trControl = trainControl(
                     method = "cv", 
                     number = 10, 
                     verboseIter = TRUE    
                   )
)
toc()
print(boston_ridge)

#------------------------------------------------------------------------------
data("diamonds", package = "ggplot2")
dim(diamonds)

set.seed(123)
tic()
price_model <- train(price ~ . , data = diamonds, 
                     method = "lm", 
                     trControl = trainControl(
                       method = "repeatedcv", 
                       number = 10,
                       repeats = 5,
                       verboseIter = TRUE
                     ))
toc()
print(price_model)

