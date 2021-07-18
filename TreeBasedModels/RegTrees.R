############################################################################
############################################################################
###                                                                      ###
###                       FITTING REGRESSION TREES                       ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)
library(caret)
library(tree)
library(rpart)
library(rpart.plot)
library(ISLR)

# Loading the data

data("Boston", package = "MASS")
str(Boston)
contents(Boston)


# Splitting the data ------------------------------------------------------
set.seed(1)
index <- createDataPartition(Boston$medv, times = 1, p = 0.7, 
                             list = FALSE)
train_set <- Boston[index, ]
test_set <- Boston[-index, ]


# Fitting tree model ------------------------------------------------------

tree_boston_model <- tree(medv ~ . , data= train_set)

summary(tree_boston_model)

print(tree_boston_model)


# ======================= Explore the model ================================
str(tree_boston_model)
length(tree_boston_model)
names(tree_boston_model)
tree_boston_model$frame
tree_boston_model$where
tree_boston_model$terms
tree_boston_model$call
tree_boston_model$y
tree_boston_model$weights

# Plotting the tree
?plot.tree
?text.tree

plot(tree_boston_model)
text(tree_boston_model, pretty=0)

# Prunning the tree
cv_boston <- cv.tree(tree_boston_model)
names(cv_boston)
cv_boston$size
cv_boston$dev
cv_boston$k
cv_boston$method

# Plotting the Cross-Validation

plot(cv_boston$size,cv_boston$dev,
     type='b', lty = 2, col = "blue", lwd = 2)

df <- data.frame(size = cv_boston$size, dev = cv_boston$dev)

ggplot(data = df, aes(size, dev)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3)


# Prunning the Tree

prune_boston <- prune.tree(tree_boston_model,best=5)
plot(prune_boston)
text(prune_boston,pretty=0)

yhat <- predict(tree_boston_model, newdata = Boston[-train, ])
boston_test <- Boston[-train,"medv"]

plot(yhat,boston_test)
abline(0,1)
mean((yhat - boston_test)^2)

ggplot(data.frame(yhat, boston_test), aes(yhat, boston_test)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0)

##***************************************************************
##                     Using rpart package                     **
##***************************************************************

library(rpart)

rpart_boston <- rpart(medv ~ ., data = train_set)
plot(rpart_boston)
text(rpart_boston, pretty = 0)
summary(rpart_boston)


