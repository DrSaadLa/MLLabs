###************************************************************************
###************************************************************************
###                                                                     ***
###                    REGRESSION TREE FINAL PROJECT                    ***
###                SCHOOLS GRADES FROM UCI ML REPOSITORY                ***
###                                                                     ***
###************************************************************************
###************************************************************************

data_url <- browseURL(paste0("https://archive.ics.uci.edu",
                          "/ml/datasets/Student+Performance"))

# Loading Packages
require(tidyverse)
require(caret)
require(rpart)
require(rpart.plot)

# Reading the data from the wed

grades <- read.csv(paste0("https://raw.githubusercontent.com/",
                         "DrSaadLa/MLLabs/main/data/grades.csv"), 
                   stringsAsFactors = TRUE)

# Save the data for a later use on your hard drive

# save(grades, file = "grades.rda")

##****************************************************************
##                       Data Exploration                       **
##****************************************************************

glimpse(grades)
head(grades)

Hmisc::describe(grades)

# Splitting the data ------------------------------------------------------
set.seed(234)
ind <- createDataPartition(grades$final_grade, 
                           times = 1, 
                           p = 0.7, 
                           list = FALSE)
train_grade <- grades[ind, ]
  
test_grade  <- grades[-ind, ]


# Fitting a regression Tree Model

rt_model <- rpart(final_grade ~ . , 
            method = 'anova', 
            data = train_grade)

# Look at the model output                      
print(rt_model)

# Plotting the fitted RT model 
rpart.plot(x = rt_model, yesno = 1, type = 0, extra =0)

##****************************************************************
##                       Model Evaluation                       **
##****************************************************************
##*                 We use MAE and RMSE
##*                 --------------------
library(Metrics)  

pred <- predict(object = rt_model,   
                newdata = test_grade)

mae <- mae(actual = test_grade$final_grade, 
                    predicted = pred)

rmse <- rmse(actual = test_grade$final_grade, 
            predicted = pred)

mae; rmse

##****************************************************************
##                         Model Tuning                         **
##****************************************************************
# Accessing the cp can be done through cptable 

cps <- rt_model$cptable
cps

# Extracting the best cp from the xerror column

best_cp_index <- which.min(rt_model$cptable[, 'xerror'])
best_cp <- rt_model$cptable[best_cp_index, "CP"]

best_cp

rt_tuned <- prune(rt_model, cp = best_cp)
rpart.plot(rt_tuned, yesno = 1, extra = 0, type = 1)


##***************************************************************
##             Gridsearch for model hyperparameter             **
##***************************************************************
##*                   GridSearch Manually
##*                   -------------------

# take minsplit and maxdepth hyparameter
minsplit <- seq(1, 5, 1)                # take a longer grid in real world
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations 
grid_search <- expand.grid(minsplit = minsplit,
                          maxdepth = maxdepth)

nrow(grid_search)

# Initialize an empty list
grade_models <- list()

# WE write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(grid_search)) {
  
  # Get minsplit, maxdepth values at row i
  minsplit <- grid_search$minsplit[i]
  maxdepth <- grid_search$maxdepth[i]
  
  # Train a model and store in the list
  grade_models[[i]] <- rpart(formula = final_grade ~ ., 
                             data = train_grade, 
                             method = "anova",
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}

##***************************************************************
##                       Grid Evaluation                       **
##***************************************************************

# In this part we introduce a new concept. A validation set 
# just to mimic the cross validation technique. 
# I will split the test set into 2 splits, We could have done this
# at the beginning. But we are doing it now. Eventually we end up 
# having 15% validation, 15% test, 70% training

set.seed(234)
valid_ind <- createDataPartition(test_grade$final_grade, 
                                 times = 1, 
                                 p = 0.15, 
                                 list = FALSE)
valid_grade <- test_grade[valid_ind, ]
test_grade <- test_grade[-valid_ind, ]


library(Metrics)                              # For the metrics
# Create an empty vector to store RMSE values
rmse_values <- c()

# Write a loop over the models to compute validation RMSE
for (i in 1:nrow(grid_search)) {
  model <- grade_models[[i]]
  # Predict on grade_valid 
  pred <- predict(object = model,
                  newdata = valid_grade)
  # Compute validation RMSE and add to the rmse_values
  rmse_values[i] <- rmse(actual = valid_grade$final_grade, 
                         predicted = pred)
}


##****************************************************************
##                   Selecting the best model                   **
##****************************************************************

# Identify the model with smallest validation set RMSE
class(grade_models)
length(grade_models)

# Find the minimum rmse_values 
min_val_rmse <- which.min(rmse_values)

# Select The best model
grade_models[[min_val_rmse]]

# Simply in one step like this 
best_model <- grade_models[[which.min(rmse_values)]]

# Check the model attributes
names(best_model)

# Print the model parameters of the best model
best_model$control[c('minsplit', 'maxdepth')]

ctrl <- sapply(best_model$control, as.numeric)
ctrl
# Evaluate the model on the test set
pred <- predict(object = best_model,
                newdata = test_grade)
rmse(actual = test_grade$final_grade, 
     predicted = pred)

##***************************************************************
##                    Gridsearch with caret                    **
##***************************************************************
# Find the hyperparameter to tune in caret using modelLookup() function

modelLookup("rpart")

# Construct the grid
my_grid <- expand.grid(cp = seq(0, 0.05, length = 20))

# The control
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)
# The Grid Search
set.seed(144)
gs <- train(final_grade ~ . , 
            method = "rpart",
            data = grades, 
            tuneGrid = my_grid, 
            trControl = fitControl)
gs$finalModel

rpart.plot(gs$finalModel, yesno = 1, extra = 0, type = 1)
