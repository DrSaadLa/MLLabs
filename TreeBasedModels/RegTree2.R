###*************************************************************************
###*************************************************************************
###                                                                      ***
###                           REGRESSION TREES                           ***
###                              LECTURE 2                               ***
###                            RPART PACKAGE                             ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
###*                Loading packages
###*                ----------------

require(rpart)
require(rpart.plot)
require(dslabs)
require(tidyverse)
require(caret)

##****************************************************************
##                      Motivation Example                      **
##****************************************************************
help(package='rpart')
data("cu.summary")
rt_rpart <- rpart(Mileage ~ Price + Country + Reliability + Type,
                  method = 'anova', 
                  data = cu.summary)
# 
plot(rt_rpart, uniform = TRUE, margin = 0.1)
text(rt_rpart, use.n = TRUE, all = TRUE, cex=.5)

## Using rplot.plot
?rpart.plot

rpart.plot(rt_rpart)

# Adding yesno arg to control yes/no text (0, 1, 2)

rpart.plot(rt_rpart, yesno = 0)
rpart.plot(rt_rpart, yesno = 1)
rpart.plot(rt_rpart, yesno = 2)

# Adding type arg to control the labels (0 to 5)

rpart.plot(rt_rpart, yesno = 2, type = 1)

# Adding extra information with extra argument (1 to 11)

rpart.plot(rt_rpart, 
           yesno = 2,
           type = 1,
           extra = 1) # number of observation


# For combining extra information use 100 + (1 to 11)

rpart.plot(rt_rpart, 
           yesno = 2,
           type = 1,
           extra = 101) # percentage plus number of obs

# Controlling the text with cex arg

rpart.plot(rt_rpart, 
           yesno = 1,
           type = 1,
           extra = 1, 
           cex = 0.75)
##****************************************************************
##                          Example 02                          **
##****************************************************************
library(mlbench, quietly = TRUE)
data("BostonHousing")
str(BostonHousing)

# Fit the model
fit_boston <- rpart()

# Print the model

# check the properties of the model using names() function  


# Plot the results
plot()
text()

# or
rpart.plot()


##***************************************************************
##                   Regression Tree Process                   **
##***************************************************************
library(dslabs, quietly = TRUE)
data("polls_2008")
?polls_2008

## Fitting a regression tree model 

rt_fit <- rpart(margin ~ . ,
                method = 'anova',
                data = polls_2008)

rpart.plot(rt_fit, 
           yesno = 1,
           type = 0,
           extra = 1, 
           cex = 0.75)
## Prediction

polls_2008 %>% mutate(y_hat = predict(rt_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), color = "red")

##****************************************************************
##                   rpart.control() function                   **
##****************************************************************
?rpart.control

# Changing parameters

cp_fit <- rpart(margin ~ . , data = polls_2008, 
                control = rpart.control(cp = 0, minsplit = 2))

# Plotting prediction 
polls_2008 %>% mutate(y_hat = predict(cp_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day,y_hat), color = "red")

# - -----------------------------------------------------------------------
## Example 02:

## Control splitting for BostonHousing dataset with the control arg

cp_fit_boston <- rpart( , control = )

rpart.plot(cp_fit_boston)




##***************************************************************
##     Cross Validation for determining complex parameter     **
##***************************************************************
library(caret)

train_rpart <- train(margin ~ .,
                     method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)


names(train_rpart)
ggplot(train_rpart)

# Access the final model

train_rpart$finalModel

# The best Tune

train_rpart$bestTune

# Plotting the final model 

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.8)

# Plotting the prediction 

polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) + 
  geom_step(aes(day, y_hat), color = "red")

##****************************************************************
##                      Pruning the model                       **
##****************************************************************

best_cp <- as.numeric(train_rpart$bestTune)
prune_fit <- prune(fit, cp = best_cp)

summary(prune_fit)
names(prune_fit)


plot(prune_fit, margin = 0.1, compress = TRUE)
text(prune_fit, mex = 0.75)
