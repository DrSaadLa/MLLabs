###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###                                                                      :::
###                       SIMPLE LINEAR REGRESSION                       :::
###                                                                      :::
###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Install and load broom package 
if (!require(broom)) install.packages("broom")

# Install ggplot2 then load it

# Install dslabs 

# Install gapminder 

# Install ISLR

# Install tidyverse

# install ggfortify


##################################################################
##              Load packages if already installed              ##
##################################################################
# Load dplyr
library(dplyr)
# load ggplot2

# load broom

# load ggfortify 

# load MASS

## Load Cars93 from MASS package
df <- 

##################################################################
##                       Data Exploration                       ##
##################################################################
# Use as many functions as you need to explore the data







##################################################################
##                      Variable Selection                      ##
##################################################################

# select Weight and MPG.highway variables and save to new data frame





##################################################################
##                      Summary Statistics                      ##
##################################################################
# Use summary with the new data frame



# Plot the data using ggplot 





# Add a linear fit line to the previous plot







##----------------------------------------------------------------
##               Fitting Simple Linear Regression               --
##----------------------------------------------------------------

model <- lm(MPG.highway ~ Weight, data = my_df)
model

##---------------------------------------------------------------
##                     Checking lm objects                     --
##---------------------------------------------------------------
# Show the components of lm object
str(model)
class(model)
typeof(model)
length(model)
names(model)

# Summary Function
summary(model)


# Coefficients Function



# Fitted function




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Manual Calculation                      ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# y_hat = b0_hat + b1_hat*x
coefs <- coefficients(model)
coefs
class(coefs)

# Now we have the coefficients
explan_var <- my_df$Weight

man_fit <- coefs[1] + coefs[2] * explan_var

head(man_fit)
head(fitted(model))

all.equal(man_fit, as.numeric(model$fitted.values))

##...............................................................
##                    Using mutate function                    ..
##...............................................................

my_new_df <- my_df %>% 
      mutate(fitted_vals = fitted(model)) %>% 
      mutate(man_fitted_vals = coefs[1] + coefs[2] * explan_var)
  

head(my_new_df)

# Residuals Function
resid <- residuals(model)
resid

# Compute the residuals manually
# errors = y - y_hat

man_resid <- my_df$MPG.highway - man_fit
head(man_resid)
head(resid)

names(resid) <- NULL

all.equal(man_resid, resid)

##...............................................................
##                    Using mutate function                    ..
##...............................................................

my_new_df <- my_new_df %>% 
  mutate(resids = residuals(model)) %>% 
  mutate(man_resid = my_df$MPG.highway - fitted(model))

head(my_new_df)

# anova function

anova(model)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                       Diagnostic Plots                       ++
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# All plots 
# Check the help 
?plot.lm

plot(model, which = 1:6)


# You can pass one by one 

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)
plot(model, which = 4)
plot(model, which = 5)
plot(model, which = 6)

##...............................................................
##               Plotting with autoplot function               ..
##...............................................................

require(ggfortify)

autoplot(model, which = 1:6)

# This function is useful as you can specify how to plot the plots
# for example we want three rows with 1 column
autoplot(model, 
         which = 1:3,
         nrow = 3, 
         ncol = 1)
#---
autoplot(model, 
         which = 4:6,
         nrow = 3, 
         ncol = 1)

# Predict function

pred <- predict(model, my_df)

head(pred)

# This is the same as fitted function
fit <- fitted(model)
head(fit)
all.equal(pred, fit)

# Or at once

all.equal(fitted(model), predict(model, my_df))


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###                                                                     ~~~
###                       BROOM PACKAGE FUNCTIONS                       ~~~
###                                                                     ~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##===============================================================
##                        tidy function                        ==
##===============================================================

# You can check the methods available to this function first
methods(tidy)
help("tidy.lm")

tidied_model <- tidy(model)
tidied_model

names(tidied_model)

# Add the argument confidence interval to tidy() function
tidy(model, conf.int = TRUE)

# Change the confidence level to 90%

tidy(model, conf.int = TRUE, conf.level = 0.90)

# Extract the model terms


# Extract the intercept


# Extract the slope



# Extract estimated coefficients 



# Extract The estimated intercept


# Extract the standard error. 




##================================================================
##                       Augment function                       ==
##================================================================

augmented_model <- augment(model)
augmented_model

# check the components of the augmented model (hint: use names())

names(augmented_model)

# Augment the model using the pipe %>% 

aug_model <- model %>% 
  augment()
# Extract the fitted values



# Use the pipe
model %>% 
  augment() %>% 
  pull(.fitted)

# Extract the residuals


# Extract the leverage


##===============================================================
##                       glance function                       ==
##===============================================================

glanced_model <- glance(model)
glanced_model


# Check the glance object (use names())

names(glanced_model)


# Extract the goodness-of-fit metric (Coefficient of determination)


# Extract the Coefficient of determination using '%>%'



# Extract the adjusted R2


# Use %>% 


# Extract the residual standard error (RSE)


# Use the pipe



# Extract the number of observations


# Use the pipe


