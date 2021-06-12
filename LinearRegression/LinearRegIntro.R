###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###                                                                      :::
###                       SIMPLE LINEAR REGRESSION                       :::
###                                                                      :::
###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing packages  ----------------------------------------------------
# The next packages are going to be used in this course
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidymodels)) install.packages("tidymodels")
if (!require(knitr)) install.packages("knitr")
if (!require(broom)) install.packages("broom")
if (!require(dslabs)) install.packages("dslabs")
if (!require(gapminder)) install.packages("gapminder")
if (!require(sigr)) install.packages("sigr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(Hmisc)) install.packages("Hmisc")
if (!require(psych)) install.packages("psych")
if (!require(ISLR)) install.packages("ISLR")  
if (!require(Hmisc)) install.packages("Hmisc")
if (!require(psych)) install.packages("psych")
if (!require(psych)) install.packages("pastecs")
# Installing swirl package 
if (!require(swirl)) install.packages("swirl")

# Visit the website for more information about swirl package
browseURL("https://github.com/swirldev/swirl_courses")

# For installing packages out of CRAN we use the devtools
install.packages("devtools")
# Installing the package data of the book Principles of Econometrics.
devtools::install_github("https://github.com/ccolonescu/PoEdata")

# Loading necessary packages ----------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(broom)


## Loading data
df <- MASS::Cars93

##################################################################
##                       Data Exploration                       ##
##################################################################
colnames(df)
str(df) 
glimpse(df)
head(df)
tail(df)
names(df)  
colnames(df)
nrow(df)
ncol(df)
summary(df)

##################################################################
##                      Variable Selection                      ##
##################################################################


my_df <- df %>% select(Weight, MPG.highway)
my_df %>% head(10)


# Rename the variables
my_df <- my_df %>% rename(wt = Weight, mpg = MPG.highway)

# Check the names 
names(my_df)
##################################################################
##                      Summary Statistics                      ##
##################################################################
summary(my_df)

# Using other packages 
Hmisc::describe(my_df)
psych::describe(my_df)
pastecs::stat.desc(my_df)

# More information 
pastecs::stat.desc(my_df, norm = TRUE, p = TRUE)

# Plotting the data

# Explain ggplot
dev.new()
ggplot(data = my_df, mapping = aes(x = wt, y = mpg)) + 
  geom_point(alpha = .5, color = 'blue')

dev.off()

my_df %>% ggplot(aes(MPG.highway, Weight)) +
  geom_point(color = "blue", alpha = 0.5)

# Adding a fitted line to the plot 

my_df %>% ggplot(aes(MPG.highway, Weight)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)


##----------------------------------------------------------------
##               Fitting Simple Linear Regression               --
##----------------------------------------------------------------

model <- lm(mpg ~ wt, data = my_df)
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

coefficients(model)

# Fitted function

fitted(model)


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
install.packages("ggfortify")
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
tidied_model$term

# Extract the intercept
tidied_model$term[1]

# Extract the slope

tidied_model$term[2]

# Extract estimated coefficients 

tidied_model$estimate

# Extract The estimated intercept
tidied_model$estimate[1]

# Extract the standard error. 

tidied_model$std.error


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

augmented_model$.fitted

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
glanced_model$r.squared

# Extract the Coefficient of determination using '%>%'
model %>% 
  glance() %>% 
  pull(r.squared)


# Extract the adjusted R2
glanced_model$adj.r.squared

# Use %>% 
model %>% 
  glance() %>% 
  pull(adj.r.squared)


# Extract the residual standard error (RSE)
glanced_model$sigma

# Use the pipe

model %>% 
  glance() %>% 
  pull(sigma)

# Extract the number of observations
glanced_model$nobs

# Use the pipe

model %>% 
  glance() %>% 
  pull(nobs)
