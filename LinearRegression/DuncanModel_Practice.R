# load Necessary libraries 


library("car") 
library(carData)


head(Duncan, n=10)
dim(Duncan)


# Obtain summary statistics for the variables in `Duncan`:

summary(Duncan)


# As a first graph, we view a histogram of the variable prestige

Duncan %>% ggplot(aes(x = prestige)) +
  geom_histogram()



## 1.5.1 Examining the Data

# The `scatterplotMatrix()` function in the **car** package produces 
#scatterplots for all paris of variables. 
#A few relatively remote points are marked 
# by case names, in this instance by occupation.

scatterplotMatrix(~ prestige + education + income, 
                   id=list(n=3), data=Duncan)


## 1.5.2 Regression Analysis

# We use the`lm()` function to fit a linear regression model to the data:

duncan_model <- lm(prestige ~ education + income, data=Duncan)


# Model Summary
summary(duncan.model)

# The `brief()` and `S()` functions, both in the **car** package, provide 
# alternative summaries of a regression fit.

brief(duncan_model)

S(duncan_model)

## 1.5.3 Regression Diagnostics

# The `rstudent()` function returns studentized residuals, 
# and the `densityPlot()` function fits an adaptive kernel density 
# estimator to the distribution of the studentized residuals:

densityPlot(rstudent(duncan_model))


# A `qqPlot()` can be used as a check for nonnormal errors, 
# comparing the studentized residuals to a t-distribution:

qqPlot(duncan_model, id=list(n=3))


# Test For the outliers

outlierTest(duncan_model)


# This graph displays influence measures in index plots:

influenceIndexPlot(duncan_model, vars=c("Cook", "hat"), 
                   id=list(n=3))


# Added-variable plots for Duncan's regression, looking for influential cases:


avPlots(duncan_model, 
    id=list(cex=0.75, n=3, method="mahal"))


# Component-plus-residual plots for the regression, checking for nonlinearity:


crPlots(duncan_model, smooth=list(span=0.7))


# Tests for non-constant error variance:


ncvTest(duncan_model)
ncvTest(duncan.model, var.formula= ~ income + education)


# Removing the cases `"minister"` and "`conductor'":


whichNames(c("minister", "conductor"), Duncan)
duncan_model_2 <- update(duncan_model, subset=-c(6, 16))
summary(duncan_model_2)


# Comparing the regressions with and without these two cases:

compareCoefs(duncan_model, duncan_model_2)

