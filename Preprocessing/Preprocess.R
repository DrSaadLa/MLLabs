###*************************************************************************
###*************************************************************************
###                                                                      ***
###                          DATA PREPROCESSING                          ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
library(mlbench)
library(rattle)
data("wine")


apply(wine, 2, FUN = var)




load("preproc.rda")

##****************************************************************
##                   Distribution Exploration                   **
##****************************************************************
##*                 1. Histogram
##*                 ------------
par(mfrow = c(3, 1))

hist(preproc$Age)
hist(preproc$ConvertedSalary)
hist(preproc$Years.Experience)


##*                 2. Boxplot
##*                 ----------

boxplot(preproc$Age)
boxplot(preproc$ConvertedSalary)
boxplot(preproc$Years.Experience)

## Pairplot

psych::pairs.panels(preproc)

par(mfrow = c(1,1))

##****************************************************************
##                      Log Transformation                      **
##****************************************************************

par(mfrow = c(2, 1))

# Age variable
# -----------
hist(preproc$Age)
hist(log(preproc$Age))

# Salary variable
#---------------
hist(preproc$ConvertedSalary)
hist(log(preproc$ConvertedSalary))
par(mfrow = c(1,1))


## Checking the statistics after the log transformation

summary(preproc$ConvertedSalary)
summary(log(preproc$ConvertedSalary))

# Variables with high variance: 
# ----------------------------

load("chemicals.rda")

apply(chemicals, 2, var)
apply(chemicals, 2, sd)


##***************************************************************
##                       Standardization                       **
##***************************************************************

scaled <- scale(preproc, na.rm = T)

summary(scaled)


apply(scaled, 2, FUN = sd, na.rm = T)
apply(scaled, 2, FUN = mean, na.rm = T)

##***************************************************************
##                        Normalization                        **
##***************************************************************

normalize <- function(x, na.rm = TRUE) {
  nom <- x- min(x, na.rm = na.rm) 
  denom <- max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
  return(nom / denom)
}

normalized <- apply(preproc, 2, FUN = normalize, na.rm = TRUE)

summary(normalized)


##****************************************************************
##                 Preprocess Function of caret                 **
##****************************************************************

std_prepr <- preProcess(preproc, 
                       method = c('center', 'scale'))

std_data <- predict(std_data, preproc)

summary(std_data)
apply(std_data, 2, FUN = sd, na.rm = T)

psych::pairs.panels(std_data)
hist(std_data$ConvertedSalary)

# If we want to scale on or more variable using preProcess

one_prepr <- preProcess(preproc[, 1, drop = F], 
                      method = c('center', 'scale'))
one_std <- predict(one_var, preproc[, 1, drop = F])
summary(one_std)
sd(one_std$Age)

# MinMax Scaler -----------------------------------------------------------

minmax_prepr <- preProcess(preproc, method = 'range')
minmax_scaled <- predict(minmax_prepr, preproc)

summary(minmax_scaled)

hist(minmax_scaled$ConvertedSalary)

