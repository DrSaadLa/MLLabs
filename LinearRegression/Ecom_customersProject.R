###########################################################################
###########################################################################
###                                                                     ###
###                     ECOMMERCE CUSTOMERS PROJECT                     ###
###                          LINEAR REGRESSION                          ###
###                                                                     ###
###########################################################################
###########################################################################
###*                        Loading Packages
###*                        ----------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(corrplot))


##===============================================================
##                     Reading in the data                     ==
##===============================================================

e_customers <- read.csv("Ecommerce_Customers.csv")

# Explore the data set 
# Data contents
Hmisc::contents(e_customers)

glimpse(e_customers)

head(e_customers, 10)

tail(e_customers)

car::brief(e_customers)

##================================================================
##                EDA: Exploratory Data Analysis                ==
##================================================================

cust_data <- e_customers[, 4:ncol(e_customers)]

# Check the column names again, 
colnames(cust_data)            # The names contain dots, I will substitute 
                               # each dot with underscore "_"

var_names <- gsub("\\.", "_", colnames(cust_data))
var_names[1] <- "Avg_Session_Length"

##---------------------------------------------------------------
##                  "sub" -- "gsub" functions                  --
##---------------------------------------------------------------
# These functions belong to regular expressions or pattern matching 
# and replacement 
# sub function replaces the first match 
sub('r', 's', x = "roar")

# gsub function replaces all matches 

gsub('r', 's', x = "roar")

# There are many characters that represent matching. 
# see documentation a good website that teaches regular expressions
#==================================================================

colnames(cust_data) <- var_names
colnames(cust_data)

# Get the summary statistics of the variables
summary(cust_data)
Hmisc::describe(cust_data)
psych::describe(cust_data, skew = TRUE, 
                IQR = TRUE)

# The Dependent Variables in this study is "Yearly_Amount_Spent"
# We are going to explore the distribution of this variable

ggplot(cust_data, aes(x = Yearly_Amount_Spent)) + 
  geom_histogram(aes(y = ..density..), 
                 fill = "steelblue") +
  geom_density(color = "red", lwd = 1.2) + 
  stat_function(fun = dnorm, args = list(mean = mean(cust_data$Yearly_Amount_Spent), 
                            sd = sd(cust_data$Yearly_Amount_Spent)),
                color = "orange", lwd = 1.2) +
  theme_linedraw()

#################################################################
##                        Joint Graphs                         ##
#################################################################

library(WVPlots)

# Membership VS TimeOnApp

ScatterHist(cust_data, title = "Yearly_Amount_Spent VS TimeOnApp", 
            xvar = "Time_on_App", yvar = "Yearly_Amount_Spent", 
            smoothmethod = "lm")

# Yearly_Amount_Spent VS TimeOnWebsite
ScatterHist(cust_data, title = "Yearly_Amount_Spent VS TimeOnWebsite", 
            xvar = "Time_on_Website", yvar = "Yearly_Amount_Spent", 
            smoothmethod = "lm")

# YearlyAmountSpent VS Length_of_Membership

ScatterHist(cust_data, title = "YearlyAmountSpent VS Length_of_Membership", 
            xvar = "Length_of_Membership", yvar = "Yearly_Amount_Spent", 
            smoothmethod = "lm", hist_color = "#00ACBB", 
            point_alpha = 0.3, 
            point_color = "#FF00CC")

##***************************************************************
##                    Study the correlation                    **
##***************************************************************
psych::lowerCor(x = cust_data)
psych::corr.test(cust_data)$p

# Plotting the ScatterPlotMatrix
# First look at the help, and the arguments 
# I am going to tweak the knobs a little
pairs(cust_data)  

#change the color and get halp matrix
pairs(cust_data, lower.panel = NULL, col= "blue")

# Or if you want only the lower part matrix

pairs(cust_data, upper.panel = NULL, col= "blue")

# Check the documentation for more information
?pairs

# Scatter Matrix with psych package
library(psych)

pairs.panels(cust_data, 
             method = "pearson", # Correlation method
             hist.col = "#11AACC",
             density = TRUE, 
             cex.cor = 1.3, 
             col = "red", 
             lm = TRUE, 
             pch = 25,    # point character
             bg = "cyan") # background

# Scatter Matrix with car package
car::scatterplotMatrix(cust_data, 
                      col = "steelblue", 
                      pch = 21, 
                      upper.panel = NULL)
# Lastly
library(PerformanceAnalytics)
chart.Correlation(cust_data, 
                  histogram=TRUE,
                  pch=19, 
                  col = "blue")


##***************************************************************
##                  correlation plot matrices                  **
##***************************************************************
correl <- cor(cust_data)

psysch::cor.plot(correl)

corrplot::corrplot(correl)

# Tweak the knobs
corrplot(correl, type = "upper", 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 45)

# check ?corrplot

# Heatmap 

heatmap(correl, symm = TRUE, 
        cexRow = 0.7, 
        cexCol = 0.7)

# ggcorrplot
p <- ggcorrplot::ggcorrplot(correl, 
                       method = "circle", 
                       type = "lower", 
                       ggtheme = ggplot2::theme_linedraw, 
                       lab_col = "blue", 
                       lab_size = 3,
                       tl.cex = 10, 
                       lab = TRUE, 
                       pch.cex = 10, 
                       colors = c("#6D9EC1", "white", "#E46726")) 
p
p + guides(scale = "none")

###*************************************************************************
###*************************************************************************
###                                                                      ***
###                          SPLITTING THE DATA                          ***
###                        TRAINING AND TEST SETS                        ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
###*

ind <- createDataPartition(cust_data$Yearly_Amount_Spent, 
                          p = 0.7, times = 1, list = FALSE)

train_set <- cust_data[ind, ]
test_set <- cust_data[-ind, ]
nrow(train_set); nrow(test_set)


# Training the model ------------------------------------------------------

lm_fit <- lm(Yearly_Amount_Spent ~ . , data = train_set)

broom::tidy(lm_fit)
broom::glance(lm_fit)

###*  *** Prediction ***
#     --------------------

pred <- predict(object = lm_fit, newdata = test_set, type = "response") 

head(pred)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set$Yearly_Amount_Spent
mae <- Metrics::mae(actual = actual, predicted = pred)
mse <- Metrics::mse(actual = actual, predicted = pred)
rmse <- Metrics::rmse(actual = actual, predicted = pred)

# Table of results

knitr::kable(cbind(mae, mse, rmse))



