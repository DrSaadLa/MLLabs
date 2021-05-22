# Installing necessary-packages ------------------------------------------------------
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


# Loading packages --------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(dslabs)
library(gapminder)
library(sigr)
library(Hmisc)
library(psych)



# Reading or Importing Data -----------------------------------------------

url <- "https://raw.githubusercontent.com/DrSaadLa/MLLabs/main/data/housing.csv"

# You can download the file into your computer using 

# download.file(url = url, destfile = "housing.csv")
# Or read it from the web 

housing <- read_csv(url)
housing2 <- read.csv(url)


# Explore the dataset -----------------------------------------------------

str(housing)
str(housing2)

# Using dplyr function glimpse

glimpse(housing)

names(housing)
colnames(housing)
nrow(housing)
ncol(housing)  

head(housing2)
tail(housing2)



# Summary Statistics ------------------------------------------------------
summary(housing)

Hmisc::describe(housing)
psych::describe(housing)



