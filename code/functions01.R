# First Function ----------------------------------------------------------

hello <- function(){
  print("Hello R programmers!")
}

# = , <- gets

# Call the function
hello()    # call 


# Write a function that says good bye
bye <- function(){
  print("Good bye!!!!!!!!")
}
bye()

# Try It Yourself ---------------------------------------------------------
# write a function with a name of your choice, without arguments
# That says anything

anything <- function(){
  print("today is the first R programming session") # body
}
anything()

# function template

my_func <- function(arg1, arg2){
  # Do something 
}

# You give a name to the function here is my_func, preferably a "verb"
# use the assignment operator '<-'
# Use the keyword 'function' 
# Pass some arguments to the function if there are any between parentheses
# function(arg1, arg2)   = This is called the function signature
# open curly braces {}
# Inside the curly braces is the body of the function


# Checking Functions ------------------------------------------------------
# Check the plot function
args(plot)
formals(plot)
body(plot)
environment(plot)

# - -----------------------------------------------------------------------
# Try with rank function
args(rank)
formals(rank)

print_my_name <- function(){
  print("I am Dr. Saad")
}
print_my_name()
print_my_name
body(print_my_name)
# - -----------------------------------------------------------------------
# Checking the source code of functions -----------------------------------
# cor()
cor

# str()
view(str)
# data()
View(data)
# Use View()
View(lm)



# Functions as objects ----------------------------------------------------

my_func <- function(){}

# Check if my_func is.function
is.function(my_func)

# Check the class
class(my_func)

# Check the type
typeof(my_func)

# Call the function 
my_func()

# - -----------------------------------------------------------------------
# Write Functions that take input from user

define_me <- function(){
  Name <- readline(prompt = "Hello Mr (Mrs): ")
  paste("My name is: ", Name, sep = "")
}

define_me()

# Call the function without parentheses


# Print the body of the function with body() function



# - -----------------------------------------------------------------------
# Write a more general function

def_me <- function(){
  # Add as many informations
  # then use paste(), cat() or print() functions
  
}



# Functions without arguments --------------------------------------------------
# Write a function that adds numbers
add_func <- function(){
  # add some numbers here
}

# Call the function


# Write another function:




# Passing Arguments -------------------------------------------------------
# write a function that add numbers
add_num <- function(){
  
} 

# write a function that multiplies a number by 2

mult_by_2 <- function(){}

# write a square function

square <- function(){}
  
# write a cubic function

# cubic <- 


# Write a power function

power <- function(x, y){
  x**y
} 

# Call the power function 



# Passing default arguments ----------------------------------------------
# Write a cubic function with a default value of 3


# write a general power function



# Use the return keyword to return a value. 



# More Default Arguments --------------------------------------------------
# Write a function that calculates PV value 
# pv = fv/(1-r)^n








# Give the previous function default values, 
# fv= 2000, r = 0.05, n = 6




###
fahr_to_celsius <- function(){

  
  
}
###
cel_to_fah <- function(){

  
  
}


###

circle <- function() {

  
  
}


###
area <- function(){

  
}


# Pythagorean Theorem
hypotenuse <- function(){
  
  
}




## Pass Vectors, Matrices and Logicals to the previous functions




#################################################################
##                      if-else statement                      ##
#################################################################
# Example no:1
if(runif(1, 0, 25) > 20){
  print("This number is big")
} else if(runif(1, 0, 25)  < 10){
  print("This is less than 10")
} else {
  print("I think this my lucky day")
}


# Run the example again and again to see the results

# Write another example using if-else statment 



# Arguments ---------------------------------------------------------------
args(lm)
args(set.seed)
args(rank)
args(cor)
args(print.Date)
args(prop.test)
args(prop.table)
# The NULL ----------------------------------------------------------------

View(lm)
View(cor)
View(print.Date)


# Write a function that takes a NULL default


# NULL with Variables 

a <- 12
b <- NA
c <- NULL
is.null(a); is.null(b); is.null(c)

# Negate the is.null with '!' Sign
#!is.null()                         means not null

!is.null(a)
!is.null(b)
!is.null(c)

# NULL with data frames

data("cats")
head(cats)
df <- cats
# Set one column to NULL
df$Hwt <- NULL
names(df)

# From MASS package load cars93 and set three columns to NULL




# NULL as argument  -------------------------------------------------------
# Write a function that takes a three arguments, required, default, and NULL

func <- function(x, y = 2, z = NULL){
  if (is.null(z)){
    print("I am z and I have a NULL argument")
    return(x + y)
  } else if (is.numeric(z)){
    print ("I am z and I am not null")
    return(x + y + z)
  } else {
    return(paste(x, y, z, sep = " - "))
  }
}

# Call the function three times 
func(1)
func(1, 9, z = 3)
func(2, 4, 'a')

# Try out something with NULL argument 






# The ellipsis ... --------------------------------------------------------
# Write a function for a Geometric mean

gm <- function(x){
  exp(mean(log(x)))   # Not very clear
}

library(magrittr)
gm <- function(x){
  x %>% 
    log() %>% 
    mean() %>% 
    exp()
}

x <- sample(1:39, 5)
gm(x)

# What if x has a missing value 
x[3] <- NA

gm(x)          # Result is NA. 
# How can we solve this problem

# We try to find the problem first 
log(x)   # it has no problem whatsoever 
mean(x)  # The problem happens here then

# Thus, we should be focusing on this function

args(mean)
args(mean.default)   # it has an argument called na.rm 

# na.rm is a conventional name for removing missing values 
# rewrite the function again

gm <- function(x){
  x %>% 
    log() %>% 
    mean(na.rm = TRUE) %>% 
    exp()
}
gm(x)

# This is not efficient code, as you have know about the arguments of 
# the inner functions 

# We can add an arg to remove missing values
# (na.rm) and set it to false in the signature
# and we have to change the body of the function 
gm <- function(x, na.rm = FALSE){
  x %>% 
    log() %>% 
    # change the body of this function
    mean(na.rm = na.rm) %>% 
    exp()
}
gm(x)

# The question, why an argument in the function signature. 
# To deal with the issue before any calculations occur. 

# There is still one problem with the code. It is a bit complicated 
# Usually, we need a simplified code

# Use ... to simplify it 
# The function will be write again

gm <- function(x, ...){
  x %>% 
    log() %>% 
    mean(...) %>% 
    exp()
}

# This means accept any other argument in gm() function and 
# pass them to mean() function.

# One of the main drawbacks of using ... is the user has to read the 
# documentation of the inner functions

##================================================================
##                  Practice wih Harmonic mean                  ==
##================================================================

# The formula is: harmonic_mean = 1/mean(x)

# Write a function to calculate the harmonic mean. 
# first: write reciprocal function

reciprocal <- function(){
  
}

# Write a function to calculate the harmonic mean
hm <- function() {

  
  
}

# test the function 




# Test the function with NA




# Solve the problem of missing values 

hm <- function(x, removing_missing = FALSE) {

  
  
   
}

hm(y, removing_missing = TRUE)

# The second way: use ...
hm <- function(x) {
  
}

# Test the function again
hm(y, na.rm = TRUE)

## ... as a first function argument 

args(paste)
args(cat)


# Use the function paste and paste0 with examples





#################################################################
##                    Categorical Arguments                    ##
#################################################################

func <- function(cat_arg =c("choice1", "choice2")){
  cat_arg <- match.arg(cat_arg)
}

# Try to write a naive function with categorical arguments






###########################################################################
###########################################################################
###                                                                     ###
###                        PRACTICE: READING CODE                       ###
###                      MEAN FUNCTION SOURCE CODE                      ###
###                                                                     ###
###########################################################################
###########################################################################

mean <- function(x, ...) UseMethod("mean")

mean.default <- function(x, trim = 0, na.rm = FALSE, ...)
{
  if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    # TRUE 
    # FAlSE 
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }

  if (na.rm)
    x <- x[!is.na(x)]
  if(!is.numeric(trim) || length(trim) != 1L)
    stop("'trim' must be numeric of length one")
  n <- length(x)
  if(trim > 0 && n) {
    if(is.complex(x))
      stop("trimmed means are not defined for complex data")
    if(anyNA(x)) return(NA_real_)
    if(trim >= 0.5) return(stats::median(x, na.rm=FALSE))
    lo <- floor(n*trim)+1
    hi <- n+1-lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  .Internal(mean(x))
}

