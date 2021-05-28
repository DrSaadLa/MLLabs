# First Function ----------------------------------------------------------

hello <- function(){
  print("Hello R programmers!")
}

# Call the function
hello()

# Write a function that says good bye



# Try It Yourself ---------------------------------------------------------
# write a function with a name of your choice, without arguments
# That says anything




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






# - -----------------------------------------------------------------------
# Checking the source code of functions -----------------------------------
# cor()

# str()


# data()

# lm()

# Use View()
View(lm)






# Functions as objects ----------------------------------------------------

my_func <- function(){}

# Check if my_func is.function

# Check the class

# Check the type


# Call the function 

# - -----------------------------------------------------------------------
# Write Functions that take input from user

define_me <- function(){
  Name <- readline(prompt = "Hello Mr (Mrs): ")
  paste("My name is: ", Name, sep = " ")
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



#































# write a function that add numbers
add_num <- function(x, y, z){
  x + y+ z
} 

add_num(12, 34, 3456)

# write a function that multiplies a number by 2

mult_by_2 <- function(x){
  x* 2
}
mult_by_2(4)
# write a square function

square <- function(x){
  x**2
}
square(45)
# write a cubic function

cubic <- function(x) x**3
cubic(3)

power <- function(x, y){
  x**y
}

power(2, 3)

# Solution to exercises
fahr_to_celsius <- function(f_tem){
  c_temp <- (f_tem - 32)*(5/9)
  return(c_temp)
}
fahr_to_celsius(0)
fahr_to_celsius(20)
fahr_to_celsius(60)
fahr_to_celsius(100)
cel_to_fah <- function(c_temp){
  f_temp <- (c_temp * 9/5) + 32
  return (f_temp)
}
cel_to_fah(0)
cel_to_fah(37)

circle <- function(r) {
  c <- 2*pi*r
  return(c)
}

circle(0.4)

area <- function(r){
  return(pi*r**2)
}
area(6)

# Pythagorean Theorem
hypotenuse <- function(a, b){
  sqrt(a^2 + b^2)
}
hypotenuse(2, 3)
