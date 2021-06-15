############################################################################
############################################################################
###                                                                      ###
###                          CHECKING ARGUMENTS                          ###
###                                                                      ###
############################################################################
#############################################################################

library(magrittr)
# First example

gm <- function(x, ...){
  x %>% 
    log() %>% 
    mean(...) %>% 
    exp()
}

# If we a pass non-numeric argument to gm function

gm('a')

# The error comes from the log() function not from the gm() function, 
# What should we then?


# You ask yourself the next questions?
# geometric mean is for numbers. Can we calculate it for letters or words?
# if not, try to handle that in the body of the function
# Let us rewrite the previous function to throw an error if the passed 
# argument is not numeric

# is.numeric(x) function returns TRUE if x is numeric or FALSE otherwise.
# !is.numeric(x): can be read not numeric. the opposite of the first

# 1.Ensure the argument is numeric

gm <- function(x, ...){
  if(...){
    # Add a stop message
  }
  x %>% 
    log() %>% 
    mean(...) %>% 
    exp()
}

# Test with the function with a character



# Another question: can we calculate the geometric mean for negative numbers
# The answer is no. So, we should hand this as well 

gm <- function(x, ...){
  if(...){
    # Add a stop message
  }
  # Check for positive values
  if( ...){
    # Add a stop message
  }
  x %>% 
    log() %>% 
    mean(...) %>% 
    exp()
}
gm(-5)


# Check for a vector 
gm(c(1, 5, -4, 7))
# We have an issue, we should fix that as well 
# The second condition must take a vector under consideration

gm <- function(x, ...){
  if(...){
    # Stop message
  }
  for(i in seq_along(x)) {
    if (any(x[i] < 0, na.rm = TRUE)){
      stop("X must be positive, while it has negative value ",
           x[i], "in the position ", i, '.')
    }
  }
  x %>% 
    log() %>% 
    mean(...) %>% 
    exp()
}

gm(c(1, -2))



#################################################################
##            Using packages for writing assertions            ##
#################################################################

library(assertive)

gm <- function(x, ...){
  # Assert x is numeric
  # Assert all x elements are positive
  x %>% 
    log() %>% 
    mean(...) %>% 
    exp()
}
gm('a')
gm(-3)

gm(c(1, 3, -2))

# Using any() functions
gm <- function(x, na.rm = FALSE){
  # Assert is numeric
  if(any(is_non_positive(x), na.rm = TRUE)){
    stop("x contains a non positive value, geometric mean cannot be computed 
         for negative values")
  }
  x %>% 
    log() %>% 
    mean(na.rm = na.rm) %>% 
    exp()
}

gm(c(1, 4, -5))

#################################################################
##                 Practice with Harmonic Mean                 ##
#################################################################

#1. assert that x is numeric

hm <- function(x, na.rm = FALSE) {
  
}

# 2. assert x is positive

hm <- function(x, na.rm = FALSE) {
  
}

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                       Fixing Arguments                       ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Instead of throwing an error, sometimes is useful to fix the arguments
# Fixing the na.rm argument

gm <- function(x, na.rm = FALSE){
  assert_is_numeric(x)
  if(any(is_non_positive(x), na.rm = TRUE)){
    stop("x contains a non positive value, geometric mean cannot be computed 
         for negative values")
  }
  na.rm <- coerce_to(use_first(na.rm), target_class = "logical")
  x %>% 
    log() %>% 
    mean(na.rm = na.rm) %>% 
    exp()
}
gm(1:3, na.rm = 1:3)


# Rewrite the complete version of harmonic mean function
hm <- function(x, na.rm = FALSE) {
  
  # Use the first value of na.rm, and coerce to logical
  

}