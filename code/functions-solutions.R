#################################################################
##                          Solutions                          ##
#################################################################


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



##================================================================
##                  Practice wih Harmonic mean                  ==
##================================================================

# The formula is: harmonic_mean = 1/mean(x)

# Write a function to calculate the harmonic mean. 
# first: write reciprocal function

reciprocal <- function(x){
  return(1/x)
}

# Write a function to calculate the harmonic mean
hm <- function(x) {
  x %>%
    reciprocal() %>%
    mean() %>%
    reciprocal() 
}

# test the function 

y <- sample(1:99, 7)

hm(y)


y[c(2, 6)] <- NA
y

hm(y)

# Solve the problem of missing values 

hm <- function(x, removing_missing = FALSE) {
  x %>%
    reciprocal() %>%
    mean(na.rm = removing_missing) %>%
    reciprocal() 
}

hm(y, removing_missing = TRUE)

# The second way ...
hm <- function(x, ...) {
  x %>%
    reciprocal() %>%
    mean(...) %>%
    reciprocal() 
}

hm(y, na.rm = TRUE)

