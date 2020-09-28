# Martha Gizaw
# Advanced R Final Exam
# March 4-5, 2020

# CREATING AN EXTENSION OF ARITHMETIC FUNCTIONS

# OBJECTIVE: To create a new function that extends the functionality of the existing arithmetic 
# functions in R, such as exp(), log(), sqrt(), and sin().

# The arithmetic functions I will use in this exam are as follows: 
# exp(), log(), sqrt(), abs(), ceiling(), floor(), trunc(), round(), signif(), cos(), sin(), tan()

#===============================================================================================#

# (1) Create a function named "plot_math" that takes two inputs: a function, and a vector x.

# I used FUN as an argument to input any math function to determine the result of the vector x.
plot_math <- function(FUN, x){
  return(FUN(x))
}

#===============================================================================================#

# (2) Your function should perform the chosen function on each value in x.

# To check the results of x, I executed plot_math for each math function. Let x contain three 
# vector values.

# Solve with Euler's number e and its exponent x.
plot_math(exp, c(1, 2.71, log(10))) # log() is the same as ln, so e^ln(x) = x.

# Calculate the natural log of x.
plot_math(log, c(1, 10, 54.59999))

# Calculate the square root of x.
plot_math(sqrt, c(1, 10^2, 960+2))

# Calculate the absolute value of x.
plot_math(abs, c(5, -5, -(-3/-5)))

# Round to the nearest integer greater than x using ceiling.
# Round to the nearest integer less than x using floor. 
# Round x to the nearest integer in the direction of 0 using trunc.
plot_math(ceiling, c(1, 1.11, -1.75))
plot_math(floor, c(1, 1.11, -1.75))
plot_math(trunc, c(1, 1.11, -1.75))

# Round x to the nearest whole number.
plot_math(round, c(5.66666667, -3.39, 5/6))

# Round x to a number of significant figures.
plot_math(signif, c(0.707, 1, 5.33333/6.667))

# Calculate the cosine, sine, and tangent of x.
plot_math(cos, c(0, 0.556, 3.14))
plot_math(sin, c(0, 0.556, 3.14))
plot_math(tan, c(0, 0.556, 3.14))

#===============================================================================================#

# (3) It should then plot the results in a scatterplot (use the plot() function), with the values
# from x in the x-axis and the output from the plot_math() function in the y-axis.

# I have updated the function with a statement for plotting the output.
plot_math <- function(FUN, x){
  return(plot(x, FUN(x), main="plot_math() output"))
}

# I have varied x to accurately plot the graphs of each math function.
plot_math(exp, c(50:100))

plot_math(abs, c(-500:500))

plot_math(log, c(0:500))
plot_math(sqrt, c(0:500))

# The graphs for ceiling, floor, trunc, round, and signif are all linear.
plot_math(ceiling, c(0:500))
plot_math(floor, c(0:500))
plot_math(trunc, c(0:500))
plot_math(round, c(0:500))
plot_math(signif, c(0:500))

# Variables that store generated sequences are vectors, too! Now we can clearly see the trig 
# graphs.
x <- seq(0,10,0.1)
is.vector(x)
plot_math(cos, x)
plot_math(sin, x)
plot_math(tan, x)

#===============================================================================================#

# (4) Now, include an option to return the output from (2) as a vector in addition to plotting 
# it. This will require an additional argument for the function as well as an if-clause in the 
# function.

# In plot_math, plot a function that matches with the variable y. Suppose y calls for the 
# square root of x. If the user specifies FUN = sqrt, plot_math will return y as a vector 
# while plotting it. If the user specifies a function other than sqrt (e.g. FUN = log), 
# plot_math will output FALSE because y does not match with FUN, and the scatterplot will 
# not appear.
plot_math <- function(FUN, x, y){
  ifelse(y == FUN(x), plot(x, y, main="plot_math() output") & return(as.vector(y)), return(FALSE))
}

# If y can equate to FUN, then we can return the output as a vector while producing the graph.
x <- c(0:500)
plot_math(sqrt, x, sqrt(x))
plot_math(log, x, sqrt(x))
plot_math(log, x, log(x))

#===============================================================================================#

# (5) Last, include a check that will stop your function from running if the user passes negative 
# x values while specifying the sqrt() or log() functions (you cannot take the square root or log 
# of a negative number).

# I have updated plot_math to include a simple test in an inelse clause that checks if x is 0 
# or greater, the vector output y does not contain any values of NaN, and the user specifies 
# either FUN = sqrt or FUN = log for y. While y can match with any math function, if all of 
# the above conditions are true, then plot_math will properly plot the sqrt and log outputs.
plot_math <- function(FUN, x, y){
  while(any(y == FUN(x))){
    ifelse(x >= 0 & any(is.nan(y)) == FALSE & (y == sqrt(x) | y == log(x)),
           plot(x, y, main="plot_math() output") & return(as.vector(y)), return(FALSE))
  }
}

# First, let's try a vector with all positive values. The tryCatch loops are used to check for 
# any errors or warnings with sqrt or log.
x <- c(0:500)
plot_math(abs, x, abs(x))
tryCatch(
  expr = {
    plot_math(sqrt, x, sqrt(x))
  },
  warning = function(w){
    stop("You cannot take the square root of a negative number!")
    print(w)
  }
)
tryCatch(
  expr = {
    plot_math(log, x, log(x))
  },
  warning = function(w){
    stop("You cannot take the log of a negative number!")
    print(w)
  }
)

# Now let's try a vector with half of the values being negative integers.
x <- c(-500:500)
plot_math(abs, x, abs(x))
tryCatch(
  expr = {
    plot_math(sqrt, x, sqrt(x))
  },
  warning = function(w){
    stop("You cannot take the square root of a negative number!")
    print(w)
  }
)
tryCatch(
  expr = {
    plot_math(log, x, log(x))
  },
  warning = function(w){
    stop("You cannot take the log of a negative number!")
    print(w)
  }
)

#===============================================================================================#

"THE END!"
graphics.off()
clc <- function() cat(rep("\n", 50))
clc()
rm(list = ls(all.names = TRUE))

# Martha Gizaw
# Advanced R Final Exam
# March 4-5, 2020