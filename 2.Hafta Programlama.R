# Recall ------------------------------------------------------------------
library(help = "stats")

df <- data.frame('a' = c(1,2,3,4,5) , 'b' = c(11,12,13,14,15))
df
is.data.frame(df)
as.matrix(df)
colnames(as.matrix(df))

# Adding vector and matrix together as list
m=matrix(1:10, 5 ,2)
m
l4 <- list('Vector' = c(10,11,12,13) , 'Matrix' = m)
l4

l5 <- list('Vector' = runif(5) , 'Matrix' = m, 'DF' = df)
l5

# %in% usage

x <- rep(1:2, 3)
x
x <- rep(1:2, each=3)
x
x %in% c(1, 5)
x[x %in% c(1, 5)]

# ! x
# x & y
# x && y
# x | y
# x || y

x <- c(1,2,3,4,5)
y <- c(10,11,12,13,15)

# NOTE 
# & and && indicate logical AND and | and || indicate logical OR. 
# The shorter form performs elementwise comparisons in much the 
# same way as arithmetic operators. 
# The longer form evaluates left to right examining only 
# the first element of each vector. 
# Evaluation proceeds only until the result is determined. 
# The longer form is appropriate for programming control-flow.

x <- 1:5
x > 1 & x < 3
x > 1 && x < 3

#About Programming ---------------
# Commonly used control structures are
# if and else: testing a condition and acting on it
# 
# for: execute a loop a fixed number of times
# while: execute a loop while a condition is true
# 
# break: break the execution of a loop
# next: skip an interation of a loop
# repeat: execute an infinite loop (must break out of it to stop)

# If statement ------------------------------------------------------------
# 
# #if (condition){
#   Do something
# } else {
#   Do something different
# }

x <- 3
if (x > 4) {
  print("x is greater than 4")
}


i <- 5
if (i > 3){
  # set.seed(11) # for reproducibility
  runif(5, 10, 20)
  
} else {
  message("No output")
}

# if (condition1) { 
#   expr1
# } else if (condition2) {
#   expr2
# } else if  (condition3) {
#   expr3
# } else {
#   expr4
# }

catvec <- LETTERS[1:10]
catvec

#category <- 'A' #  
category <- sample(catvec, 1) #check help !
category

price <- 10

# About cat() usage
# The simple printing method in R is to use print(). 
# As its name indicates, this method prints its arguments 
# on the R console. However, cat() does the same thing but is valid 
# only for atomic types (logical, integer, real, complex, character) 
# and names

if (category == 'A') {
  cat('A vat rate of 8% is applied.', 'The total price is', price * 1.08)
} else if (category == 'B') {
  cat('A vat rate of 10% is applied.', 'The total price is', price * 1.10)
} else {
  cat('A vat rate of 20% is applied.', 'The total price is', price * 1.20)
}

paste('A vat rate of 20% is applied.','The total price is', price *1.20)  

# switch  -----------------------------------------------------------------

x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2" 
  } else if (x == "c") {
    "option 3"
  } else {
    "option 4"
    # stop("Invalid `x` value")
  }
}

x_option(x="q")


# In a more short way we can use switch 
# Usage: switch (expression, list)

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         d = "option 4"
  ) }

# x_option("a")

switch(2,"red","green","blue")


# ifelse ------------------------------------------------------------------
mydata <- sample(100, 10)
mydata

class(mydata)

df
df$a

# usage of ifelse() function
# ifelse(test, yes, no)

x = ifelse(df$a>3, 1, df$a)
x


ifelse(mydata > 50, 50, mydata)


newdata <- cbind(x, mydata)
newdata # as data frame
newdata <- as.data.frame(newdata)

ifelse(newdata$x == 1, newdata$mydata <- 0, mydata)

# For Loop ----------------------------------------------------------------

# for (variable in sequence){
#   Do something
# }

# Example 
1:4
b <- 1:4
for (i in 1:4){
  j <- i + 10
  toget_ij <- cbind(i,j)
  # class(toget_ij)
  print(toget_ij)
}

x <- matrix(1:6, 2, 3, byrow = T)
x

dim(x)
length(x)

# seq_len ?

for(i in seq_len(nrow(x))) { # seq_len(nrow(x)): 1:nrow(x)
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}

# Combination 
x <- runif(10, 3, 8)
x

mean(x)
# hist(x)
n <- length(x)

xnorm = xstandr = vector(mode="numeric", length= length(x))

x[11] <- "a"
x
# numeric(10)
for(i in 1:n){
  # If any of the expressions are not all TRUE, stop is called !
  stopifnot(is.numeric(x))
  # Rescaling of the x values
  if(mean(x) > 5) {
    xnorm[i] <- (x[i] - min(x)) / (max(x) - min(x))
  }
  else{
    xstandr[i] <- (x[i] - mean(x)) / sqrt(var(x))
  }
}

xnorm
hist(xnorm)
xstandr
hist(xstandr)

# While Loop --------------------------------------------------------------

# while (condition){
#   Do something
# }

i <- 2
while (i < 5){
  print(i)
  i <- i + 1
}


z <- 5
zval <- NULL
while (z >= 4 & z <= 10) { # & usage is possible or not ?
  set.seed(11) # important for reproducibility
  
  # rbinom(n, size, prob)
  coin <- rbinom(1, 1, 0.5) 
  
  if (coin == 1) {
    ## random walk
    z <- z + 1
  } else {
    z <- z - 1
  }
}

z

# Break command -----------------------------------------------------------
#  the loop is immediately terminated and 
# program control resumes at the next statement following the loop

no <- c(1:50)
no

for (val in no) 
{ 
  if (val %% 5 == 0)  
  { 
    print(paste("Coming out from for loop Where i = ", val)) 
    #next
  } 
  
  if (val %% 39 == 0)  
  { 
    print(paste("Coming out from for loop Where i = ", val)) 
    break
  } 
  print(paste("Values are: ", val)) 
} 

# Next command ------------------------------------------------------------

# next is used to skip an iteration of a loop.
x <- 1:50
for (val in x) {
  
  if (val %% 3 == 1){
    next
  }
  print(val)
  
}

# Functions ---------------------------------------------------------------

name <- function(arg1, arg2, ...)
  
# Importance of ...
  # Another frequent requirement is to allow one function to pass 
  # on argument settings to another

# function_name <- function(var){
#   Do something
#   return(new_variable)
# }

sum(c(1,2,3,NA), na.rm=TRUE)

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

# All R functions have three parts:

# the body(), the code inside the function.
body(roll)

# the formals(), the list of arguments which controls how you can call the function.
formals(roll)
# the environment(), the "map" of the location of the function's variables.
environment(roll)

roll()

square <- function(x){
  
  squared <- x*x
  
  return(squared)
  # return(c(x,squared))
  # return(list(value = x, squaredval = squared))
}

square(4)

# R functions arguments can be matched positionally or by name. 

str(rnorm)
mydata <- rnorm(100, 2, 1)              ## Generate some data
# hist(mydata)

x=matrix(rnorm(25), 5, 5, T)
x
# is.matrix(x)


x <- as.data.frame(x)

# Taking the traspose of a matrix
mytrans <- function(x) {
  if (!is.matrix(x)) {
    # warning("argument is not a matrix: returning NA")
    # return(NA_real_)
    # OR 
    x <- as.matrix(x)
  }
  y <- matrix(0, nrow=ncol(x), ncol=nrow(x))
  
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      y[j,i] <- x[i,j]
      
      if(y[j,i] >= 0.5){
        y[j,i] <- 1
      }
      else{
        y[j,i] <- 0
      }
    }
  }
  return(list("x" = x, "y" = y)) # For multiple argument we need to use list general
}

str(x)
mytrans(x)

# FOR SELF STUDY

# Summary function --------------------------------------------------------
rep <- 100
set.seed(1130)
df <- as.data.frame(cbind(rnorm(rep), runif(rep), 
                          sample(100,rep,replace = TRUE)))

SummaryLoc = function(data) {
  if (!is.data.frame(x)) {
    data <- as.data.frame(data)
  }
  
  n <- dim(data)[1]
  ncol <- dim(data)[2]
  
  dfsumm <- matrix(0,3,ncol)
  
  for (i in 1:ncol) {
    # For mean
    Sumdf <- lapply(data, sum)
    #class(Sumdf)
    dfsumm[1,i] <- unlist(Sumdf[i])/n
    
    # For median
    Sortdf=matrix(0,n,ncol)
    Sortdf[,i] <- sort(data[,i])
    #head(Sortdf)
    #class(Sortdf[,i])
    
    if(n %% 2 == 0) {
      dfsumm[2,i] <- sum(Sortdf[c(n/2,n/2+1),i]) / 2
    }
    
    if(n %% 2 == 1) {
      dfsumm[2,i] <- Sortdf[(n+1)/2,i]
    }
    
    # For Mode
    CountObs <- table(df[i])
    CountObs <- as.vector(CountObs)
    CountObs
    
    if (max(CountObs) > 1) {
      maxIndf <- which.max(table(df[i]))
      dfsumm[3,i] <- as.numeric(rownames(as.matrix(maxIndf)))
    }
    else {
      dfsumm[3,i] <- NaN
    }
    
  }
  # ,Output
  return(dfsumm)
}

summary(x)

f <- function(x,y) 2*x*(y**2)+2*(x**2)*y+x*y

# Specific example do.call and call functions 

# Basic R syntax of do.call function
do.call("any_function", arguments_list) 
# Basic R syntax of call function
call("any_function", argument1, argument2)   

x1 <- runif(8)
x1   

do.call("sum", list(x1))  # second argument !

my_call <- call("sum", x1)
my_call
eval(my_call)

# Debugging Part ----------------------------------------------------------

# debugging is designed to help you find bugs by figuring out where 
# the code is not behaving in the way that you expect.

f <- function(a) { g(a) }
g <- function(b) h(b)
h <- function(c) i(c)

i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}

# TRY 
f(5)

f("a")

traceback()
# From Debug button above ?

# try ----------------------------------------------------------------

try(f(10))

try(f("a"))

tryCatch(f("a"))

# example of stopifnot

i <- function(d) {
  stopifnot(is.numeric(d) == TRUE)
  d + 10
}

f(5)

stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 > 2) 

# For interactive debugging

# you need more information, and the easiest way to get it is with the 
# interactive debugger which allows you to pause execution of a function 
# and interactively explore its state.

g <- function(b) {
  browser() 
  h(b)
}
f(10)

# browser() is just a regular function call which means that you can run it conditionally
# by wrapping it in an if statement:

g <- function(b) {
  if (b < 0) {
    browser()
  }
  h(b)
}
f(10)

f(-5)

# About browser commands

# Some other alternatives
# recover function 

options(error = recover)
f("x")

options(error = NULL)

# Non interactive debugging part:

# When you can't explore interactively, 
# it's particularly important to spend some time making the problem as small 
# as possible so you can iterate quickly.

# dump.frames() is the equivalent to recover() for non-interactive code; 
# it saves a last.dump.rda file in the working directory.


# Pring debugging 
# where you insert numerous print statements to precisely locate the problem, 
# and see the values of important variables.

f <- function(a) {
  cat("f()\n")
  g(a)
}
g <- function(b) {
  cat("g()\n")
  cat("b =", b, "\n")
  h(b)
}
h <- function(c) {
  cat("i()\n")
  i(c)
}

f(10)
f("a")

# For Non-error failures
# There are other ways for a function to fail apart from throwing an error:

f <- function() g()
g <- function() warning("Hi!")
f()
options(warn = 2) # Convert into error
f()
options(warn = 1) # to keep it as warning

f <- function() g()
g <- function() message("Hi!")
f()


# Other sources
# https://data-flair.training/blogs/debugging-in-r-programming/

# For automatization 

# You can automate this process with the errorist and searcher packages
# https://cran.r-project.org/web/packages/errorist/index.html
# https://cran.r-project.org/web/packages/searcher/searcher.pdf

# Performance Measuring ---------------------------------------------------

# KEEP in MIND THAT : Never grow a vector 

n=100
x <- 1:n
x
system.time( x <- 1:n )

# Elapsed Time: the time charged to the CPU(s) for the expression.
# User Time: the wall clock time. The time that you as a user experienced.

# OR Preallocate
x <- vector("numeric", n)
system.time(
  for(i in 1:n){
    x[i] <- i })

# OR  Growing
x <- NULL # Length zero
system.time(
  for(i in 1:n)
    x <- c(x, i)
)

# Vectorization
# use a vectorized solution wherever possible.

x <- vector("numeric", n)
system.time(
  for(i in 1:n)
    x[i] <- rnorm(1)
)

system.time(
  x <- rnorm(n)
)

# Another example 

x <- 1:100000
## the vectorized version ...
system.time( y <- x^2)
y
## or the for loop version?

z <- vector(mode = mode(x), length = length(x))
system.time(
  for(i in seq_along(x)) {
    z[i] <- x[i]^2
  })

identical(y, z)

# For further information about timing/code profiling 
# Source: https://www.alexejgossmann.com/benchmarking_r/

# About profvis -----------------------------------------------------------
# Profvis is a tool for helping you to understand how R spends its time. 
# It provides a interactive graphical interface for visualizing data 
# from Rprof, R's built-in tool for collecting profiling data.

#' There are two ways to use profvis: 
#' From the Profile menu in RStudio. 
#' With profvis::profvis(). I recommend storing your code in 
#' a separate file and source()ing it in; this will 
#' ensure you get the best connection between profiling data and source code.

# About profiling the code, to understand which part is slow
install.packages("profvis")
library("profvis")

cities <- c("New York", "Paris", "London", "Leuven",
            "Ankara", "Dresden", "Padova")

num_chars <- c() # or NULL
num_chars

profvis({
  #system.time(
    for(i in 1:length(cities)) {
      # nchar: Count the Number of Characters 
      num_chars[i] <- nchar(cities[i]) }
  
})

#profvis({

# lapply function
system.time(
  lapply(cities, nchar)
)

#})

# To get unlist output
unlist(lapply(cities, nchar)) 

# CHECK other families of lapply; sapply, tapply etc. 
# https://www.guru99.com/r-apply-sapply-tapply.html


# Numeric Example
profvis({
  N <- 10000
  x1 <- rnorm(N)
  x2 <- rnorm(N)
  df <- as.data.frame(cbind(x1, x2))
  #df
  
  index <- length(df[, 1])
  # index <- dim(df)[1]
  
  #system.time( 
  for (loop in c(1:index)) {
    df$mean2[loop] <- mean(c(df[loop, 1], df[loop, 2]))
  }
  #)
  
  #system.time(
  df$mean1 <- apply(df, 1, mean)
  #  )
  
  #system.time(
  df$mean3 <- rowMeans(df[, c(1, 2)])
  #  )
})

# Another example for profvis

# Generate data
times <- 4e5
cols <- 150
data <- as.data.frame(x = matrix(rnorm(times * cols, mean = 5), ncol = cols))
data <- cbind(id = paste0("g", seq_len(times)), data)

profvis({
  data1 <- data
  # Four different ways of getting column means
  means <- apply(data1[, names(data1) != "id"], 2, mean)
  means <- colMeans(data1[, names(data1) != "id"])
  means <- lapply(data1[, names(data1) != "id"], mean)
  means <- vapply(data1[, names(data1) != "id"], mean, numeric(1))
})

# For further readings about profvis
# https://rstudio.github.io/profvis/index.html

# https://bookdown.org/rdpeng/rprogdatascience/profiling-r-code.html

# Microbenchmark -------------------------------------------------------------

# A microbenchmark is a measurement of the performance of a very small piece of code, 
# something that might take milliseconds (ms), microseconds (µs), 
# or nanoseconds (ns) to run. Microbenchmarks are useful for comparing small 
# snippets of code for specific tasks.

# A great tool is the bench package.
# The bench package uses a high precision timer, 
# making it possible to compare operations that only take a tiny amount of time
install.packages("bench")
library(bench)

x <- runif(100)

lb <- bench::mark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1/2)
)

# bench::mark() returns the results as a tibble, 
# with one row for each input expression, and the followings:
# min, mean, median, max, and itr/sec summarise the time taken by the expression. 
# Focus on the minimum (the best possible running time) and the median (the typical time).
# mem_alloc tells you the amount of memory allocated by the first run
# n_gc() tells you the total number of garbage collections

lb

lb[c("expression", "min", "median", "itr/sec", "n_gc")]

library(beeswarm)
plot(lb, type = "beeswarm")

# About the interpretation

# 1 ms, then one thousand calls take a second.
# 1 µs, then one million calls take a second.
# 1 ns, then one billion calls take a second.

# Example 

(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1/2),
  exp(log(x) / 2)
))

lb <- bench::mark(
  # Four different ways of getting column means
  apply(data[, names(data) != "id"], 2, mean),
  colMeans(data[, names(data) != "id"]),
  lapply(data[, names(data) != "id"], mean),
  vapply(data[, names(data) != "id"], mean, numeric(1))
, check = FALSE)

lb
plot(lb, type = "beeswarm")

str(lb)
class(lb$expression)[[1]]
cbind(lb$expression, lb$total_time)

# Summary -----------------------------------------------------------------

# 1. Look for existing solutions.

mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)

x <- runif(100)
all.equal(mean1(x), mean2(x))

microbenchmark(
  mean1(x),
  mean2(x)
)

# 2. Do less work.

# rowSums(), colSums(), rowMeans(), and colMeans() are faster 
# than equivalent invocations that use apply() because they are vectorised 

# vapply() is faster than sapply() because it pre-specifies the output type.

system.time( vapply(data1[, names(data1) != "id"], median, numeric(1)) )
system.time( sapply(data1[, names(data1) != "id"], median) )

start_time <- Sys.time()
vapply(data1[, names(data1) != "id"], median, numeric(1))
end_time <- Sys.time()
end_time - start_time

# 3. Vectorise.

# 4. Parallelise.
# use of parallel, doParallel, foreach package, 

library(parallel)
numCores <- detectCores()
numCores

# using foreach
library(foreach)

system.time(
foreach (i=1:10000) %do% {
  sqrt(i)
})

library(doParallel)
registerDoParallel(numCores)  # use multicore, set to the number of our cores
system.time(foreach (i=1:10000) %dopar% {
  sqrt(i)
})

# Further reading
# https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html

# 5. Avoid copies.

# Whenever you use c(), append(), cbind(), rbind(), or paste() 
# to create a bigger object, R must first allocate space for the 
# new object and then copy the old object to its new home. 
# If you're repeating this many times, like in a for loop, 
# this can be quite expensive

# 6. Byte-code compile.

# 7. A final technique is to rewrite in a faster language, like C++. 
# That's a big topic and is covered in Rcpp.
  
# For further reading
# http://adv-r.had.co.nz/Profiling.html#improve-perf
