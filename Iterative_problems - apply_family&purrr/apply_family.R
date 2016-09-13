
library(dplyr)

set.seed(1)
x1 <- rnorm(10, mean = 3, sd = 1)
x2 <- rpois(10, lambda = 2)
y <- 2*x1 - x2 + rnorm(10, mean = 0, sd = 1)

data <- data.frame(x1 = x1, x2 = x2, y = y)
cor(data)

######################
### apply function ###
######################

# use function on one of the dimension of the matrix/data.frame

# apply(data.frame, dimension, function)
# apply(X, MARGIN, FUN)

# compute the mean over column dimension
apply(data, 2, mean)

# compute the mean over row dimension
apply(data, 1, mean)


## exercise #1
# find the maximum value in each column using apply function


# find the 0.75 quantile of each column using apply function


## apply more complex function ##
# define function
brownian_motion <- function(vector, N = 10){
  for(i in 1:N){
  vector[length(vector) + 1] <- vector[length(vector)] + rnorm(1)
  }
  return(vector)
}

# execute function
apply(data, 2, brownian_motion)

# use additonal parameters
apply(data, 2, brownian_motion, N = 20)


## more then 2 dimensions matrix

# 6 rows, 4 columns, 2 'categories'
arr <- array(data = rnorm(6*4*2), dim = c(6,4,2))
arr

apply(arr, 1, mean)
apply(arr, 2, mean)
apply(arr, 3, mean)

# mean per each combination of a row and category
apply(arr, c(1,3), mean)

#######################
### lapply function ###
#######################

# use function on each element of the vector/list

# vector 
x <- rnorm(10)

# square each element of the vector
lapply(x, function(x, factor) x^2)

# get results in a form of a vector
lapply(x, function(x, factor) x^2) %>% unlist

# simplievied lapply (it tries to guess desiered output format)
sapply(x, function(x, factor) x^2)

vapply(x, function(x, factor) x^2) 


# lapply on the list

# define our list (three data.frames)
data_list <- list()
for(i in 1:3){
  x1 <- rnorm(10, mean = 3, sd = 1)
  x2 <- rpois(10, lambda = 2)
  y <- 2*x1 - x2 + rnorm(10, mean = 0, sd = 1)
  
  data_list[[i]] <- data.frame(y = y, x1 = x1, x2 = x2)
}

# build lm model on all three datasets
lapply(data_list, lm)

# fit without the intercept
lapply(data_list, lm, formula = y~0+x1+x2)

# save the results
models <- lapply(data_list, lm)
models

# execute summary on all three lm objects
lapply(models, summary)


## write the data simulation step in form of a function
simulation <- function(obs){
  x1 <- rnorm(obs, mean = 3, sd = 1)
  x2 <- rpois(obs, lambda = 2)
  y <- 2*x1 - x2 + rnorm(obs, mean = 0, sd = 1)
  
  output <- data.frame(y = y, x1 = x1, x2 = x2)
  return(output)
}

# execute the data simulation step with lapply function
data_list2 <- list(10,10,10)
lapply(data_list2,simulation)


