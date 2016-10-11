##########################################
### purrr package - iterative problems ###
##########################################

# load package
.libPaths('') # add path at which you did unzipped the packages shared via email
library(purrr)

# map function
# execute function iterativly over dimension of given object

# use on a data.frame
df <- data.frame(a = 1:10, b = 11:20)
map(df, mean)

# use on a list
l <- list(a = 1:10, b = 11:20)
map(l, mean)

# use on a vector
vector <- c(a = 1, b = 2)
map(vector, mean)

# get results in a different form than a list:
map_dbl(l, mean)
map_lgl(l, is.numeric)
map_chr(l, typeof)

# lapply versions
vapply(l, mean, FUN.VALUE = double(1))
vapply(l, is.numeric, FUN.VALUE = logical(1))
vapply(l, typeof, FUN.VALUE = character(1))

# pass additonal arguments
map(l, quantile)
map(l, quantile, probs = 0.95)
map_dbl(l, quantile, probs = 0.95)

## Exercise #2
# execute summary function on object (iris_list) using map function
# and answer the question:
# which iris type have on average the longest petals (Petal.Length)
iris_list <- split(iris,iris$Species)


# linear model example
map(data_list, lm) %>% map(summary)
models <- map(data_list, lm)

# create a bigger sample
n_obs <- list(1000,1000,1000)
data_list2 <- lapply(n_obs,simulation)
models <- map(data_list2, lm)

# more complex function
ploting <- function(model_object){
  plot(y = model_object$fitted, x = model_object$model$y)
}
map(models, ploting)

# walk, map and pipes
walk(models, ploting)
map(data_list2, lm) %>% walk(ploting) %>% map(summary)

## shortcuts

# anonymous functions
map(df, function(x) max(abs(x))) # standard approach
map(df, ~ max(abs(.))) # shortcut

map(data_list, function(x) lm(y ~ x1+x2, data = x)) # standard approach
map(data_list, ~ lm(y ~ x1+x2, data = .)) # shortcut

# subsetting
map(data_list, ~ lm(y ~ x1+x2, data = .)) %>% map('coefficients')
map(df, quantile) %>% map('75%')

# Exercise #3
# mtcars data frame splitted by number of cyliners 
# in result we've got a list of three data frame
cyl <- split(mtcars, mtcars$cyl)

# compute the mean mpg for each using map function


# construct linear model (mpg ~ wt) for each using 
# map function with anonymous function inside


# use shortcut for anonymous functions to get same results


# save the results in object models
models <- 

# use map and coef to get the coefficients for each model (save as coefs)
coefs <- 

# use string shortcut to extract the wt coefficient


## let's combine what we've learn at workshop#1 and use pipe notation
models <- mtcars %>% split(mtcars$cyl) %>% map(~ lm(mpg ~ wt, data = .))

map(models, summary) %>% map_dbl('r.squared')


## mapping over many arguments

# one argument
n <- list(5,10,20)
map(n, rnorm)

# two arguments
means <- list(1,5,10)
map2(n,means, rnorm)

# p arguments
sds <- list(0.1,1,0.5)
simulation <- pmap(list(n = n, mean = means, sd = sds), rnorm)

## mapping with side effects - walk function
simulation <- pmap(list(n = list(100,100,100), mean = means, sd = sds), rnorm)
par(mfrow = c(1,3))
walk(simulation, hist)

# walking over multiple arguments
titles <- c('Normal(10, 1)', 'Uniform(0, 5)', 'Exp(5)')
pwalk(list(x = simulation, main = titles), hist, xlab = '')

# walk, pipes and map
simulation %>% walk(hist) %>% map(summary)

pmap(list(n = list(100,100,100), mean = means, sd = sds), rnorm) %>%
  walk(hist) %>% map(summary)


## THE END ##
