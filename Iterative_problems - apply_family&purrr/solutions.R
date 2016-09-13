## exercise #1
# find the maximum value in each column using apply function
apply(data, 2, max)

# find the 0.75 quantile of each column using apply function
apply(data, 2, quantile)

## Exercise #2
# execute summary function on object a created list using map function
# and answer the question:
# which iris type have on average the longest petals (Petal.Length)
iris_list <- split(iris,iris$Species)
map(iris_list, summary)

# Exercise #3
# mtcars data frame splitted by number of cyliners 
# in result we've got a list of three data frame
cyl <- split(mtcars, mtcars$cyl)

# compute the mean  for each using map function
map(cyl, mean, 'mpg')

# construct linear model for each using map function with anonymous function inside
map(cyl, function(df) lm(mpg ~ wt, data = df))

# use shortcut for anonymous functions to get same results
map(cyl, ~lm(mpg ~ wt, data = .))

# save the results in object models
models <- map(cyl, ~lm(mpg ~ wt, data = .))

# use map and coef to get the coefficients for each model (save as coefs)
coefs <- map(models, coef)

# use string shortcut to extract the wt coefficient
map(coefs, 'wt')
map(coefs, 2)