##########################
## data processing in R ##
######### dplyr ##########

library(dplyr)
getwd()

# simulate some data
sim_data <- data.frame(N1 = rnorm(1000,0,1),N2 = rnorm(1000,2,1),P1 = rpois(1000,2),
                       factor_variable = c(rep('type 1',500),rep('type 2',500)))
vector <- c(1,2,3)

# base notation 
# function(input)
mean(vector)

# pipe notation
# input %>% function()
vector %>% mean

# look into the data
sim_data %>% head

# assinging result
first_five_obserations <- sim_data %>% head()
first_five_obserations

# printing out results while defining the processing
# base notation
(first_five_obserations <- head(sim_data))

# pipe notation
first_five_obserations <- sim_data %>% head %>% print

### dplyr functions ###

## select() - selecting variables/columns ##
sim_data %>% select(N1) %>% head
sim_data %>% select(contains('N'),P1) %>% head

## filter() - filtering on condition ##
sim_data %>% filter(P1 >= 2) %>% head
sim_data %>% filter(factor_variable == 'type 1') %>% head
sim_data %>% filter(factor_variable %in% c('type 2','type 3')) %>% head # %in% notation for vector of values
sim_data %>% filter(!factor_variable %in% c('type 2','type 3')) %>% head # ! notatnion for negation of whole expression
sim_data %>% filter(P1 > 2, N1 > 0) %>% head  # AND: (condition 1, condition 2)
sim_data %>% filter(P1 > 2 & N1 > 0) %>% head # AND: (condition 1 & condition 2)
sim_data %>% filter(P1 > 2 | N1 > 0) %>% head # OR: (condition 1 | condition 2)

## mutate() - defining new variables ##
sim_data %>% mutate(new_variable = N1 + P1) %>% head
sim_data %>% mutate(monthly_diff = N1 - lag(N1)) %>% head
sim_data %>% mutate(yearly_diff = N1 - lag(N1,4)) %>% head
sim_data %>% mutate(N1_mean = mean(N1)) %>% head
sim_data %>% mutate(N1_quantile_0.9 = quantile(N1,0.9)) %>% head
sim_data %>% mutate(factor_variable = gsub(factor_variable, pattern = 'type',replace = 'typ')) %>% head

install.packages('TTR')
library(TTR)
sim_data %>% mutate(moving_avg = SMA(N1,5)) %>% head(10)

## group_by() - grouping by some variables ##
sim_data %>% group_by(factor_variable) %>% mutate(mean_in_type = mean(N1)) %>% data.frame
sim_data %>% group_by(factor_variable) %>% summarize(mean_in_type = mean(N1))

### exercise - european indicies ###
stocks <- EuStockMarkets %>% data.frame
stocks$date <- time(EuStockMarkets)

# select subset that contain only SMI and DAX


# add a SMI_10d_ret column to the stocks object defined as 10day simple return of SMI


# add a year column to the stock object using substr function on date column

# calculate mean SMI 10day return in each year (tip: use group_by and summarize functions)




## exercise - Edgar Anderson's Iris Data set (it is already loaded) ##
iris %>% head
iris %>% str
iris %>% select(Species) %>% table()

# create new variable that will consist of first three characters of Species variable

# compute mean, median, 0.1 quantile and 0.9 quantile of Sepal.Length for each category of Species variable

