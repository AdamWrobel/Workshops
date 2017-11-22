###### dplyr #####
### exercise ###
stocks <- EuStockMarkets %>% data.frame
stocks$date <- time(EuStockMarkets)

# select subset that contain only SMI and DAX
stocks %>% select(SMI,DAX)

# add a SMI_10d_ret column to the stocks object defined as 10day simple return of SMI
stocks <- 
  stocks %>% mutate(SMI_ret = (SMI - lag(SMI,10))/lag(SMI,10)) 
stocks %>% head(15)

# add a year column to the stock object using substr function on date column
stocks <- 
  stocks %>% mutate(year = substr(date, 0, 4)) 

# calculate mean SMI 10day return in each year (tip: use group_by and summarize functions)
stocks %>% group_by(year) %>% summarize(mean_ret = mean(SMI_ret, na.rm = T))




## exercise - Edgar Anderson's Iris Data set (it is already loaded) ##
iris %>% head
iris %>% str
iris %>% select(Species) %>% table()

# create new variable that will consist of first three characters of Species variable
iris %>% mutate(short = substr(Species,1,3)) %>% head

# compute mean, median, 0.1 quantile and 0.9 quantile of Sepal.Length for each category of Species variable
iris %>% group_by(Species) %>% mutate(mean = mean(Sepal.Length),
                                      median = median(Sepal.Length),
                                      quantile_0.1 = quantile(Sepal.Length,0.1),
                                      quantiel_0.9 = quantile(Sepal.Length,0.9)) %>%
  head

iris %>% group_by(Species) %>% summarize(mean = mean(Sepal.Length),
                                         median = median(Sepal.Length),
                                         quantile_0.1 = quantile(Sepal.Length,0.1),
                                         quantile_0.9 = quantile(Sepal.Length,0.9))


new_df <- data.frame(opinion = c('nice','funny','not nice'),
                     Species2 = c('setosa','versicolor','virginica'))

iris %>% left_join(new_df, by = c('Species' = 'Species2')) %>% head()


## mutate and summarize on different dimensions ##
sim_data %>% group_by(P1,factor_variable) %>% mutate(mean_N1 = mean(N1), distance_from_the_mean = N1 - mean_N1) %>%
  arrange(P1,factor_variable) %>% group_by(factor_variable) %>% summarize(mean(distance_from_the_mean))




######## reading data ##########

# read from csv file - try to read macroeconomic_data_USA.csv using read.table or read.csv function
read.table('macroeconomic_data_USA.csv', sep = ';')

# read from excel file
# try to read sheet '2014' from lifetables1990-2014.xlsx' file using read_excel function
install.packages('readxl')
library(readxl)

read_excel('lifetables1990-2014.xlsx', sheet = '2014', skip = 3)