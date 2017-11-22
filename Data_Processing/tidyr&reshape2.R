#####################################
####### data processing in R ########
######### tidyr & reshape2 ##########

source('reading_excel.R')


##############
## reshape2 ##
##############

library(reshape2)
# cast - go from long to short
# dcast - cast for data.frames
# dcast(row_variables ~ column_variables, value.var)

# taking year and sex as a row variables and age as a column variable; value that we want to have in cells is ex
out <- PTTZ %>% dcast(year + sex ~  age, value.var = 'ex')
out[,1:20] %>% head

# taking year as a row variable and age as a column variable; value that we want to have in cells is mean ex (on gender dimension)
temp <- PTTZ %>% dcast(year ~  age, value.var = 'ex', fun.aggregate = mean)

# melt - the other way around (from long to short)
# id.vars - row variables
# variable.name - name of column variable that we are creating by melting multiple columns
# value.name - name of a column with values from the cells (before melting)
melt(out,id.vars = c('year', 'sex'), variable.name = 'age',value.name = 'ex') %>% head


## exercise - EuStockMarkets##
EuStockMarkets

# using dcast function change it to two column data set with 
# first column being value of the index and a second one being a name of the index

# using melt function get back to original data set

###########
## tidyr ##
###########

library(tidyr)

# gather - columns into rows
# key - name for a column with names of Edf1:Edf10
# value - name for a column with values
# DAX:FTSE - range of columns that we would like to take into rows
stocks <- EuStockMarkets %>% data.frame %>% mutate(day = 1:n())

stocks_2 <- gather(stocks, key ='index', value ='index_value',DAX:FTSE)
stocks_2 %>% mutate(simple_10_ret = (index_value - lag(index_value,10))/lag(index_value,10)) %>% head(15)

# spread - rows into columns
# key - name for a column with names of Edf1:Edf10
# value - name for a column with values
# DAX:FTSE - range of columns that we would like to take into rows
stocks_2 %>% spread(key = index, value = index_value) %>% head()

