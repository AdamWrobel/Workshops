

## dplyr - wprowadzenie ##

# load library
library(dplyr)

# pipe notation: %>%
output <- input %>% function()
  
# example
mean_vector <- vector %>% mean()

vector
mean_vector


## dplyr - select i filter ##

# select – selecting columns/variables
input %>% select(column1, column2)
iris %>% select(Petal.Length, Petal.Width, Species)

# filter – filtering on some condition
input %>% filter(type == "type A")
iris %>% filter(Species == "versicolor")


## dplyr – arrange i mutate ##

# arrange – sorting on given column
input %>% arrange(column1)
iris %>% arrange(Petal.Length)

# mutate – creating new variables
input %>% mutate(column3 = pmax(column1, column2))
iris %>% mutate(Petal.Length.squared = Petal.Length^2)


## dplyr – group_by ##

# group_by & mutate
input %>% group_by(type) %>% 
  mutate(mean_per_type = mean(column1))

# iris example
iris %>% group_by(Species) %>% 
  mutate(mean_PL_per_Species = mean(Petal.Length))


# dplyr - summarize

# group_by & summarize
input %>% group_by(type) %>%
  summarize(mean_per_type = mean(column1))

# iris example
iris %>% group_by(Species) %>% 
  summarize(mean_PL_per_Species = mean(Petal.Length))

## Cwiczenie 1 ##
## dplyr – po³¹czenie wszystkich elementów ##

# jeszcze raz zapoznajmy sie z zobiorem iris
iris %>% head

# zaczynajac od iris %>% dodawaj kolejne kroki
# stworz nowa zmienna Petal.Surface, ktora jest zdefiniowana jako Petal.Length*Petal.Width
iris %>% mutate(Petal.Surface = Petal.Length*Petal.Width)

# wyfiltruj tylko te obserwacje, gdzie Petal.Surface > 0.5
iris %>% mutate(Petal.Surface = Petal.Length*Petal.Width) %>% filter(Petal.Surface > 0.5)

# policz srednia z Petal.Surface dla obserwacji, gdzie Petal.Surface > 0.5
iris %>% mutate(Petal.Surface = Petal.Length*Petal.Width) %>% filter(Petal.Surface > 0.5) %>% 
  group_by(Species) %>% summarize(mean_Petal.Surface = mean(Petal.Surface))

