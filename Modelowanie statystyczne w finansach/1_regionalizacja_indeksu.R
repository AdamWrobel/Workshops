# Problem 1 - regionalizacja indexu 

setwd('C:/Users/Adam/AGH_modelowanie')

# wczytanie danych
HPI <- read.csv("HPI.csv")

# przejrzenie danych
str(HPI)
head(HPI)

# wczytanie pakietow
library(dplyr)
library(tidyr)
library(ggplot2)

# zamiana typu zmiennej Date
HPI$Date <- HPI$Date %>% as.Date()

# dodajmy zmienna miesi¹c
HPI2 <- HPI %>% mutate(month = substr(Date,6,7))

# przejscie na dlugi format tabeli
HPI_long <- HPI2 %>% gather('Index','Value', AZ.Phoenix:National.US)

# wizualizacja danych
HPI_long %>% ggplot() + geom_line(aes(x = Date, y = Value, group = Index, colour = Index))

# tylko kilka wybranych miast i index dla US
HPI_long %>% filter(Index %in% c('CA.Los.Angeles','MI.Detroit','IL.Chicago','National.US')) %>% 
  ggplot() + geom_line(aes(x = Date, y = Value, Group = Index, colour = Index))

# policzenie stop zwrotu
HPI_long_2<- HPI_long %>% group_by(Index) %>% 
  mutate(simple_quarterly_return = (Value-lag(Value,3))/lag(Value,3)) %>%
  filter(month %in% c('03','06','09','12'))

HPI_short <- HPI_long_2 %>% select(-Value) %>% 
  spread(key = Index, value = simple_quarterly_return)
                                
# pozostawienie danych dla trzech wybranych stanow 
# (w naszym przykladzie to w tych stanach mamy skoncentrowany nasz portfel)
HPI_short <- HPI_short %>% select(Date,National.US,CA.Los.Angeles,MI.Detroit,IL.Chicago)
HPI_short <- HPI_short[complete.cases(HPI_short),]


plot(y = HPI_short$CA.Los.Angeles, x = HPI_short$National.US, 
     xlim = c(-0.1,0.1), ylim = c(-0.1,0.1))
grid()

# Cwiczenia 1
# Zbuduj model, ktory zdefinuje realiacje pomiedzy indeksem z Los Angeles a indexem narodowym
# tak, aby model pozwalal na projekcje indeksu Los Angeles majac zdefiniowana zmiane indeksu narodowego

