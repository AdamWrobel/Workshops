
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

###########################
### Cwiczenie 2 - iris  ###
###########################

# plot Sepal.Length against Petal.Length with grouping by Species
iris %>% ggplot(aes(x = Sepal.Length, y = Petal.Length, 
                    group = Species, colour = Species)) + geom_point()

