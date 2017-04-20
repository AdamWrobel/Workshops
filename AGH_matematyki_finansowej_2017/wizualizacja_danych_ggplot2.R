########################################
####### Wizualizacja danych w R ########
############### ggplot2 ################

library(ggplot2)

# step 1 - ggplot() function
ggplot()

# define some data
sim_data <- data.frame(N1 = rnorm(1000,0,1),N2 = rnorm(1000,2,1),P1 = rpois(1000,2),
                       factor_variable = c(rep('type 1',500),rep('type 2',500)))

# step 2 - providing data.frame
ggplot(sim_data)

# step 3 - opening aestetic
ggplot(sim_data, aes())

# step 4 - defining aesteric parameters
ggplot(sim_data, aes(x = N1, y = N2, group = NULL))

# step 5 - adding plot layer
ggplot(sim_data, aes(x = N1, y = N2, group = NULL)) + 
  geom_point()

# step 6 - adding the grouping
ggplot(sim_data, aes(x = N1, y = N2, group = factor_variable, colour = factor_variable)) + 
  geom_point() 

# step 7 - adding the facet
ggplot(sim_data, aes(x = N1, y = N2, group = P1, colour = P1)) + 
  geom_point() + facet_wrap(~factor_variable)


################
### Przyklady ###
################

# Polskie tablice trwania zycia #
PTTZ <- read.csv('Polskie_tablice_trwania_zycia_1990_2015.csv')

head(PTTZ)

# wykres prawdopodobienstwa smierci w ciagu roku dla osoby w wieku 30 lat
PTTZ %>% filter(age == 30) %>%
  ggplot(aes(x = year, y = qx, group = sex, colour = factor(sex))) + geom_line() +
  scale_color_discrete('Sex')

# rozk³ad podopodobienstwa smierci dla osob w wieku 25-30 w podziale na wiek
PTTZ %>% filter(age >= 25, age <= 30) %>% ggplot(aes(qx, group = age, fill = factor(age))) + geom_density(alpha = 0.3) + 
  facet_wrap(~sex)

# rozk³ad podopodobienstwa smierci dla osob w wieku 25-30 w podziale na plec
PTTZ %>% filter(age >= 25, age <= 30) %>% ggplot(aes(qx, group = sex, fill = factor(sex))) + geom_density(alpha = 0.3) + 
  facet_wrap(~age)

# wysymulowane dane #
sim_data2 <- data.frame(N1 = rnorm(10000,0,2), N2 = rnorm(10000,2,4))

sim_data2 %>%
  ggplot(aes(N1, fill = 'N1')) +
  geom_histogram(alpha = 0.4,binwidth=.5, position="identity") +
  geom_histogram(aes(N2, fill = 'N2'),alpha = 0.2,binwidth=.5, position="identity")

plot_to_export <- 
sim_data2 %>%
  ggplot(aes(N1, group = NULL, fill = 'N(0,2)')) +
  geom_histogram(alpha = 0.2,binwidth=.5, position="identity") + 
  geom_histogram(aes(N2, fill = 'N(2,4)'),alpha = 0.2,binwidth=.5, position="identity") +
  scale_fill_discrete('Distibution')

print(plot_to_export)

# export the plot
png('normal_dist_plot.png',width = 1600, height = 900, res = 250)
print(plot_to_export)
dev.off()

## popular geom objects #

# geom_point()
# geom_line()
# geom_bar()
# geom_denisty()
# geom_histogram()
# geom_ribbon()


###########################
### Cwiczenie 2 - iris  ###
###########################

# plot Sepal.Length against Petal.Length with grouping by Species
iris
head(iris)


