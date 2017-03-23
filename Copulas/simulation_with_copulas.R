## Simulation with copulas ##

rm(list=ls())
setwd('C:\\Localdata\\FJ52UY\\RAS\\Copulas')

# libraries
library(Matrix);library(rgl);library(CDVine);library(fitdistrplus);library(MASS)
library(ghyp);library(dplyr)

########################
## problem definition ## slide 3
########################

## we want to calculate Valule at Risk (VAR) on our risk drivers ##
## two market risk drivers: equity index (WIG) and IR (PLN_IR_1Y) ##

## Eqity (WIG) ##
EQ_WIG <- read.csv('EQ_WIG.csv')
EQ_WIG <- EQ_WIG %>% group_by(year_month = substr(Date,1,7)) %>% 
  filter(substr(Date,9,10) == max(substr(Date,9,10))) %>% data.frame
plot(EQ_WIG$EQ_WIG, type = 'l')

## IR (PLN_IR_1R) ##
IR_PLN <-read.csv('IR_PLN.csv') %>% mutate(year_month = substr(Date,1,7))
plot(IR_PLN$YR1, type = 'l')
matplot(IR_PLN[,2:4], type = 'l') # for other tenors

## bind both
risk_drivers <- IR_PLN %>% left_join(EQ_WIG, by = 'year_month') %>% select(-contains('Date'))

# transofrm data
risk_drivers_trans <- risk_drivers %>% 
  mutate(WIG_log = log(EQ_WIG)) %>% 
  mutate(WIG_log_return = WIG_log - lag(WIG_log)) %>%
  mutate(YR1_log_return = log(YR1) - lag(log(YR1))) %>%
  filter(is.na(WIG_log_return) ==F) %>%
  mutate(YR1_log_return_N = scale(YR1_log_return),WIG_log_return_N = scale(WIG_log_return))

# plot data
risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return PDF')
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% plot(main = 'EQ_WIG log return PDF',col = 'red')
risk_drivers_trans %>% select(YR1_log_return_N) %>% as.matrix %>% density %>% plot(main = 'Normalized log return PDF')
risk_drivers_trans %>% select(WIG_log_return_N) %>% as.matrix %>% density %>% lines(col = 'red')
grid()
legend('topright', c('YR1_normalized','EQ_WIG_normalized'), col = c('red','black'), lty=1)


##########################
## fit NIG distribution ## slides 4-5 (4-8)
##########################

YR1_NIG <-fit.NIGuv(risk_drivers_trans$YR1_log_return, silent = T)
EQ_WIG_NIG <-fit.NIGuv(risk_drivers_trans$WIG_log_return, silent = T)


###############################
## simulate from distibution ## slides 6-8
###############################

risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return probability density function',lwd = 2)
qghyp(seq(from=0, to =1, by = 0.001),YR1_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topright',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2),lwd = 2, col = c('black','cadetblue3'))
Sys.sleep(3)
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% plot(main = 'EQ WIG log return probability density function',lwd = 2, ylim = c(0,7))
qghyp(seq(from=0, to =1, by = 0.001),EQ_WIG_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2), lwd = 2, col = c('black','cadetblue3'))


###########################################
## Dependency between those risk drivers ##
###########################################
## we want to capture dependency not just in body, but also in tails and linear correlation will not capture this ##

# plot dependency
plot(risk_drivers_trans$YR1_log_return_N, risk_drivers_trans$WIG_log_return_N, pch = 19,cex = 1.2,
     xlim = c(-4,4), ylim = c(-4,4), xlab = 'YR1 log return - normalized', ylab = 'EQ_WIG log return - normalized')


##########################################
## Intrduction to the concept of Copula ## - slide 9-10
##########################################

sample_size = 50000

# sampling three indpendent uniformly distibutied variables/vectors
uniform_distibutions <- matrix(runif(3*sample_size),sample_size,3) 
hist(uniform_distibutions[,1])

# converting them into normally distibuted with use of inverse cumulative function techniqe (analitical transformation)
normal_distibutions <- qnorm(uniform_distibutions)
plot(density(normal_distibutions[,1]))

# defining dependency parameter
linear_correlation =0.4

# inputing it into correlation matrix as in multivariat distibution
cor_matrix <- Matrix(linear_correlation,3,3, sparse = TRUE)
diag(cor_matrix) <- 1
cor_matrix

# cholesky decomposition - techniqe for conversion of correlation matrix
chol <- chol(cor_matrix)

# incorporating dependency between normal vectors
n_cor <- normal_distibutions %*% chol

# convering normal vectors with dependency into uniform vector
u_cor <- pnorm(n_cor %>% as.matrix)

# convering uniform vectors with dependency into specifc marginal distibution
normal_with_dependency <- qnorm(u_cor) # normal
student_t_with_dependency <- qt(u_cor,df=6) # student-t with 4-degree of freedom

# check their linear correlation
cor(normal_with_dependency)
cor(student_t_with_dependency)

# check kendall tau independence from margins - reduce number of simulations
#cor(student_t_with_dependency, method = 'kendall')


##########################
## Copula Vizualization ##
##########################

plot(normal_distibutions[,1],normal_distibutions[,2]) # no dependency
plot(n_cor[,1],n_cor[,2]) # multivariat normal distibution
plot(normal_with_dependency[,1],normal_with_dependency[,2]) # gaussian copula with normal margins
plot(student_t_with_dependency[,1],student_t_with_dependency[,2]) # gaussian copula with student t margins

# 3D plot
open3d()
mfrow3d(1, 2)
plot3d(normal_with_dependency, xlim =c(-5,5), ylim =c(-5,5), zlim = c(-5,5),col="lightblue", pch =19)
next3d()
plot3d(student_t_with_dependency, xlim =c(-5,5), ylim =c(-5,5), zlim = c(-5,5),col="chartreuse2", pch =19)

# 3D density plot - same copula with different dependency parameter
open3d()
mfrow3d(1, 2)
den3d_wihtout_dep <- kde2d(normal_distibutions[,1], normal_distibutions[,2], n = 20)
persp3d(den3d_wihtout_dep,col="lightblue", box = T, ticktype = 'detailed', main = 'gaussian copula without dependency')
next3d()
den3d_with_dep <- kde2d(normal_with_dependency[,1], normal_with_dependency[,2], n = 20)
persp3d(den3d_with_dep,col="chartreuse2",add = F, main = 'gaussian copula with dependency')

# two dimensional copulas
gaussian_copula <- BiCopSim(20000,family = 1, par= 0.9)
plot(gaussian_copula)
gaussian_copula_with_normal_margins <- qnorm(gaussian_copula)
plot(gaussian_copula_with_normal_margins)
gaussian_copula_with_student_t_margins <- qt(gaussian_copula, df = 5)
plot(gaussian_copula_with_student_t_margins)
#cor(gaussian_copula_with_normal_margins, method = 'kendall')

# multidimentional copulas
gaussian_copula <- BiCopSim(5000,family = c(1,1,1), par= c(0,0,0))

plot3d(gaussian_copula)
gaussian_copula_with_normal_margins <- qnorm(gaussian_copula)
plot3d(gaussian_copula_with_normal_margins, xlab = '',ylab ='', zlab ='')
gaussian_copula_with_student_t_margins <- qt(gaussian_copula, df = 5)
plot3d(gaussian_copula_with_student_t_margins, add = T, col = 'red')


#######################################################
## differnt copulas that capture specific dependency ## slide 11
#######################################################

Clayton_copula <- BiCopSim(10000,family = 3, par= 4)
Clayton_copula_with_normal_margins <- qnorm(Clayton_copula)
plot(Clayton_copula_with_normal_margins)

Gumbel_copula <- BiCopSim(10000,family = 4, par= 4)
Gumbel_copula_with_normal_margins <- qnorm(Gumbel_copula)
plot(Gumbel_copula_with_normal_margins)

BB7_copula <- BiCopSim(10000,family = 9, par= 3, par2 = 3)
#plot(BB7_copula)
BB7_copula_with_normal_margins <- qnorm(BB7_copula)
plot(BB7_copula_with_normal_margins)

par(mfrow=c(2,3))
plot(Clayton_copula_with_normal_margins)
plot(Gumbel_copula_with_normal_margins)
plot(BB7_copula_with_normal_margins)
par(mfrow=c(1,1))

# 3D Gaussian with no dependency
gaussian_copula_3D <- CDVineSim(100000,family = c(1,1,1), par= c(0,0,0), type =1)
gaussian_copula_3D_with_normal_margins <- qnorm(gaussian_copula_3D)
plot3d(gaussian_copula_3D_with_normal_margins)

# 3D Gumbel
Gumbel_copula <- CDVineSim(100000,family = c(4,4,4), par= c(2,2,2), type =1)
Gumbel_copula_with_normal_margins <- qnorm(Gumbel_copula)
plot3d(Gumbel_copula_with_normal_margins)


###################################################################
# simulating Clayton copula without ready to use function for it ## slide 12
###################################################################

theta <- 4
sample_size <- 10000
V <- rgamma(sample_size,1/theta,1)
R <- matrix(rexp(2*sample_size,1),sample_size,2)
U <- (1+ R/V)^(-1/theta)
qnorm(U) %>% plot(pch = 19)
points(Clayton_copula_with_normal_margins, col = 'green')


#######################################
## fiting copula to simulated copula ##
#######################################

estimated_par <- BiCopEst(u1 = pnorm(Clayton_copula_with_normal_margins[,1]),
                          u2 = pnorm(Clayton_copula_with_normal_margins[,2]),
                          family = 3
                          )$par
estimated_par
plot(Clayton_copula_with_normal_margins)
points(BiCopSim(1000,family = 3, par= estimated_par) %>% qnorm(), col = 'red', pch = 19)
legend('bottomright', c('simulated Clayton copula with parameter 4', 'simulated from fitted Clayton copula'),
       col = c('black','red'), pch = c(1,19))
(kendall_tau <- estimated_par/(estimated_par+2) )
cor(Clayton_copula_with_normal_margins, method = 'kendall')


# choosing copula - will it find Clayton copula most appropriate?
BiCopSelect(u1 = pnorm(Clayton_copula_with_normal_margins[,1]),
            u2 = pnorm(Clayton_copula_with_normal_margins[,2])
            )

##################################################################
## using copula to deal with problem described at the beggining ##
##################################################################

# dependency
plot(risk_drivers_trans$YR1_log_return_N, risk_drivers_trans$WIG_log_return_N, pch = 19,cex = 1.2,
     xlim = c(-4,4), ylim = c(-4,4), xlab = 'YR1 log return - normalized', ylab = 'EQ_WIG log return - normalized')
grid()

# tranformation into uniform vectors - since in that implementation copulas are fitted on uniformly distibuted margins
# we are using previously perfomred distibution fitting
uniform_IR_1 <- risk_drivers_trans %>% select(YR1_log_return) %>% pghyp(YR1_NIG)
uniform_WIG <- risk_drivers_trans %>% select(WIG_log_return) %>% pghyp(EQ_WIG_NIG)
hist(uniform_IR_1) # since we are transforming empirical data - it will be as good as our distibution fitting
hist(uniform_WIG)

# selecting most appropriate copula based on AIC criterion
(fitted_copula <- BiCopSelect(u1 = uniform_IR_1,u2 = uniform_WIG))

# since rotated Gumbel copula was choosen we will simulated from this type of copula to get intuition on its shape
simulated_rotated_Gumbel_copula <- BiCopSim(10000,family = fitted_copula$family, par= -3)
plot(qnorm(simulated_rotated_Gumbel_copula))

# simulating copula based of choosen copula and its parameter
simulated_from_fitted_copula<- BiCopSim(2000,family = fitted_copula$family, par= fitted_copula$par, par2 =fitted_copula$par2)
simulated_IR <- simulated_from_fitted_copula[,1] %>% qghyp(YR1_NIG)
simulated_WIG <- simulated_from_fitted_copula[,2] %>% qghyp(EQ_WIG_NIG)


# ploting both empirical data and margins simulated from fitted copula
plot(simulated_IR,simulated_WIG, xlab = 'log returns of IR month to month', ylab = 'log returns of equity index WIG month to month')
points(risk_drivers_trans$YR1_log_return, risk_drivers_trans$WIG_log_return, col = 'red', pch = 19)
grid()
legend('bottomleft', c('empirical data', 'simulated from fitted copula'), col = c('red','black'), pch = c(19,1))

# 3D density plot
open3d()
mfrow3d(1, 2)
den3d_emp <- kde2d(risk_drivers_trans$YR1_log_return, risk_drivers_trans$WIG_log_return)
persp3d(den3d_emp,col="lightblue", box = T, ticktype = 'detailed',xlim=c(-0.2,0.2),ylim =c(-0.2,0.2))
next3d()
den3d_sim <- kde2d(simulated_IR, simulated_WIG)
persp3d(den3d_sim,col="chartreuse2",add = F,xlim=c(-0.2,0.2),ylim =c(-0.2,0.2))


###############
## Exercises ##
###############

## exercise 1 - simulate 2-dimensional copula of your choosing (with normal margins) and visualize it
# functions BiCopSim and qnorm will be useful



## exercise 2 - simulate 3-dimensional copula of your choosing (also mariginal distributions) and visualize it
# functions pairs() and plot3d() could be used for dependency visualization



## exercise 3 - fit copula to absolute changes in IR5 and IR10

# some processing already writen
risk_drivers_trans_exercise <- risk_drivers %>% mutate(YR5_diff = YR5 - lag(YR5), YR10_diff = YR10 - lag(YR10)) %>% 
  filter(is.na(YR5_diff)==F) %>% 
  mutate(YR10_diff_N = scale(YR10_diff),YR5_diff_N = scale(YR5_diff))

# exploring dependency
risk_drivers_trans_exercise %>% select(YR5_diff) %>% as.matrix %>% density %>% plot
risk_drivers_trans_exercise %>% select(YR10_diff) %>% as.matrix %>% density %>% plot

risk_drivers_trans_exercise %>% select(YR5_diff,YR10_diff) %>% plot

## Steps:

# a) fit distribution to YR5_diff and YR10_diff

Y5_fit <- fitdist(risk_drivers_trans_exercise$YR5_diff, distr = 'norm')

# b) transform YR5_diff and YR10_diff into uniform distribution (use cfd function denoted by p letter - for instance pnorm)


# c) fit copula to YR5_diff and YR10_diff


# d) simulate from fitted copula


# e) vizulize results



