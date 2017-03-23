
## exercise 1 - simulate 2-dimensional copula of your choosing (with normal margins) and visualize it
# functions BiCopSim and qnorm will be useful
copula_1 <- BiCopSim(5000,family = 4, par= 3)
copula_1 %>% qnorm(copula_1) %>% plot

## exercise 2 - simulate 3-dimensional copula of your choosing (also mariginal distributions) and visualize it
copula_2 <- BiCopSim(5000,family = c(4,3,4), par= c(3,3,3))
copula_2 %>% qnorm(copula_2) %>% plot3d()
pairs(qnorm(copula_2))


## exercise 3 - fit copula to IR5 and IR10
risk_drivers_trans_exercise <- risk_drivers %>% mutate(YR5_diff = YR5 - lag(YR5), YR10_diff = YR10 - lag(YR10)) %>% 
  filter(is.na(YR5_diff)==F) %>% 
  mutate(YR10_diff_N = scale(YR10_diff),YR5_diff_N = scale(YR5_diff))
risk_drivers_trans_exercise %>% select(YR5_diff) %>% as.matrix %>% density %>% plot
risk_drivers_trans_exercise %>% select(YR10_diff) %>% as.matrix %>% density %>% plot

risk_drivers_trans_exercise %>% select(YR5_diff,YR10_diff) %>% plot

# steps:

# a) fit distribution to YR5_diff and YR10_diff
YR5_NIG <-fit.NIGuv(risk_drivers_trans_exercise$YR5_diff, silent = T)
YR10_NIG <-fit.NIGuv(risk_drivers_trans_exercise$YR10_diff, silent = T)

# b) transform YR5_diff and YR10_diff into uniform distribution
YR5_NIG_unif <- pghyp(risk_drivers_trans_exercise$YR5_diff,YR5_NIG)
YR10_NIG_unif <- pghyp(risk_drivers_trans_exercise$YR10_diff,YR10_NIG)

# c) fit copula to YR5_diff and YR10_diff
fitted_copula_exercise <- BiCopSelect(u1 = YR5_NIG_unif,u2 = YR10_NIG_unif)

# d) simulate from fitted copula
simulated_copula_exercise <- BiCopSim(5000,family = fitted_copula_exercise$family, par = fitted_copula_exercise$par, par2 = fitted_copula_exercise$par2)
simulated_IR5 <- simulated_copula_exercise[,1] %>% qghyp(YR5_NIG)
simulated_IR10 <- simulated_copula_exercise[,2] %>% qghyp(YR10_NIG)
# e) vizulize results
plot(simulated_IR5,simulated_IR10)
points(risk_drivers_trans_exercise$YR5_diff,risk_drivers_trans_exercise$YR10_diff, col = 'red', pch = 19)
grid()
legend('topleft', c('empirical data', 'simulated from fitted copula'), col = c('red','black'), pch = c(19,1))










