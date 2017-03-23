## exercise 4 - fit copula to IR5 and IR10 and IR15
risk_drivers_trans_exercise <- risk_drivers %>% mutate(YR5_diff = YR5 - lag(YR5), YR10_diff = YR10 - lag(YR10), YR15_diff = YR15 - lag(YR15)) %>% 
  filter(is.na(YR5_diff)==F) %>% 
  mutate(YR10_diff_N = scale(YR10_diff),YR5_diff_N = scale(YR5_diff))
risk_drivers_trans_exercise %>% select(YR5_diff) %>% as.matrix %>% density %>% plot
risk_drivers_trans_exercise %>% select(YR10_diff) %>% as.matrix %>% density %>% plot

risk_drivers_trans_exercise %>% select(YR5_diff,YR10_diff,YR15_diff) %>% plot3d

# steps:

# a) fit distribution to YR5_diff and YR10_diff
YR5_NIG <-fit.NIGuv(risk_drivers_trans_exercise$YR5_diff, silent = T)
YR10_NIG <-fit.NIGuv(risk_drivers_trans_exercise$YR10_diff, silent = T)
YR15_NIG <-fit.NIGuv(risk_drivers_trans_exercise$YR15_diff, silent = T)

# b) transform YR5_diff and YR10_diff into uniform distribution
YR5_NIG_unif <- pghyp(risk_drivers_trans_exercise$YR5_diff,YR5_NIG)
YR10_NIG_unif <- pghyp(risk_drivers_trans_exercise$YR10_diff,YR10_NIG)
YR15_NIG_unif <- pghyp(risk_drivers_trans_exercise$YR15_diff,YR15_NIG)

# c) fit copula to YR5_diff and YR10_diff
f1 <- BiCopSelect(u1 = YR5_NIG_unif,u2 = YR10_NIG_unif)
f2 <- BiCopSelect(u1 = YR5_NIG_unif,u2 = YR15_NIG_unif)
f3 <- BiCopSelect(u1 = YR10_NIG_unif,u2 = YR15_NIG_unif)

# d) simulate from fitted copula
simulated_copula_exercise2 <- BiCopSim(10000,family = c(f1$family,f2$family, f3$family),
                                       par = c(f1$par,f2$par,f3$par), par2 = c(f1$par2,f2$par2,f3$par2))
simulated_IR5 <- simulated_copula_exercise2[,1] %>% qghyp(YR5_NIG)
simulated_IR10 <- simulated_copula_exercise2[,2] %>% qghyp(YR10_NIG)
simulated_IR15 <- simulated_copula_exercise2[,3] %>% qghyp(YR15_NIG)
# e) vizulize results
plot3d(simulated_IR5,simulated_IR10,simulated_IR15)
plot3d(risk_drivers_trans_exercise$YR5_diff,risk_drivers_trans_exercise$YR10_diff,risk_drivers_trans_exercise$YR15_diff, col = 'red', add=T, size = 5)


D_Vine_copula <- CDVineSeqEst(data.frame(YR5_NIG_unif, YR10_NIG_unif,YR15_NIG_unif),family = c(2,2,2), type = 2)
sim <- CDVineSim(1000,family = c(2,2,2),par=D_Vine_copula$par,par2=D_Vine_copula$par2,type=2)
simulated2_IR5 <- sim[,1] %>% qghyp(YR5_NIG)
simulated2_IR10 <- sim[,2] %>% qghyp(YR10_NIG)
simulated2_IR15 <- sim[,3] %>% qghyp(YR15_NIG)
plot3d(simulated2_IR5,simulated2_IR10,simulated2_IR15, add =T, col = 'blue')


## exercise 5 - fit copula to IR5 and IR10 and EQ_WIG
risk_drivers_trans_exercise2 <- risk_drivers %>% mutate(YR5_diff = YR5 - lag(YR5), YR10_diff = YR10 - lag(YR10), YR15_diff = YR15 - lag(YR15),
                                                        WIG_log = log(EQ_WIG)) %>% mutate(WIG_log_return = WIG_log - lag(WIG_log)) %>% 
  filter(is.na(YR5_diff)==F) 


risk_drivers_trans_exercise2 %>% select(YR5_diff,YR10_diff,WIG_log_return) %>% plot3d

# steps:

# a) fit distribution to YR5_diff and YR10_diff
YR5_NIG <-fit.NIGuv(risk_drivers_trans_exercise2$YR5_diff, silent = T)
YR10_NIG <-fit.NIGuv(risk_drivers_trans_exercise2$YR10_diff, silent = T)
WIG_NIG <-fit.NIGuv(risk_drivers_trans_exercise2$WIG_log_return, silent = T)

# b) transform YR5_diff and YR10_diff into uniform distribution
YR5_NIG_unif <- pghyp(risk_drivers_trans_exercise2$YR5_diff,YR5_NIG)
YR10_NIG_unif <- pghyp(risk_drivers_trans_exercise2$YR10_diff,YR10_NIG)
WIG_NIG_NIG_unif <- pghyp(risk_drivers_trans_exercise2$WIG_log_return,WIG_NIG)

# c) fit copula to YR5_diff and YR10_diff
f1 <- BiCopSelect(u1 = YR5_NIG_unif,u2 = YR10_NIG_unif)
f2 <- BiCopSelect(u1 = YR5_NIG_unif,u2 = WIG_NIG_NIG_unif)
f3 <- BiCopSelect(u1 = YR10_NIG_unif,u2 = WIG_NIG_NIG_unif)

# d) simulate from fitted copula
simulated_copula_exercise2 <- BiCopSim(10000,family = c(f1$family,f2$family, f3$family),
                                       par = c(f1$par,f2$par,f3$par), par2 = c(f1$par2,f2$par2,f3$par2))
simulated_IR5 <- simulated_copula_exercise2[,1] %>% qghyp(YR5_NIG)
simulated_IR10 <- simulated_copula_exercise2[,2] %>% qghyp(YR10_NIG)
simulated_WIG <- simulated_copula_exercise2[,3] %>% qghyp(WIG_NIG)
# e) vizulize results
plot3d(simulated_IR5,simulated_IR10,simulated_WIG)
plot3d(risk_drivers_trans_exercise2$YR5_diff,risk_drivers_trans_exercise2$YR10_diff,risk_drivers_trans_exercise2$WIG_log_return, col = 'red', add=T, size = 5)


D_Vine_copula <- CDVineSeqEst(data.frame(YR5_NIG_unif, YR10_NIG_unif,WIG_NIG_NIG_unif),family = c(2,34,36), type = 2)
sim <- CDVineSim(1000,family = c(2,34,36),par=D_Vine_copula$par,par2=D_Vine_copula$par2,type=2)
simulated3_IR5 <- sim[,1] %>% qghyp(YR5_NIG)
simulated3_IR10 <- sim[,2] %>% qghyp(YR10_NIG)
simulated3_WIG <- sim[,3] %>% qghyp(WIG_NIG)
plot3d(simulated3_IR5,simulated3_IR10,simulated3_WIG, add =T, col = 'blue')

