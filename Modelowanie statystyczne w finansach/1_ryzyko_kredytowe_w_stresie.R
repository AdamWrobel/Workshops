# Problem 2 - ryzyko kredytowe

# wczytanie danych
mortgages <- read.csv("mortgages.csv")
mortgages %>% head

betas <- data.frame(city = c('CA.Los.Angeles','MI.Detroit','IL.Chicago'),
                    beta = c(1.487553,1.172492,1.272835))

# join danych
mortgages2 <- mortgages %>% left_join(betas)

# zadanie szoku indeku
National_HPI_stress <- -0.12

# wyliczenie szoku, wartoœci zabezpieczenia w stresie, LTV w stresie
mortgages3 <- 
mortgages2 %>% mutate(shock = beta *National_HPI_stress,
                      collateral_stressed = collateral *(1 +  shock),
                      LTV_stress = loan_size/collateral_stressed)

# wylicznie prawdopobieñstwa zobserwowania niezerowej straty pod warunkiem bankructwa
mortgages4 <- 
mortgages3 %>% mutate(LGD_stress = pnorm(-2.2 + 1.3 * LTV_stress),
                      LGD = pnorm(-2.2 + 1.3 * LTV))

# wizualizacja rozk³adu prawdopobieñstwa zobserwowania niezerowej straty pod warunkiem bankructwa
density(mortgages4$LGD) %>% plot
density(mortgages4$LGD_stress) %>% lines(col = 'blue')
grid()

ggplot(mortgages4) + 
  geom_density(aes(LGD, fill = 'normal'), alpha = 0.5) +
  geom_density(aes(LGD_stress, fill = 'stress'), alpha = 0.5) + 
  facet_wrap(~city)


# wylicznie oczekiwanej straty na portfelu
mortgages5 <- 
  mortgages4 %>% mutate(expected_stress_loss = PD * LGD_stress * loan_size)

# agregacja
mortgages5 %>% group_by(city) %>% summarize(expected_total_loss = sum(expected_stress_loss),
                                            total_exposure = sum(loan_size),
                                            loss_rate = expected_total_loss/total_exposure)

# Cwiczenie 2
# A) zmien szok dla HPI na -0.2 (-20%) i zinterpretuj wyniki


# B) zmieñ parametr beta dla CA.Los.Angeles na 2 (pozostawiaj¹c szok na -0.2) i zinterpretuj wyniki