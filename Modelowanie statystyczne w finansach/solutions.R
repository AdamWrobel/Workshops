# Cwiczenie 1

model_Los_Angeles <- lm(CA.Los.Angeles ~ National.US,data = HPI_short)


summary(model_Los_Angeles)

plot(y = HPI_short$CA.Los.Angeles, x = HPI_short$National.US, 
     xlim = c(-0.1,0.1), ylim = c(-0.1,0.1))
abline(model_Los_Angeles$coefficients, lwd = 2)
grid()