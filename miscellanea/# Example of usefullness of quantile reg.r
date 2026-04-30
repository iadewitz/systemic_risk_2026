# Example of usefullness of quantile regression
library(quantreg)

# Param
beta0 <- 0.5
beta1 <- 0.7
delta <- 0.4

# Simulate data
m <- 50000 # Nsim

x <- rnorm(m, 5, 1)
epsilon <- rnorm(m, 0, 1)
y <- numeric(m + 1)
y[1] <- 0 # Initial value
for (i in 2:(m + 1)) {
    y[i] <- beta0 + beta1 * y[i - 1] + sqrt(delta * x[i - 1]) * epsilon[i]
}
k <- 100 # Burn-in period

plot(x[(k + 1):m], y[(k + 2):(m + 1)])
plot(y[(k + 1):m], y[(k + 2):(m + 1)])

summary(lm(y[(k + 2):(m + 1)] ~ y[(k + 1):m] + x[(k + 1):m]))

m <- quantreg::rq(y[(k + 2):(m + 1)] ~ y[(k + 1):m] + x[(k + 1):m],
    tau = c(0.1, 0.5, 0.9)
)
summary(m)
