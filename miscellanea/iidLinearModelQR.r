library(mvtnorm)
library(quantreg)

# Simulate the design matrix
p <- 10
n <- 10000
mu <- rep(0, p)
Sigma <- diag(p)

set.seed(123)
X <- mvtnorm::rmvnorm(n, mean = rep(0, p), sigma  = Sigma)
X <- cbind(1, X)
head(X)

# Simulate errors
set.seed(234)
epsilon <-rnorm(n, mean = 0, sd = 2)

# Compute response
set.seed(345)
beta <- 1 + rnorm((p + 1), mean = 0, sd = 2)
beta
y <- X %*% beta + epsilon

# Linear regression
mLm <- lm(y ~ X - 1)
mLm
summary(mLm)

# Quantile regression
q <- seq(0.05, 0.95, by = 0.05)
mqr <- rq(y ~ X - 1, tau = q)
summary(mqr)

# How much does the intercept change?
str(mqr$coefficients)
mqr$coefficients[1, ] - beta[1]
quantile(epsilon, probs = q)
plot(
    quantile(epsilon, probs = q),
    mqr$coefficients[1, ] - beta[1],
    xlab = "Quantiles of the error",
    ylab = "Intercept change (qr - truth)",
)
abline(0, 1, col = "red")

# At the median we expect to estimate the true value of \beta
mqr$coefficients[1, "tau= 0.50"] - beta[1]

# How much do the slopes change?
mqr$coefficients[2:(p + 1), ] - beta[2:(p + 1)]
boxplot(t(mqr$coefficients[2:(p + 1), ] - beta[2:(p + 1)]))