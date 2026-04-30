library(mvtnorm)
library(quantreg)

# Parameters
beta_full <- c(1, 1, 1 / 2, 1 / 3, 1 / 4, 1 / 5)
beta <- c(beta_full, rep(0, 500 - length(beta_full)))
R <- 1000 # Monte Carlo simulations
n <- 100 # Sample size
beta_thresh <- 1e-3

# Regressors
p <- 499
rho <- 0.5
mu_reg <- rep(0, p)
sigma_reg <- matrix(0, p, p)
for (i in 1:nrow(sigma_reg)) {
    for (j in 1:ncol(sigma_reg)) {
        sigma_reg[i, j] <- rho^abs(i - j)
    }
}

# Error terms
mu_epsilon <- 0
sigma_epsilon <- 1    

# Store results
coef_matrix <- matrix(0, nrow = R, ncol = length(beta))

for (r in 1:R) {
    cat("Run", r, "\n")

    # Draw regressors
    set.seed(r)
    Z <- rmvnorm(n, mean = mu_reg, sigma = sigma_reg)
    X <- cbind(rep(1, nrow(Z)), Z)

    # Draw innovations and response
    set.seed(R + r)
    epsilon <- rnorm(
        n = n,
        mean = mu_epsilon,
        sd = sigma_epsilon
    )
    y <- X %*% beta + epsilon

    # Estimation
    X_standardized <- apply(Z, 2, function(col) (col - mean(col)) / sd(col))
    X_standardized <- cbind(rep(1, nrow(X_standardized)), X_standardized)

    lambda <- quantreg::LassoLambdaHat(X_standardized, R = 1000, tau = 0.5, C = 1.1, alpha = 0.9)
    lambda[1] <- 0 # Do not penalize intercept
    m <- rq.fit.lasso(X_standardized, y, tau = 0.5, lambda = lambda)
    coef_matrix[r, ] <- coef(m)

}

selected_components <- coef_matrix > 1e-3
selected_components[1:10, 1:10]
apply(selected_components, 2, function(col) sum(col))
mod_size <- apply(selected_components, 1, function(row) sum(row))
hist(mod_size)




