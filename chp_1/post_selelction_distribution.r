library(mvtnorm)
library(quantreg)
source("functions.r")

# Parameters
p <- 200 # Total number of parameters (intercept included)
beta_full <- c(1, 1, 1 / 2, 1 / 5, -1, -1 / 2, -1 / 5) # non-zero parameters
beta <- c(beta_full, rep(0, p - length(beta_full)))
R <- 3000 # Monte Carlo simulations 
n <- 100 # Sample size
beta_thresh <- 1e-3 # threshold for identify zero components

# Regressors
rho <- 0.25 # correlation between adjacent features
mu_reg <- rep(0, p - 1) 
sigma_reg <- matrix(0, p - 1, p - 1)
for (i in 1:nrow(sigma_reg)) {
    for (j in 1:ncol(sigma_reg)) {
        sigma_reg[i, j] <- rho^abs(i - j) # toeplitz structure for var(X)
    }
}

# Error terms
mu_epsilon <- 0
sigma_epsilon <- 1

# Most trivial setting: the design matrix is fixed once and outisde the Monte-Carlo simulation
# Draw regressors
set.seed(123)
Z <- rmvnorm(n, mean = mu_reg, sigma = sigma_reg)
X <- cbind(rep(1, nrow(Z)), Z)
delta <- 1.2

# alpha = 0.5
alpha <- 0.5
coef_matrix_1 <- matrix(0, nrow = R/2, ncol = p)
coef_matrix_2 <- matrix(0, nrow = R / 2, ncol = p)
cov_betas_1 <- list()
sd_beta_1 <- matrix(NA, nrow = R/2, ncol = p)
cov_betas_2 <- list()
sd_beta_2 <- matrix(NA, nrow = R / 2, ncol = p) 
christoffersen_test_1 <- matrix(NA, nrow = R / 2, ncol = 3)
colnames(christoffersen_test_1) <- c("LR_UC", "LR_ind", "LR_CC") 
christoffersen_test_2 <- matrix(NA, nrow = R / 2, ncol = 3)
colnames(christoffersen_test_2) <- c("LR_UC", "LR_ind", "LR_CC")
goodness_of_fit_1 <- numeric(length = R/2)
goodness_of_fit_2 <- numeric(length = R/2)
for (r in 1:(R/2)) {
    cat("Run", r, "\n")

    # Draw innovations and response
    set.seed(R + r)
    epsilon <- rnorm(
        n = n,
        mean = mu_epsilon,
        sd = sigma_epsilon
    )
    epsilon_tilde <- delta * Z[, 1] * epsilon
    y <- X %*% beta + epsilon_tilde

    # First approach: variable selection and model estimation on the same sample
    # Variable selection
    X_standardized <- apply(Z, 2, function(col) (col - mean(col)) / sd(col))
    X_standardized <- cbind(rep(1, nrow(X_standardized)), X_standardized)

    lambda <- quantreg::LassoLambdaHat(X_standardized, R = 1000, tau = alpha, C = 1.1, alpha = 0.9)
    lambda[1] <- 0 # Do not penalize the intercept
    m_lasso <- rq.fit.lasso(X_standardized, y, tau = alpha, lambda = lambda)
    selected_components <- which(abs(coef(m_lasso)) > beta_thresh)
    X_df <- as.data.frame(X[, selected_components])

    # Restricted model estimation
    m <- rq(y ~ . -1, data = X_df, tau = alpha)
    coef_matrix_1[r, selected_components] <- coef(m)
    temp_1 <- summary(m, se = "nid", covariance = TRUE)
    cov_betas_1[[r]] <- temp_1$cov
    sd_beta_1[r, selected_components] <- diag(temp_1$cov)^(1/2) # Sandwich estimator (not i.i.d. hyp.)

    # Second approach: use one sample to select the relevant features, and another one (drawn from the same distribution) to make inference
    set.seed(2*R + r)
    epsilon_oos <- rnorm(
        n = n,
        mean = mu_epsilon,
        sd = sigma_epsilon
    )
    epsilon_oos_tilde <- delta * Z[, 1] * epsilon_oos
    y_oos <- X %*% beta + epsilon_oos_tilde

    m_oos <- rq(y_oos ~ . -1, data = X_df, tau = alpha)
    coef_matrix_2[r, selected_components] <- coef(m_oos)
    temp_2 <- summary(m_oos, se = "nid", covariance = TRUE)
    cov_betas_2[[r]] <- temp_2$cov
    sd_beta_2[r, selected_components] <- diag(temp_2$cov)^(1/2) # Sandwich estimator (not i.i.d. hyp.)

    # Simple goodness-of-fit evaluation on a third sample (still conditional on regressors)
    set.seed(3*R + r)
    epsilon_oos_2 <- rnorm(
        n = n,
        mean = mu_epsilon,
        sd = sigma_epsilon
    )
    epsilon_oos_tilde_2 <- delta * Z[, 1] * epsilon_oos_2
    y_oos_2 <- X %*% beta + epsilon_oos_tilde_2
    
    # Christoffersen's test for the conditional converage hypothesis
    pred_1_oos_2 <- predict(m, newdata = X_df)
    mat_bool_exceptions_1 <- matrix(y_oos_2 < pred_1_oos_2, nrow = n, ncol = 1)
    christoffersen_test_1[r, 1:3] <- unlist(christoffersen_test(mat_bool_exceptions_1, alpha = alpha))
    
    pred_2_oos_2 <- predict(m_oos, newdata = X_df)
    mat_bool_exceptions_2 <- matrix(y_oos_2 < pred_2_oos_2, nrow = n, ncol = 1)
    christoffersen_test_2[r, 1:3] <- unlist(christoffersen_test(mat_bool_exceptions_2, alpha = alpha))

    # OOS Pseudo-R^2
    q_null_model <- rep(quantile(y_oos_2, probs = alpha), length = n)
    goodness_of_fit_1[r] <- pseudo_R2(y_oos_2, pred_1_oos_2, q_null_model, alpha)
    goodness_of_fit_2[r] <- pseudo_R2(y_oos_2, pred_2_oos_2, q_null_model, alpha)
}

# Overall goodness-of-fit: are the goodness-of-fits of the competing models similar or not? 
plot(goodness_of_fit_1,
    goodness_of_fit_2,
    xlab = "pseudo R^2 - approach 1",
    ylab = "pseudo R^2 - approach 2",
    main = "rho = 0.25"
)
abline(a = 0, b = 1, col = "red")

# MSE
mse_1 <- rep(NA, p)
mse_2 <- rep(NA, p)
for (i in 1:p){
    mse_1[i] <- mean((coef_matrix_1[, i] - rep(beta[i], R / 2))^2)
    mse_2[i] <- mean((coef_matrix_2[, i] - rep(beta[i], R / 2))^2)
}
cbind(mse_1, mse_2)
sum(mse_1 - mse_2 > 0)/p

# Kupiec's unconditional coverage test
sum(christoffersen_test_1[, 1] > qchisq(0.95, 1))/(R/2)
sum(christoffersen_test_2[, 1] > qchisq(0.95, 1))/(R/2)

# std. dev. estimators
sd_1 <- apply(coef_matrix_1, 2, function(col) sd(col)) # Monte-Carlo estimates (include 0s from non-selection events)
sd_2 <- apply(coef_matrix_2, 2, function(col) sd(col))
mean_sd_beta_1 <- apply(sd_beta_1, 2, function(col) mean(col, na.rm = TRUE)) # Mean of the sandwich estimates (available only conditional on selections)
mean_sd_beta_2 <- apply(sd_beta_2, 2, function(col) mean(col, na.rm = TRUE))

# Mean estimators
mean_1 <- apply(coef_matrix_1, 2, function(col) mean(col))
mean_2 <- apply(coef_matrix_2, 2, function(col) mean(col))
cond_mean_1 <- apply(coef_matrix_1, 2, function(col) mean(col[col != 0]))
cond_mean_2 <- apply(coef_matrix_2, 2, function(col) mean(col[col != 0]))

# Helper: round y up to the next tick that pretty() would place on the (y-)axis
next_pretty_tick <- function(y) {
    tks <- pretty(c(0, y)) # Those are the ticks used by R to produce a plot
    stp <- tks[2] - tks[1] # tks always contains equally spaced points
    ceiling(y / stp) * stp # Smallest multiple of stp bigger than y
}

# Empirical distributions
# Marginal distributions of non-null estimators
# Approach 1: same-sample bias
par(mfrow = c(2, 3))
for (j in 2:length(beta_full)){
    # Do not consider the intercept

    h <- hist(coef_matrix_1[, j],
        breaks = 20,
        plot = FALSE # Do not plot the histogram: we need to fix ylim parameters
    )
    y_max <- max(c(
        max(h$density),
        dnorm(beta[j], beta[j], sd_1[j]),
        dnorm(beta[j], beta[j], mean_sd_beta_1[j])
    ))
    hist(coef_matrix_1[, j],
        freq = FALSE,
        breaks = 20,
        ylim = c(0, next_pretty_tick(y_max)),
        main = paste("M. D. beta_", j - 1, " - approccio 1", sep = ""),
        xlab = paste("beta_", j - 1, sep = "")
    )
    curve(dnorm(x, beta[j], sd_1[j]), add = TRUE, col = "red", lty = 2)
    curve(dnorm(x, beta[j], mean_sd_beta_1[j]), add = TRUE, col = "blue", lty = 2)
    segments(x0 = beta[j], y0 = 0, x1 = beta[j], y1 = next_pretty_tick(y_max), col = "red") # Prettier version of abline to avoid out of axes lines; *1.05 to align it to 
    segments(x0 = mean_1[j], y0 = 0, x1 = mean_1[j], y1 = next_pretty_tick(y_max), col = "black")
}

# Approach 2: inference on a new sample from the same distribution
par(mfrow = c(2, 3))
for (j in 2:length(beta_full)){

    h <- hist(coef_matrix_2[, j], breaks = 20, plot = FALSE)
    y_max <- max(c(
        max(h$density),
        dnorm(beta[j], beta[j], sd_2[j]),
        dnorm(beta[j], beta[j], mean_sd_beta_2[j])
    ))

    hist(coef_matrix_2[, j],
        freq = FALSE,
        breaks = 20,
        ylim = c(0, next_pretty_tick(y_max)),
        main = paste("M. D. beta_", j - 1, " - approccio 2", sep = ""),
        xlab = paste("beta_", j - 1, sep = "")
    )
    curve(dnorm(x, beta[j], sd_2[j]), add = TRUE, col = "red", lty = 2)
    curve(dnorm(x, beta[j], mean_sd_beta_2[j]), add = TRUE, col = "blue", lty = 2)
    segments(x0 = beta[j], y0 = 0, x1 = beta[j], y1 = next_pretty_tick(y_max), col = "red")
    segments(x0 = mean_2[j], y0 = 0, x1 = mean_2[j], y1 = next_pretty_tick(y_max), col = "black")
}

# Conditional distribution of non-null estimators
selected_matrix <- coef_matrix_1 != 0  # R/2 × p logical; the i-th row identifies the components selected after the first step QR in the i-th run
# Approach 1
par(mfrow = c(2, 3))
for (j in 2:length(beta_full)){
    # Do not consider the intercept
    sel <- selected_matrix[, j]

    h <- hist(coef_matrix_1[sel, j], breaks = 20, plot = FALSE)
    y_max <- max(c(
        max(h$density),
        dnorm(beta[j], beta[j], sd_1[j]),
        dnorm(beta[j], beta[j], mean_sd_beta_1[j])
    ))

    hist(coef_matrix_1[sel, j],
        freq = FALSE,
        breaks = 20,
        ylim = c(0, next_pretty_tick(y_max)),
        main = paste("C. D. beta_", j - 1, " - approccio 1", sep = ""),
        xlab = paste("beta_", j - 1, sep = "")
    )
    curve(dnorm(x, beta[j], sd_1[j]), add = TRUE, col = "red", lty = 2)
    curve(dnorm(x, beta[j], mean_sd_beta_1[j]), add = TRUE, col = "blue", lty = 2)
    segments(x0 = beta[j], y0 = 0, x1 = beta[j], y1 = next_pretty_tick(y_max), col = "red")
    segments(x0 = cond_mean_1[j], y0 = 0, x1 = cond_mean_1[j], y1 = next_pretty_tick(y_max), col = "black")
}

# Approach 2
par(mfrow = c(2, 3))
for (j in 2:length(beta_full)){
    # Do not consider the intercept
    sel <- selected_matrix[, j]

    h <- hist(coef_matrix_2[sel, j], breaks = 20, plot = FALSE)
    y_max <- max(c(
        max(h$density),
        dnorm(beta[j], beta[j], sd_2[j]),
        dnorm(beta[j], beta[j], mean_sd_beta_2[j])
    ))

    hist(coef_matrix_2[sel, j],
        freq = FALSE,
        breaks = 20,
        ylim = c(0, next_pretty_tick(y_max)),
        main = paste("C. D. beta_", j - 1, " - approccio 2", sep = ""),
        xlab = paste("beta_", j - 1, sep = "")
    )
    curve(dnorm(x, beta[j], sd_2[j]), add = TRUE, col = "red", lty = 2)
    curve(dnorm(x, beta[j], mean_sd_beta_2[j]), add = TRUE, col = "blue", lty = 2)
    segments(x0 = beta[j], y0 = 0, x1 = beta[j], y1 = next_pretty_tick(y_max), col = "red")
    segments(x0 = cond_mean_2[j], y0 = 0, x1 = cond_mean_2[j], y1 = next_pretty_tick(y_max), col = "black")
}

# Conditional distribution of some null components' estimators
# Approach 1
par(mfrow = c(2, 3))
for (j in (length(beta_full) + 1):(length(beta_full) + 6)){
    sel <- selected_matrix[, j]

    h <- hist(coef_matrix_1[sel, j], breaks = 20, plot = FALSE)
    y_max <- max(c(
        max(h$density),
        dnorm(beta[j], beta[j], sd_1[j]),
        dnorm(beta[j], beta[j], mean_sd_beta_1[j])
    ))

    hist(coef_matrix_1[sel, j],
        freq = FALSE,
        breaks = 20,
        ylim = c(0, next_pretty_tick(y_max)),
        main = paste("C. D. beta_", j - 1, " - approccio 1", sep = ""),
        xlab = paste("beta_", j - 1, sep = "")
    )
    curve(dnorm(x, beta[j], sd_1[j]), add = TRUE, col = "red", lty = 2)
    curve(dnorm(x, beta[j], mean_sd_beta_1[j]), add = TRUE, col = "blue", lty = 2)
    segments(x0 = beta[j], y0 = 0, x1 = beta[j], y1 = next_pretty_tick(y_max), col = "red")
    segments(x0 = cond_mean_1[j], y0 = 0, x1 = cond_mean_1[j], y1 = next_pretty_tick(y_max), col = "black")
}

# Approach 2
par(mfrow = c(2, 3))
for (j in (length(beta_full) + 1):(length(beta_full) + 6)){
    sel <- selected_matrix[, j]

    h <- hist(coef_matrix_2[sel, j], breaks = 20, plot = FALSE)
    y_max <- max(c(
        max(h$density),
        dnorm(beta[j], beta[j], sd_2[j]),
        dnorm(beta[j], beta[j], mean_sd_beta_2[j])
    ))

    hist(coef_matrix_2[sel, j],
        freq = FALSE,
        breaks = 20,
        ylim = c(0, next_pretty_tick(y_max)),
        main = paste("C. D. beta_", j - 1, " - approccio 2", sep = ""),
        xlab = paste("beta_", j - 1, sep = "")
    )
    curve(dnorm(x, beta[j], sd_2[j]), add = TRUE, col = "red", lty = 2)
    curve(dnorm(x, beta[j], mean_sd_beta_2[j]), add = TRUE, col = "blue", lty = 2)
    segments(x0 = beta[j], y0 = 0, x1 = beta[j], y1 = next_pretty_tick(y_max), col = "red")
    segments(x0 = cond_mean_2[j], y0 = 0, x1 = cond_mean_2[j], y1 = next_pretty_tick(y_max), col = "black")
}

# Conditional bias summary for the non null components
cond_bias_1 <- sapply(2:length(beta_full), function(j) {
    sel <- selected_matrix[, j]
    mean(coef_matrix_1[sel, j]) - beta[j]
})
cond_bias_2 <- sapply(2:length(beta_full), function(j) {
    sel <- selected_matrix[, j]
    mean(coef_matrix_2[sel, j]) - beta[j]
})
rbind(bias_regime1 = cond_bias_1, bias_regime2 = cond_bias_2)

# Conditional  inferential properties
# Confidence intervals at a nominal coverage of 0.95
is_contained_1 <- matrix(NA, nrow = R/2, ncol = p)
for (j in 1:length(beta)){
    indices_included <- which(coef_matrix_1[, j] != 0) # Conditional on selection: consider only the runs in which the current parameter was included
    for (r in indices_included){
        # Compute the bounds of the interval (using the std. dev. estimated in the current run (it exists by definition))
        low_b <- coef_matrix_1[r, j] + -qnorm(0.975) * sd_beta_1[r, j]
        upp_b <- coef_matrix_1[r, j] + qnorm(0.975) * sd_beta_1[r, j]

        # Boolean: is the true value contained in the current confidence interval?
        is_contained_1[r, j] <- (beta[j] >= low_b) & (beta[j] <= upp_b)
    }
}
empirical_coverage_1 <- apply(is_contained_1, 2, function(col) sum(col, na.rm = T) / sum(!is.na(col)))

# Summary statistics for non-zero components
min_coverage_rel_param_1 <- min(empirical_coverage_1[1:length(beta_full)])
mean_coverage_rel_param_1 <-mean(empirical_coverage_1[1:length(beta_full)])
med_coverage_rel_param_1 <-quantile(empirical_coverage_1[1:length(beta_full)], probs = 0.5)
max_coverage_rel_param_1 <-max(empirical_coverage_1[1:length(beta_full)])

# Summary statistics for zero components
min_coverage_non_rel_param_1 <- min(empirical_coverage_1[(length(beta_full) + 1):p])
mean_coverage_non_rel_param_1 <-mean(empirical_coverage_1[(length(beta_full) + 1):p])
med_coverage_non_rel_param_1 <-quantile(empirical_coverage_1[(length(beta_full) + 1):p], probs = 0.5)
max_coverage_non_rel_param_1 <-max(empirical_coverage_1[(length(beta_full) + 1):p])

is_contained_2 <- matrix(NA, nrow = R/2, ncol = p)
for (j in 1:length(beta)) {
    indices_included <- which(coef_matrix_2[, j] != 0)
    for (r in indices_included) {
        low_b <- coef_matrix_2[r, j] + -qnorm(0.975) * sd_beta_2[r, j]
        upp_b <- coef_matrix_2[r, j] + qnorm(0.975) * sd_beta_2[r, j]
        is_contained_2[r, j] <- (beta[j] >= low_b) & (beta[j] <= upp_b)
    }
}
empirical_coverage_2 <- apply(is_contained_2, 2, function(col) sum(col, na.rm = T) / sum(!is.na(col)))

# Summary statistics for non-zero components
min_coverage_rel_param_2 <- min(empirical_coverage_2[1:length(beta_full)])
mean_coverage_rel_param_2 <- mean(empirical_coverage_2[1:length(beta_full)])
med_coverage_rel_param_2 <- quantile(empirical_coverage_2[1:length(beta_full)], probs = 0.5)
max_coverage_rel_param_2 <- max(empirical_coverage_2[1:length(beta_full)])

# Summary statistics for zero components
min_coverage_non_rel_param_2 <- min(empirical_coverage_2[(length(beta_full) + 1):p])
mean_coverage_non_rel_param_2 <- mean(empirical_coverage_2[(length(beta_full) + 1):p])
med_coverage_non_rel_param_2 <- quantile(empirical_coverage_2[(length(beta_full) + 1):p], probs = 0.5)
max_coverage_non_rel_param_2 <- max(empirical_coverage_2[(length(beta_full) + 1):p])




# alpha = 0.25
























# alpha = 0.75

















