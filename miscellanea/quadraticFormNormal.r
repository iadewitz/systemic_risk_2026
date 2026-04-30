library(mvtnorm)
z <- NULL

M <- 10000
sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
set.seed(123)
z <- mvtnorm::rmvnorm(M, mean = c(0, 0), sigma = sigma)

y <- NULL
for (i in 1:M) {
  y[i] <- t(z[i, ]) %*% solve(sigma) %*% z[i, ] 
}
?hist
hist(y, breaks = 100, main = "Histogram of y", xlab = "y", col = "lightblue", freq = FALSE)
curve(dchisq(x, df = 2), add = TRUE, col = "red", lwd = 2)
