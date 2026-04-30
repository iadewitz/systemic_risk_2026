library(quantreg)
library(rqPen)
?rqPen::rq.pen
rqPen::rq.pen.cv



# Plain vanilla quantile regression -----------------------------------------------------

# Specificazione più semplice
T <- NROW(stoxx600_logrend_week)
T
t0 <- 52
h <- 1
# test_values <- list()
# for (t in seq(t0 + 1, T)){
#   test_values_m <- matrix(NA, NCOL(stoxx600_logrend_week), NCOL(stoxx600_logrend_week))
#   for (i in 1:NCOL(stoxx600_logrend_week)){
#     y <- stoxx600_logrend_week[(t - 52 + 1):t, i]
#     y_lag_1_abs <- abs(stoxx600_logrend_week[(t - 52):(t - 1), i])
#     for (j in setdiff(1:NCOL(stoxx600_logrend_week), i)){
#       x_lag_1 <- stoxx600_logrend_week[(t - 52):(t - 1), j]
#       x_lag_1_abs <- abs(stoxx600_logrend_week[(t - 52):(t - 1), j])
#       
#       # Stima modello
#       m <- rq(y ~ y_lag_1_abs + x_lag_1 + x_lag_1_abs, tau = 0.01)
#       s <- summary(m, se = 'boot', bsmethod = 'xy', covariance = TRUE)
#       
#       # Estrazione stime
#       beta <- as.matrix(s$coefficients[3:4, 1]) # Vettore colonna
#       
#       # Estrazione stima matrice covarianza dei parametri d'interesse
#       # diag(s$cov)^0.5
#       # s$coefficients[, 2]
#       cov <- s$cov[3:4, 3:4] # Ordine parametri segue quello di specificazione del modello;
#       # a noi interessa quindi il blocco [3:4, 3:4]
#       
#       W <- t(beta) %*% solve(cov) %*% beta
#       test_values_m[j, i] <- W
#     }
#   }
#   test_values[[h]] <- test_values_m
#   h <- h + 1
#   print(t)
# }
stoxx600_week_cut <- stoxx600_week[53:length(stoxx600_week)] # Settimane di riferimento per le reti (le prime 52 perse per
# la stima della prima rete)
load('test_values.Rdata')

# Test Wald con \alpha = 0.05
test_values_bool <- lapply(test_values, function(mat) mat > qchisq(0.95, df = 2))
adj_1 <- lapply(test_values_bool, function(mat) matrix(as.numeric(mat),
                                                       byrow = FALSE,
                                                       ncol = NCOL(stoxx600_logrend_week),
                                                       dimnames = list(colnames(stoxx600_logrend_week), 
                                                                       colnames(stoxx600_logrend_week))))


# QR Lasso

# Vedi lassolambdahat



T <- NROW(stoxx600_logrend_week)
T
t0 <- 52
h <- 1
rq2_test_values <- list()
for (t in seq(t0 + 1, T)){
  test_values_m <- matrix(NA, NCOL(stoxx600_logrend_week), NCOL(stoxx600_logrend_week))
  for (i in 1:NCOL(stoxx600_logrend_week)){
    y <- stoxx600_logrend_week[(t - 52 + 1):t, i]
    y_lag_1_abs <- abs(stoxx600_logrend_week[(t - 52):(t - 1), i])
    j <- setdiff(1:NCOL(stoxx600_logrend_week), i)
      x_lag_1 <- stoxx600_logrend_week[(t - 52):(t - 1), j]
      x_lag_1_abs <- abs(stoxx600_logrend_week[(t - 52):(t - 1), j])

      # Stima modello
      X <- model.matrix(~ y_lag_1_abs + x_lag_1 + x_lag_1_abs) # L'intercetta viene inclusa,
      # ma non verrà penalizzata
      m <- quantreg::rq(y ~ y_lag_1_abs + x_lag_1 + x_lag_1_abs,
                        tau = 0.01,
                        method = 'lasso',
                        lambda = 1)
      s <- summary(m, se = 'boot', bsmethod = 'xy', covariance = TRUE)

      # Estrazione stime
      beta <- as.matrix(s$coefficients[3:4, 1]) # Vettore colonna

      # Estrazione stima matrice covarianza dei parametri d'interesse
      # diag(s$cov)^0.5
      # s$coefficients[, 2]
      cov <- s$cov[3:4, 3:4] # Ordine parametri segue quello di specificazione del modello;
      # a noi interessa quindi il blocco [3:4, 3:4]

      W <- t(beta) %*% solve(cov) %*% beta
      rq2_test_values_m[j, i] <- W
    }
  }
  rq2_test_values[[h]] <- test_values_m
  h <- h + 1
  print(t)
}
stoxx600_week_cut <- stoxx600_week[53:length(stoxx600_week)] # Settimane di riferimento per le reti (le prime 52 perse per
# la stima della prima rete)
load('test_values.Rdata')

# Test Wald con \alpha = 0.05
test_values_bool <- lapply(test_values, function(mat) mat > qchisq(0.95, df = 2))
adj_1 <- lapply(test_values_bool, function(mat) {
  matrix(as.numeric(mat),
    byrow = FALSE,
    ncol = NCOL(stoxx600_logrend_week),
    dimnames = list(
      colnames(stoxx600_logrend_week),
      colnames(stoxx600_logrend_week)
    )
  )
})


# Quantile regression con loss exceedances -------------------------------------------------

# Belloni (2011)-like LASSO penalty 







# SCAD penalty function


























