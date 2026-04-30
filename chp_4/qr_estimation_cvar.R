library(rugarch)
source('C:/Users/iadev/Documents/GitHub/CaViAR-R/CaViaR.R')
source('functions.R')

# Some constant values
data <- stoxx600_financial_logret_week
tau <- 0.1 # VaR level 
T <- NROW(data) # Number of observations
K <- NCOL(data) # Number of series
m <- 52 # Width estimation rolling window
M <- T - m # Number of estimation windows
h <- 1 # Prediction horizon

# Years for plots 
x_label_years <- c(as.character(seq(2005, 2021, by = 2))) # Now starting from 2005
x_at_years <- NULL
for (i in 1:length(x_label_years)){
  x_at_years[i] <- min(which(floor(stoxx600_week[(m + 1):T] / 10000) == as.numeric(x_label_years[i]))) # Index of the first day of the years in x_label_years
}



# Empirical quantile ------------------------------------------------------
VaR_tau_eq <- matrix(NA,
                     nrow = M,
                     ncol = K)
colnames(VaR_tau_eq) <- colnames(data)

for (t in 1:M){
  data_train <- data[t:(t + m - 1), ]
  for(j in 1:K){
    VaR_tau_eq[t, j] <- quantile(data_train[, j],
                                 probs = tau)
  }
  print(t)
}

# Plot (example: first ts)
plot(data[m:(T - 1), 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(VaR_tau_eq[, 1],
      col = 'red')

# Backtesting

#Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < VaR_tau_eq[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)


# Contemporaneous






# GARCH -------------------------------------------------------------------

# GARCH(1, 1) gaussian

# # Lagged VaR
# lagged_VaR_tau_garch11_gauss <- matrix(NA,
#                                        nrow = M,
#                                        ncol = K)
# colnames(lagged_VaR_tau_garch11_gauss) <- colnames(data)
# 
# # Predicted contemporaneous VaR
# cont_VaR_tau_garch11_gauss <- lagged_VaR_tau_garch11_gauss
# 
# # Convergence indicator
# VaR_tau_garch11_gauss_convergence <- matrix(NA,
#                                             nrow = M,
#                                             ncol = K)
# 
# # Specification
# garch11_gauss <- ugarchspec(variance.model = list(model = "sGARCH",
#                                                   garchOrder = c(1, 1)),
#                             mean.model = list(armaOrder = c(0, 0),
#                                               include.mean = TRUE),
#                             distribution.model = "norm")
# VaR_tau_garch11_gauss_param <- array(data = 0, dim = c(M, K, 4))
# VaR_tau_garch11_gauss_sd_qml <- array(data = 0, dim = c(M, K, 4))
# 
# start_time <- Sys.time()
# for (t in 1:M){
#   data_train <- data[t:(t + m - 1), ]
#   for(j in 1:K){
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(garch11_gauss,
#                      data_train[, j],
#                      solver = 'hybrid')
#     VaR_tau_garch11_gauss_convergence[t, j] <- mod@fit$solver$sol$convergence
#     
#     if(VaR_tau_garch11_gauss_convergence[t, j] != 0){
#       lagged_VaR_tau_garch11_gauss[t, j] <- cont_VaR_tau_garch11_gauss[t, j] <- quantile(data_train[, j],
#                                                                                          probs = tau)
#     }else{
#       mod_coef <- VaR_tau_garch11_gauss_param[t, j, ] <- mod@fit$coef
#       VaR_tau_garch11_gauss_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#       
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "norm", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0)
#       
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       # sqrt(mod_coef[2] +
#       # mod_coef[3]*(mod_res[m - 1]^2) +
#       # mod_coef[4]*(mod@fit$sigma[m - 1])^2)
#       lagged_VaR_tau_garch11_gauss[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#       
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_garch11_gauss[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#     cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
#   }
#   cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_garch11_gauss_convergence[t, ], '\n')
# }
# 
# # Storing
# save(VaR_tau_garch11_gauss_convergence, file = 'obj/VaR_tau_garch11_gauss_convergence.RData')
# save(VaR_tau_garch11_gauss_param, file = 'obj/VaR_tau_garch11_gauss_param.RData')
# save(VaR_tau_garch11_gauss_sd_qml, file = 'obj/VaR_tau_garch11_gauss_sd_qml.RData')
# save(lagged_VaR_tau_garch11_gauss, file = 'obj/lagged_VaR_tau_garch11_gauss.RData')
# save(cont_VaR_tau_garch11_gauss, file = 'obj/cont_VaR_tau_garch11_gauss.RData')

# Loading
load(file = 'obj/VaR_tau_garch11_gauss_convergence.RData')
load(file = 'obj/VaR_tau_garch11_gauss_param.RData')
load(file = 'obj/VaR_tau_garch11_gauss_sd_qml.RData')
load(file = 'obj/lagged_VaR_tau_garch11_gauss.RData')
load(file = 'obj/cont_VaR_tau_garch11_gauss.RData')

# Plot (example: first ts)

# Lagged
plot(data[m:(T - 1), 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_garch11_gauss[, 1],
      col = 'red')

# Contemporaneous
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(cont_VaR_tau_garch11_gauss[, 1], col = 'blue')

# Backtesting

# Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < lagged_VaR_tau_garch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)


# Contemporanrous
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_garch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)









# GARCH(1, 1) skew-t

# # Lagged VaR
# lagged_VaR_tau_garch11_skewt <- matrix(NA,
#                                        nrow = M,
#                                        ncol = K)
# colnames(lagged_VaR_tau_garch11_skewt) <- colnames(data)
# 
# # Predicted contemporaneous VaR
# cont_VaR_tau_garch11_skewt <- lagged_VaR_tau_garch11_skewt
# 
# # Convergence indicator
# VaR_tau_garch11_skewt_convergence <- matrix(NA,
#                                             nrow = M,
#                                             ncol = K)
# 
# # Specification
# garch11_skewt <- ugarchspec(variance.model = list(model = "sGARCH", 
#                                                   garchOrder = c(1, 1)),
#                             mean.model = list(armaOrder = c(0, 0),
#                                               include.mean = TRUE),
#                             distribution.model = "sstd")
# VaR_tau_garch11_skewt_param <- array(data = 0, dim = c(M, K, 6))
# VaR_tau_garch11_skewt_sd_qml <- array(data = 0, dim = c(M, K, 6))
# 
# start_time <- Sys.time()
# for (t in 1:M){
#   data_train <- data[t:(t + m - 1), ]
#   for(j in 1:K){
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(garch11_skewt,
#                      data_train[, j],
#                      solver = 'hybrid') 
#     VaR_tau_garch11_skewt_convergence[t, j] <- mod@fit$solver$sol$convergence
#     
#     if(VaR_tau_garch11_skewt_convergence[t, j] != 0){
#       lagged_VaR_tau_garch11_skewt[t, j] <- cont_VaR_tau_garch11_skewt[t, j] <- quantile(data_train[, j],
#                                                                                          probs = tau)
#     }else{
#       mod_coef <- VaR_tau_garch11_skewt_param[t, j, ] <- mod@fit$coef
#       VaR_tau_garch11_skewt_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#       
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "sstd", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0,
#                          sigma = 1,
#                          skew = mod_coef[5], 
#                          shape = mod_coef[6]) 
#       
#       # Fitted sigma 
#       fitted_sigma <- mod@fit$sigma[m] 
#       lagged_VaR_tau_garch11_skewt[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#       
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_garch11_skewt[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#     cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
#   }
#   cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_garch11_skewt_convergence[t, ], '\n')
# }
# 
# # Storing
# save(VaR_tau_garch11_skewt_convergence, file = 'obj/VaR_tau_garch11_skewt_convergence.RData')
# save(VaR_tau_garch11_skewt_param, file = 'obj/VaR_tau_garch11_skewt_param.RData')
# save(VaR_tau_garch11_skewt_sd_qml, file = 'obj/VaR_tau_garch11_skewt_sd_qml.RData')
# save(lagged_VaR_tau_garch11_skewt, file = 'obj/lagged_VaR_tau_garch11_skewt.RData')
# save(cont_VaR_tau_garch11_skewt, file = 'obj/cont_VaR_tau_garch11_skewt.RData')

# Loading
load(file = 'obj/VaR_tau_garch11_skewt_convergence.RData')
load(file = 'obj/VaR_tau_garch11_skewt_param.RData')
load(file = 'obj/VaR_tau_garch11_skewt_sd_qml.RData')
load(file = 'obj/lagged_VaR_tau_garch11_skewt.RData')
load(file = 'obj/cont_VaR_tau_garch11_skewt.RData')

# Plot (example: first ts)

# Lagged
plot(data[m:(T - 1), 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_garch11_skewt[, 1],
      col = 'red')

# Contemporaneous
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(cont_VaR_tau_garch11_skewt[, 1], col = 'blue')

# Backtesting

# Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < lagged_VaR_tau_garch11_skewt[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)


# Contemporanrous
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_garch11_skewt[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)






# GJR-GARCH(1, 1) gaussian

# # Lagged VaR
# lagged_VaR_tau_gjrgarch11_gauss <- matrix(NA,
#                                           nrow = M,
#                                           ncol = K)
# colnames(lagged_VaR_tau_gjrgarch11_gauss) <- colnames(data)
# 
# # Predicted contemporaneous VaR
# cont_VaR_tau_gjrgarch11_gauss <- lagged_VaR_tau_gjrgarch11_gauss
#
# # Convergence indicator
# VaR_tau_gjrgarch11_gauss_convergence <- matrix(NA,
#                                                nrow = M,
#                                                ncol = K)
# 
# Specification
# gjrgarch11_gauss <- ugarchspec(variance.model = list(model = "gjrGARCH",
#                                                      garchOrder = c(1, 1)),
#                                mean.model = list(armaOrder = c(0, 0),
#                                                  include.mean = TRUE),
#                                distribution.model = "norm")
# VaR_tau_gjrgarch11_gauss_param <- array(data = 0, dim = c(M, K, 5))
# VaR_tau_gjrgarch11_gauss_sd_qml <- array(data = 0, dim = c(M, K, 5))
# 
# start_time <- Sys.time()
# for (t in 1:M){
#   data_train <- data[t:(t + m - 1), ]
#   for(j in 1:K){
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(gjrgarch11_gauss,
#                      data_train[, j],
#                      solver = 'hybrid',
#                      solver.control = list(delta = 1e-5, tol = 1e-5))
#     VaR_tau_gjrgarch11_gauss_convergence[t, j] <- mod@fit$solver$sol$convergence
#   
#     if(VaR_tau_gjrgarch11_gauss_convergence[t, j] != 0){
#       lagged_VaR_tau_gjrgarch11_gauss[t, j] <- cont_VaR_tau_gjrgarch11_gauss[t, j] <- quantile(data_train[, j],
#                                                                                                probs = tau)
#     }else{
#       mod_coef <- VaR_tau_gjrgarch11_gauss_param[t, j, ] <- mod@fit$coef
#       VaR_tau_gjrgarch11_gauss_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#    
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "norm", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0)
#      
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       lagged_VaR_tau_gjrgarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#    
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_gjrgarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#     cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
#   }
#   cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_gjrgarch11_gauss_convergence[t, ], '\n')
# }
#
# Check NaN
# sum(is.nan(cont_VaR_tau_gjrgarch11_gauss))
# apply(cont_VaR_tau_gjrgarch11_gauss, 2, function(col) sum(is.nan(col)))
# 
# Storing
# save(VaR_tau_gjrgarch11_gauss_convergence, file = 'obj/VaR_tau_gjrgarch11_gauss_convergence_nan.RData')
# save(VaR_tau_gjrgarch11_gauss_param, file = 'obj/VaR_tau_gjrgarch11_gauss_param_nan.RData')
# save(VaR_tau_gjrgarch11_gauss_sd_qml, file = 'obj/VaR_tau_gjrgarch11_gauss_sd_qml_nan.RData')
# save(lagged_VaR_tau_gjrgarch11_gauss, file = 'obj/lagged_VaR_tau_gjrgarch11_gauss_nan.RData')
# save(cont_VaR_tau_gjrgarch11_gauss, file = 'obj/cont_VaR_tau_gjrgarch11_gauss_nan.RData')
# 
# # Re-run for convergence
# start_time <- Sys.time()
# for (j in 1:K){
#   id_nan <- which(is.nan(cont_VaR_tau_gjrgarch11_gauss[, j]))
#   for(t in id_nan){
#     data_train <- data[t:(t + m - 1), j]
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(gjrgarch11_gauss,
#                      data_train,
#                      solver = 'hybrid',
#                      solver.control = list(delta = 1e-18, tol = 1e-18))
#     VaR_tau_gjrgarch11_gauss_convergence[t, j] <- mod@fit$solver$sol$convergence
#   
#     if(VaR_tau_gjrgarch11_gauss_convergence[t, j] != 0){
#       lagged_VaR_tau_gjrgarch11_gauss[t, j] <- cont_VaR_tau_gjrgarch11_gauss[t, j] <- quantile(data_train[, j],
#                                                                                                probs = tau)
#     }else{
#       mod_coef <- VaR_tau_gjrgarch11_gauss_param[t, j, ] <- mod@fit$coef
#       VaR_tau_gjrgarch11_gauss_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#    
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "norm", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0)
#      
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       # sqrt(mod_coef[2] +
#       # mod_coef[3]*(mod_res[m - 1]^2) +
#       # mod_coef[4]*(mod@fit$sigma[m - 1])^2)
#       lagged_VaR_tau_gjrgarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#    
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_gjrgarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#   }
#   cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
# }
# 
# # Check NaN
# sum(is.nan(cont_VaR_tau_gjrgarch11_gauss))
# apply(cont_VaR_tau_gjrgarch11_gauss, 2, function(col) sum(is.nan(col)))
# 
# # Storing
# save(VaR_tau_gjrgarch11_gauss_convergence, file = 'obj/VaR_tau_gjrgarch11_gauss_convergence.RData')
# save(VaR_tau_gjrgarch11_gauss_param, file = 'obj/VaR_tau_gjrgarch11_gauss_param.RData')
# save(VaR_tau_gjrgarch11_gauss_sd_qml, file = 'obj/VaR_tau_gjrgarch11_gauss_sd_qml.RData')
# save(lagged_VaR_tau_gjrgarch11_gauss, file = 'obj/lagged_VaR_tau_gjrgarch11_gauss.RData')
# save(cont_VaR_tau_gjrgarch11_gauss, file = 'obj/cont_VaR_tau_gjrgarch11_gauss.RData')

# Loading
load(file = 'obj/VaR_tau_gjrgarch11_gauss_convergence.RData')
load(file = 'obj/VaR_tau_gjrgarch11_gauss_param.RData')
load(file = 'obj/VaR_tau_gjrgarch11_gauss_sd_qml.RData')
load(file = 'obj/lagged_VaR_tau_gjrgarch11_gauss.RData')
load(file = 'obj/cont_VaR_tau_gjrgarch11_gauss.RData')

# Plot (example: first ts)

# Lagged
plot(data[m:(T - 1), 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_gjrgarch11_gauss[, 1],
      col = 'red')

# Contemporaneous
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(cont_VaR_tau_gjrgarch11_gauss[, 1], col = 'blue')

# Backtesting

# Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < lagged_VaR_tau_gjrgarch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)


# Contemporanrous
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_gjrgarch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)







# GJR-GARCH(1, 1) skew-t

# # Lagged VaR
# lagged_VaR_tau_gjrgarch11_skewt <- matrix(NA,
#                                           nrow = M,
#                                           ncol = K)
# colnames(lagged_VaR_tau_gjrgarch11_skewt) <- colnames(data)
# 
# # Predicted contemporaneous VaR
# cont_VaR_tau_gjrgarch11_skewt <- lagged_VaR_tau_gjrgarch11_skewt
# 
# # Convergence indicator
# VaR_tau_gjrgarch11_skewt_convergence <- matrix(NA,
#                                                nrow = M,
#                                                ncol = K)
# 
# # Specification
# gjrgarch11_skewt <- ugarchspec(variance.model = list(model = "gjrGARCH",
#                                                      garchOrder = c(1, 1)),
#                                mean.model = list(armaOrder = c(0, 0),
#                                                  include.mean = TRUE),
#                                distribution.model = "sstd")
# VaR_tau_gjrgarch11_skewt_param <- array(data = 0, dim = c(M, K, 7))
# VaR_tau_gjrgarch11_skewt_sd_qml <- array(data = 0, dim = c(M, K, 7))
# 
# start_time <- Sys.time()
# for (t in 1:M){
#   data_train <- data[t:(t + m - 1), ]
#   for(j in 1:K){
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(gjrgarch11_skewt,
#                      data_train[, j],
#                      solver = 'hybrid',
#                      solver.control = list(delta = 1e-10, tol = 1e-10))
#     VaR_tau_gjrgarch11_skewt_convergence[t, j] <- mod@fit$solver$sol$convergence
#     
#     if(VaR_tau_gjrgarch11_skewt_convergence[t, j] != 0){
#       lagged_VaR_tau_gjrgarch11_skewt[t, j] <- cont_VaR_tau_gjrgarch11_skewt[t, j] <- quantile(data_train[, j],
#                                                                                                probs = tau)
#     }else{
#       mod_coef <- VaR_tau_gjrgarch11_skewt_param[t, j, ] <- mod@fit$coef
#       VaR_tau_gjrgarch11_skewt_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#       
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "sstd", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0,
#                          sigma = 1,
#                          skew = mod_coef[6], 
#                          shape = mod_coef[7]) 
#       
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       # sqrt(mod_coef[2] +
#       # mod_coef[3]*(mod_res[m - 1]^2) +
#       # mod_coef[4]*(mod@fit$sigma[m - 1])^2)
#       lagged_VaR_tau_gjrgarch11_skewt[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#       
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_gjrgarch11_skewt[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#     cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
#   }
#   cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_gjrgarch11_skewt_convergence[t, ], '\n')
# }
#
# # Check NaN
# sum(is.nan(cont_VaR_tau_gjrgarch11_skewt))
# apply(cont_VaR_tau_gjrgarch11_skewt, 2, function(col) sum(is.nan(col)))
# 
# Storing
# save(VaR_tau_gjrgarch11_skewt_convergence, file = 'obj/VaR_tau_gjrgarch11_skewt_convergence_nan.RData')
# save(VaR_tau_gjrgarch11_skewt_param, file = 'obj/VaR_tau_gjrgarch11_skewt_param_nan.RData')
# save(VaR_tau_gjrgarch11_skewt_sd_qml, file = 'obj/VaR_tau_gjrgarch11_skewt_sd_qml_nan.RData')
# save(lagged_VaR_tau_gjrgarch11_skewt, file = 'obj/lagged_VaR_tau_gjrgarch11_skewt_nan.RData')
# save(cont_VaR_tau_gjrgarch11_skewt, file = 'obj/cont_VaR_tau_gjrgarch11_skewt_nan.RData')
# 
# # Re-run for convergence
# start_time <- Sys.time()
# for (j in 1:K){
#   id_nan <- which(is.nan(cont_VaR_tau_gjrgarch11_skewt[, j]))
#   for(t in id_nan){
#     data_train <- data[t:(t + m - 1), j]
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(gjrgarch11_skewt,
#                      data_train,
#                      solver = 'hybrid',
#                      solver.control = list(delta = 1e-18, tol = 1e-18))
#     VaR_tau_gjrgarch11_skewt_convergence[t, j] <- mod@fit$solver$sol$convergence
# 
#     if(VaR_tau_gjrgarch11_skewt_convergence[t, j] != 0){
#       lagged_VaR_tau_gjrgarch11_skewt[t, j] <- cont_VaR_tau_gjrgarch11_skewt[t, j] <- quantile(data_train[, j],
#                                                                                                probs = tau)
#     }else{
#       mod_coef <- VaR_tau_gjrgarch11_skewt_param[t, j, ] <- mod@fit$coef
#       VaR_tau_gjrgarch11_skewt_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
# 
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "sstd", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0,
#                          sigma = 1,
#                          skew = mod_coef[6],
#                          shape = mod_coef[7])
# 
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       lagged_VaR_tau_gjrgarch11_skewt[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
# 
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_gjrgarch11_skewt[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#   }
#   cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
# }
# 
# # Check NaN
# sum(is.nan(cont_VaR_tau_gjrgarch11_skewt))
# apply(cont_VaR_tau_gjrgarch11_skewt, 2, function(col) sum(is.nan(col)))
# 
# Storing
# save(VaR_tau_gjrgarch11_skewt_convergence, file = 'obj/VaR_tau_gjrgarch11_skewt_convergence.RData')
# save(VaR_tau_gjrgarch11_skewt_param, file = 'obj/VaR_tau_gjrgarch11_skewt_param.RData')
# save(VaR_tau_gjrgarch11_skewt_sd_qml, file = 'obj/VaR_tau_gjrgarch11_skewt_sd_qml.RData')
# save(lagged_VaR_tau_gjrgarch11_skewt, file = 'obj/lagged_VaR_tau_gjrgarch11_skewt.RData')
# save(cont_VaR_tau_gjrgarch11_skewt, file = 'obj/cont_VaR_tau_gjrgarch11_skewt.RData')

# Loading
load(file = 'obj/VaR_tau_gjrgarch11_skewt_convergence.RData')
load(file = 'obj/VaR_tau_gjrgarch11_skewt_param.RData')
load(file = 'obj/VaR_tau_gjrgarch11_skewt_sd_qml.RData')
load(file = 'obj/lagged_VaR_tau_gjrgarch11_skewt.RData')
load(file = 'obj/cont_VaR_tau_gjrgarch11_skewt.RData')

# Plot (example: first ts)
# Lagged
plot(data[m:(T - 1), 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_gjrgarch11_skewt[, 1],
      col = 'red')

# Contemporaneous
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(cont_VaR_tau_gjrgarch11_skewt[, 1], col = 'blue')

# Backtesting

# Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < lagged_VaR_tau_gjrgarch11_skewt[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)


# Contemporanrous
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_gjrgarch11_skewt[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)













# EGARCH(1, 1) gaussian

# # Lagged VaR
# lagged_VaR_tau_egarch11_gauss <- matrix(NA,
#                                         nrow = M,
#                                         ncol = K)
# colnames(lagged_VaR_tau_egarch11_gauss) <- colnames(data)
# 
# # Predicted contemporaneous VaR
# cont_VaR_tau_egarch11_gauss <- lagged_VaR_tau_egarch11_gauss
# 
# # Convergence indicator
# VaR_tau_egarch11_gauss_convergence <- matrix(NA,
#                                              nrow = M,
#                                              ncol = K)
# 
# # Specification
# egarch11_gauss <- ugarchspec(variance.model = list(model = "eGARCH",
#                                                    garchOrder = c(1, 1)),
#                              mean.model = list(armaOrder = c(0, 0),
#                                                include.mean = TRUE),
#                              distribution.model = "norm")
# VaR_tau_egarch11_gauss_param <- array(data = 0, dim = c(M, K, 5))
# VaR_tau_egarch11_gauss_sd_qml <- array(data = 0, dim = c(M, K, 5))
# 
# start_time <- Sys.time()
# for (t in 453:M){
#   data_train <- data[t:(t + m - 1), ]
#   for(j in 1:K){
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(egarch11_gauss,
#                      data_train[, j],
#                      solver = 'hybrid',
#                      solver.control = list(delta = 1e-5, tol = 1e-5))
#     VaR_tau_egarch11_gauss_convergence[t, j] <- mod@fit$solver$sol$convergence
#     
#     if(VaR_tau_egarch11_gauss_convergence[t, j] != 0){
#       lagged_VaR_tau_egarch11_gauss[t, j] <- cont_VaR_tau_egarch11_gauss[t, j] <- quantile(data_train[, j],
#                                                                                            probs = tau)
#     }else{
#       mod_coef <- VaR_tau_egarch11_gauss_param[t, j, ] <- mod@fit$coef
#       VaR_tau_egarch11_gauss_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#       
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "norm", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0)
#       
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       lagged_VaR_tau_egarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#       
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_egarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#     cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
#   }
#   cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_egarch11_gauss_convergence[t, ], '\n')
# }
# 
# # Check NaN
# sum(is.nan(lagged_VaR_tau_egarch11_gauss))
# apply(lagged_VaR_tau_egarch11_gauss, 2, function(col) sum(is.nan(col)))
# 
# sum(is.nan(cont_VaR_tau_egarch11_gauss))
# apply(cont_VaR_tau_egarch11_gauss, 2, function(col) sum(is.nan(col)))
# 
# # Storing
# save(VaR_tau_egarch11_gauss_convergence, file = 'obj/VaR_tau_egarch11_gauss_convergence.RData')
# save(VaR_tau_egarch11_gauss_param, file = 'obj/VaR_tau_egarch11_gauss_param.RData')
# save(VaR_tau_egarch11_gauss_sd_qml, file = 'obj/VaR_tau_egarch11_gauss_sd_qml.RData')
# save(lagged_VaR_tau_egarch11_gauss, file = 'obj/lagged_VaR_tau_egarch11_gauss.RData')
# save(cont_VaR_tau_egarch11_gauss, file = 'obj/cont_VaR_tau_egarch11_gauss.RData')

# Loading
load(file = 'obj/VaR_tau_egarch11_gauss_convergence.RData')
load(file = 'obj/VaR_tau_egarch11_gauss_param.RData')
load(file = 'obj/VaR_tau_egarch11_gauss_sd_qml.RData')
load(file = 'obj/lagged_VaR_tau_egarch11_gauss.RData')
load(file = 'obj/cont_VaR_tau_egarch11_gauss.RData')

# Plot (example: first ts)

# Lagged
plot(data[m:(T - 1), 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_egarch11_gauss[, 1],
      col = 'red')

# Contemporaneous
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(cont_VaR_tau_egarch11_gauss[, 1], col = 'blue')

# Backtesting

# Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < lagged_VaR_tau_egarch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)


# Contemporanrous
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_egarch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             alpha = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
sum(pvalues_christoffersen < 0.05)





# EGARCH(1, 1) skew-t

# # Lagged VaR
# lagged_VaR_tau_egarch11_skewt <- matrix(NA,
#                                         nrow = M,
#                                         ncol = K)
# colnames(lagged_VaR_tau_egarch11_skewt) <- colnames(data)
# 
# # Predicted contemporaneous VaR
# cont_VaR_tau_egarch11_skewt <- lagged_VaR_tau_egarch11_skewt
# 
# # Convergence indicator
# VaR_tau_egarch11_skewt_convergence <- matrix(NA,
#                                              nrow = M,
#                                              ncol = K)
# 
# # Specification
# egarch11_skewt <- ugarchspec(variance.model = list(model = "eGARCH",
#                                                    garchOrder = c(1, 1)),
#                              mean.model = list(armaOrder = c(0, 0),
#                                                include.mean = TRUE),
#                              distribution.model = "sstd")
# VaR_tau_egarch11_skewt_param <- array(data = 0, dim = c(M, K, 7))
# VaR_tau_egarch11_skewt_sd_qml <- array(data = 0, dim = c(M, K, 7))
# 
# start_time <- Sys.time()
# for (t in 1:M){
#   data_train <- data[t:(t + m - 1), ]
#   for(j in 1:K){
#     set.seed(123) # Reproducibility when non deterministic optimization routine
#     mod <- ugarchfit(egarch11_skewt,
#                      data_train[, j],
#                      solver = 'nlminb',
#                      solver.control = list(delta = 1e-18, tol = 1e-18))
#     VaR_tau_egarch11_skewt_convergence[t, j] <- mod@fit$solver$sol$convergence
#     
#     if(VaR_tau_egarch11_skewt_convergence[t, j] != 0){
#       lagged_VaR_tau_egarch11_skewt[t, j] <- cont_VaR_tau_egarch11_skewt[t, j] <- quantile(data_train[, j],
#                                                                                            probs = tau)
#     }else{
#       mod_coef <- VaR_tau_egarch11_skewt_param[t, j, ] <- mod@fit$coef
#       VaR_tau_egarch11_skewt_sd_qml[t, j, ] <- mod@fit$robust.matcoef[, 2]
#       
#       # Innovation \tau quantile
#       q_tau_res <- qdist(distribution = "sstd", # \tau-quantile innovation distribution
#                          p = tau,
#                          mu = 0,
#                          sigma = 1,
#                          skew = mod_coef[6], 
#                          shape = mod_coef[7]) 
#       
#       # Fitted sigma
#       fitted_sigma <- mod@fit$sigma[m]
#       lagged_VaR_tau_egarch11_skewt[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
#       
#       # Predicted sigma
#       p_mod <- ugarchforecast(mod,
#                               n.ahead = h)
#       p_sigma <- drop(p_mod@forecast$sigmaFor)
#       cont_VaR_tau_egarch11_skewt[t, j] <- mod_coef[1] + q_tau_res*p_sigma
#     }
#     cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
#   }
#   cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_egarch11_skewt_convergence[t, ], '\n')
# }
# 
# # Storing
# # save(VaR_tau_egarch11_skewt_convergence, file = 'obj/VaR_tau_egarch11_skewt_convergence.RData')
# # save(VaR_tau_egarch11_skewt_param, file = 'obj/VaR_tau_egarch11_skewt_param.RData')
# # save(VaR_tau_egarch11_skewt_sd_qml, file = 'obj/VaR_tau_egarch11_skewt_sd_qml.RData')
# # save(lagged_VaR_tau_egarch11_skewt, file = 'obj/lagged_VaR_tau_egarch11_skewt.RData')
# # save(cont_VaR_tau_egarch11_skewt, file = 'obj/cont_VaR_tau_egarch11_skewt.RData')
# 
# # Loading
# load(file = 'obj/VaR_tau_egarch11_skewt_convergence.RData')
# load(file = 'obj/VaR_tau_egarch11_skewt_param.RData')
# load(file = 'obj/VaR_tau_egarch11_skewt_sd_qml.RData')
# load(file = 'obj/lagged_VaR_tau_egarch11_skewt.RData')
# load(file = 'obj/cont_VaR_tau_egarch11_skewt.RData')
# 
# # Plot (example: first ts)
# 
# # Lagged
# plot(data[m:(T - 1), 1],
#      type = "l",
#      ylab = "",
#      xlab = "",
#      xaxt = 'n')
# axis(side = 1, at = x_at_years, labels = x_label_years)
# 
# lines(lagged_VaR_tau_egarch11_skewt[, 1],
#       col = 'red')
# 
# # Contemporaneous
# plot(data[(m + 1):T, 1],
#      type = "l",
#      ylab = "",
#      xlab = "",
#      xaxt = 'n')
# axis(side = 1, at = x_at_years, labels = x_label_years)
# 
# lines(cont_VaR_tau_egarch11_skewt[, 1], col = 'blue')
# 
# # Backtesting
# 
# # Lagged
# mat_bool_exceptions <- NULL
# for(j in 1:K){
#   mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[m:(T - 1), j] < lagged_VaR_tau_egarch11_skewt[, j]))
# }
# dim(mat_bool_exceptions)
# 
# # Number of exceptions
# colSums(mat_bool_exceptions)
# 
# # Kupiec and Christoffersen tests
# LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
#                              tau = tau)
# 
# # Kupiec test
# pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
# sum(pvalues_kupiec < 0.05)
# 
# # Christoffersen test
# pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
# sum(pvalues_christoffersen < 0.05)
# 
# 
# # Contemporanrous
# mat_bool_exceptions <- NULL
# for(j in 1:K){
#   mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_egarch11_skewt[, j]))
# }
# dim(mat_bool_exceptions)
# 
# # Number of exceptions
# colSums(mat_bool_exceptions)
# 
# # Kupiec and Christoffersen tests
# LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
#                              tau = tau)
# 
# # Kupiec test
# pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
# sum(pvalues_kupiec < 0.05)
# 
# # Christoffersen test
# pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 2, lower.tail = F)
# sum(pvalues_christoffersen < 0.05)







































# ARMA(1,1)-EGARCH(1, 1) gaussian

# Lagged VaR
lagged_VaR_tau_arma11egarch11_gauss <- matrix(NA,
                                              nrow = M,
                                              ncol = K)
colnames(lagged_VaR_tau_arma11egarch11_gauss) <- colnames(data)

# Predicted contemporaneous VaR
cont_VaR_tau_arma11egarch11_gauss <- lagged_VaR_tau_arma11egarch11_gauss

# Convergence indicator
VaR_tau_arma11egarch11_gauss_convergence <- matrix(NA,
                                                   nrow = M,
                                                   ncol = K)

# Specification
arma11egarch11_gauss <- ugarchspec(variance.model = list(model = "eGARCH", 
                                                         garchOrder = c(1, 1)),
                                   mean.model = list(armaOrder = c(1, 1),
                                                     include.mean = TRUE),
                                   distribution.model = "norm")
VaR_tau_arma11egarch11_gauss_param <- array(data = 0, dim = c(M, K, 7)) # Storing estimates

start_time <- Sys.time()
for (t in 1:M){
  data_train <- data[t:(t + m - 1), ]
  for(j in 1:1){
    set.seed(123) # Reproducibility when non deterministic optimization routine
    mod <- ugarchfit(arma11egarch11_gauss,
                     data_train[, j],
                     solver = 'hybrid') # solver.control = list(iter.max = 10000, eval.max = 10000, tol = 1e-9)
    
    VaR_tau_arma11egarch11_gauss_convergence[t, j] <- mod@fit$solver$sol$convergence # 0 indicates successful convergence
    
    if(VaR_tau_arma11egarch11_gauss_convergence[t, j] != 0){
      lagged_VaR_tau_arma11egarch11_gauss[t, j] <- lagged_VaR_tau_arma11egarch11_gauss[t - 1, j]
      cont_VaR_tau_arma11egarch11_gauss[t, j] <- cont_VaR_tau_arma11egarch11_gauss[t - 1, j]
    }else{
      mod_coef <- VaR_tau_arma11egarch11_gauss_param[t, j, ] <- mod@fit$coef 
      mod_res <- mod@fit$z
      
      # Fitted sigma 
      q_tau_res <- qdist(distribution = "norm", # \tau-quantile conditional distribution of innovations
                         p = tau,
                         mu = 0, 
                         sigma = 1)
      fitted_sigma <- sqrt(exp(mod_coef[2] +
                                 mod_coef[5]*(abs(mod_res[m]) - mean(abs(mod_res))) +
                                 mod_coef[3]*mod_res[m] +
                                 mod_coef[4]*log(mod@fit$sigma[m]^2)))
      lagged_VaR_tau_arma11egarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*fitted_sigma
      
      # Prediction conditional quantile
      p_mod <- ugarchforecast(mod,
                              n.ahead = h)
      p_sigma <- drop(p_mod@forecast$sigmaFor)
      cont_VaR_tau_arma11egarch11_gauss[t, j] <- mod_coef[1] + q_tau_res*p_sigma
    }
    cat(j, format(start_time, "%a %b %d %X %Y"), format(Sys.time(), "%a %b %d %X %Y"),'\n')
  }
  cat(t, format(Sys.time(), "%a %b %d %X %Y"), VaR_tau_arma11egarch11_gauss_convergence[t, ], '\n')
}

# save(VaR_tau_egarch11_gauss_convergence, file = 'obj/VaR_tau_egarch11_gauss_convergence.RData')
# save(lagged_VaR_tau_egarch11_gauss, file = 'obj/lagged_VaR_tau_egarch11_gauss.RData')
# save(cont_VaR_tau_egarch11_gauss, file = 'obj/cont_VaR_tau_egarch11_gauss.RData')
# load(file = 'obj/VaR_tau_egarch11_gauss_convergence.RData')
# load(file = 'obj/lagged_VaR_tau_egarch11_gauss.RData')
# load(file = 'obj/cont_VaR_tau_egarch11_gauss.RData')

# Plot (example: first ts)
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_arma11egarch11_gauss[, 1],
      col = 'red')
lines(cont_VaR_tau_arma11egarch11_gauss[, 1], col = 'blue')

# Backtesting

# Cont VaR
colnames(pred_VaR) <- names_pred_VaR


mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_arma11egarch11_gauss[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

LR_UC <- kupiec_test(mat_bool_sforamenti = mat_bool_exceptions,
                     tau = 0,05,
                     m = T - m)
pchisq(LR_UC, df = 1, lower.tail = F)



# Lagged VaR





















# CAViaR ------------------------------------------------------------------

# CAViAR - Symmetric Absolute Value (SAV)
seed <- 123

# Lagged VaR
lagged_VaR_tau_CAViaR_SAV <- matrix(NA,
                                    nrow = M,
                                    ncol = K)
colnames(lagged_VaR_tau_CAViaR_SAV) <- colnames(data)

# Predicted contemporaneous VaR
cont_VaR_tau_CAViaR_SAV <- lagged_VaR_tau_CAViaR_SAV

# Convergence indicator
VaR_tau_CAViaR_SAV_convergence <- matrix(NA,
                                         nrow = M,
                                         ncol = K)
for (t in 1:M){
  data_train <- data[t:(t + m - 1), ]
  for(j in 1:2){
    mod <- CAViaR(y = data_train[, j],
                  model = 'SAV',
                  tau = tau,
                  control = list(max_iter_out = 500,
                                 max_iter_in = 100,
                                 rel_tol_QR = 1e-10,
                                 rel_tol_param = 1e-10,
                                 trace = 0),
                  seed = seed)
    VaR_tau_CAViaR_SAV_convergence[t, j] <- mod$convergence
    
    # Lagged VaR
    lagged_VaR_tau_CAViaR_SAV[t, j] <- tail(mod$quantile, 1)
    
    # Prediction conditional quantile
    cont_VaR_tau_CAViaR_SAV[t, j] <- mod$prediction
  }
  print(t)
}

# Plot (example: first ts)
plot(data[(m + 1):T, 1],
     type = "l",
     ylab = "",
     xlab = "",
     xaxt = 'n')
axis(side = 1, at = x_at_years, labels = x_label_years)

lines(lagged_VaR_tau_CAViaR_SAV[, 1],
      col = 'red')
lines(cont_VaR_tau_CAViaR_SAV[, 1],
      col = 'blue')

# Backtesting

# Lagged
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < lagged_VaR_tau_CAViaR_SAV[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             tau = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 1, lower.tail = F)
sum(pvalues_christoffersen < 0.05)



# Contemporaneous
mat_bool_exceptions <- NULL
for(j in 1:K){
  mat_bool_exceptions <- cbind(mat_bool_exceptions, as.numeric(data[(m + 1):T, j] < cont_VaR_tau_CAViaR_SAV[, j]))
}
dim(mat_bool_exceptions)

# Number of exceptions
colSums(mat_bool_exceptions)

# Kupiec and Christoffersen tests
LR_CC <- christoffersen_test(mat_bool_exceptions = mat_bool_exceptions,
                             tau = tau)

# Kupiec test
pvalues_kupiec <- pchisq(LR_CC$LR_UC, df = 1, lower.tail = F)
sum(pvalues_kupiec < 0.05)

# Christoffersen test
pvalues_christoffersen <- pchisq(LR_CC$LR_CC, df = 1, lower.tail = F)
sum(pvalues_christoffersen < 0.05)




























# CAViAR - Asymmetric
seed <- 123

# Lagged VaR
lagged_VaR_tau_CAViaR_Asymmetric <- matrix(NA,
                                           nrow = M,
                                           ncol = K)
colnames(lagged_VaR_tau_CAViaR_Asymmetric) <- colnames(data)

# Predicted contemporaneous VaR
cont_VaR_tau_CAViaR_Asymmetric <- lagged_VaR_tau_CAViaR_Asymmetric

# Convergence indicator
VaR_tau_CAViaR_Asymmetric_convergence <- matrix(NA,
                                                nrow = M,
                                                ncol = K)
for (t in 1:M){
  data_train <- data[t:(t + t0 - 1), ]
  for(j in 1:K){
    mod <- CAViaR(y = data_train[, j],
                  model = 'Asymmetric',
                  tau = tau,
                  control = list(max_iter_out = 500,
                                 max_iter_in = 100,
                                 rel_tol_QR = 1e-10,
                                 rel_tol_param = 1e-10,
                                 trace = 0),
                  seed = seed)
    VaR_tau_CAViaR_Asymmetric_convergence[t, j] <- mod$convergence
    
    # Lagged VaR
    lagged_VaR_tau_CAViaR_Asymmetric[t, j] <- tail(mod$quantile, 1)
    
    # Prediction conditional quantile
    cont_VaR_tau_CAViaR_Asymmetric[t, j] <- mod$prediction
  }
  print(t)
}
















# CAViaR - EGARCH(1, 1)










