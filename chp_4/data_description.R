library(urca)
library(FinTS)
library(rugarch)
library(knitr)
library(kableExtra)

# Summary statistics ------------------------------------------------------

##### Equity #####
# Mean
stoxx600_financial_logret_week_mean <- apply(stoxx600_financial_logret_week, 2, mean)

# Median
stoxx600_financial_logret_week_median <- apply(stoxx600_financial_logret_week, 2, median)

# Standard deviation
stoxx600_financial_logret_week_sd <- apply(stoxx600_financial_logret_week, 2, sd)

# Maximum
stoxx600_financial_logret_week_max <- apply(stoxx600_financial_logret_week, 2, max)

# Minimum
stoxx600_financial_logret_week_min <- apply(stoxx600_financial_logret_week, 2, min)

# Skewness
stoxx600_financial_logret_week_skew <- apply(stoxx600_financial_logret_week, 2, skewness)

# Kurtosis
stoxx600_financial_logret_week_kurt <- apply(stoxx600_financial_logret_week, 2, kurtosis)

# Empirical quantile
stoxx600_financial_logret_week_eq <- apply(
  stoxx600_financial_logret_week,
  2, function(col) quantile(col, probs = 0.05)
)


# Create a data frame with the summary statistics
stoxx600_financial_logret_week_summary <- data.frame(
  Nome = colnames(stoxx600_financial_logret_week),
  TickerRefinitiv = c(bankID, insID, fsID),
  Paese = c(bankCountryComplete, insCountryComplete, fsCountryComplete),
  Media = stoxx600_financial_logret_week_mean,
  Mediana = stoxx600_financial_logret_week_median,
  Dev.Std. = stoxx600_financial_logret_week_sd,
  Max = stoxx600_financial_logret_week_max,
  Min = stoxx600_financial_logret_week_min,
  Asimmetria = stoxx600_financial_logret_week_skew,
  Curtosi = stoxx600_financial_logret_week_kurt,
  "mathcal{Q}_{0.05} (r_j)" = stoxx600_financial_logret_week_eq
)

# Create the table
?kbl
?kable_styling
latexTable <- kbl(
  x = stoxx600_financial_logret_week_summary,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  escape = FALSE
  )

# Wrap in sidewaystable
latexTableResized <- paste0(
  "\\begin{sidewaystable}\n",
  "\\centering\n",
  "\\begin{adjustbox}{width=\\textwidth,height=0.9\\textheight,center,keepaspectratio}\n",
  latexTable,
  "\n}\n",
  "\\end{adjustbox}\n",
  "\\end{sidewaystable}"
)

cat(latexTableResized, file = "tables/summary_statistics_stoxx.tex")

##### State variables #####





# Stationarity check ----------------------------------------------------------------

##### Equity #####

# Augmented Dickey-Fuller test
?ur.df
testStatADF <- numeric(length = ncol(stoxx600_financial_logret_week))
CriticalValue5pctADF <- numeric(length = ncol(stoxx600_financial_logret_week))
nRegressors <- numeric(length = ncol(stoxx600_financial_logret_week))
for (i in 1:NCOL(stoxx600_financial_logret_week)) {
  test <- urca::ur.df(
    stoxx600_financial_logret_week[, i],
    type = "none",
    selectlags = "BIC",
    lags = 10
  )
  testStatADF[i] <- test@teststat
  CriticalValue5pctADF[i] <- test@cval[1, 2] # 0.05 quantile of DF (null) distribution
  nRegressors[i] <- nrow(test@testreg$coefficients)
}
testStatADF # Test statistics 
max(testStatADF) # Maximum test statistic, i.e. the value closest to the critical one
CriticalValue5pctADF # Rejection region: test statistic < critical value
cbind(testStatADF, nRegressors)

# KPSS test
?urca::ur.kpss
testStatKPSS <- numeric(length = ncol(stoxx600_financial_logret_week))
CriticalValue5pctKPSS <- numeric(length = ncol(stoxx600_financial_logret_week))
for (i in 1:NCOL(stoxx600_financial_logret_week)) {
  test <- ur.kpss(
    stoxx600_financial_logret_week[, i],
    type = "mu"  # Constant term in the model to check stationarity around a constant level
  )
  testStatKPSS[i] <- test@teststat
  CriticalValue5pctKPSS[i] <- test@cval[1, 2]
}
testStatKPSS
max(testStatKPSS) # Maximum test statistic, i.e. the value closest to the critical one
CriticalValue5pctKPSS # Acceptance region: test statistic < critical value

# Stationarity check on variance (GARCH(1, 1) parameters)
?ugarchspec
garch11Skewt <- ugarchspec(
  variance.model = list(
  model = "sGARCH",
  garchOrder = c(1, 1)),
  mean.model = list(
  armaOrder = c(0, 0),
  include.mean = TRUE),
  distribution.model = "sstd")
garch11SkewtCoef <- data.frame(
  alpha1 = numeric(ncol(stoxx600_financial_logret_week)),
  beta1 = numeric(ncol(stoxx600_financial_logret_week)),
  shape = numeric(ncol(stoxx600_financial_logret_week)),
  sumAlpha1Beta1 = numeric(ncol(stoxx600_financial_logret_week)),
  isStationary = character(ncol(stoxx600_financial_logret_week))
)

?ugarchfit
for (i in 1:NCOL(stoxx600_financial_logret_week)) {
  print(i)
  set.seed(123)
  mod <- ugarchfit(
    garch11Skewt,
    stoxx600_financial_logret_week[, i],
    solver = "hybrid",
    fit.control = list(stationarity = 0) # No constraint during estimation
  )
  garch11SkewtCoef[i, 1:3] <- mod@fit$coef[c("alpha1", "beta1", "shape")]
  garch11SkewtCoef[i, "sumAlpha1Beta1"] <- sum(garch11SkewtCoef[i, c("alpha1", "beta1")])
  # Stationarity condition for GARCH(1, 1): alpha1 + beta1 < 1
  garch11SkewtCoef[i, "isStationary"] <- ifelse(
    garch11SkewtCoef[i, "sumAlpha1Beta1"] < 1,
    "Yes",
    "No"
  )
}
?qdist
qdist

garch11SkewtCoef
indicesNonStationary <- which(garch11SkewtCoef[, "isStationary"] == "No")
garch11SkewtCoef[indicesNonStationary, ]
colnames(stoxx600_financial_logret_week)[indicesNonStationary] # Indices of non-stationary series
possiblyNotStatEquity <- data.frame(
  Azienda = colnames(stoxx600_financial_logret_week)[indicesNonStationary],
  alpha_1 = garch11SkewtCoef[indicesNonStationary, "alpha1"],
  beta_1 = garch11SkewtCoef[indicesNonStationary, "beta1"],
  sum = garch11SkewtCoef[indicesNonStationary, "sumAlpha1Beta1"]
)
kbl(
  x = possiblyNotStatEquity,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  escape = FALSE,
  row.names = FALSE
)

# Summary statistics of possibly non-stationary series
stoxx600_financial_logret_week_summary[indicesNonStationary, ]

# Remove BPER Banca
stoxx600_financial_logret_week <- stoxx600_financial_logret_week[, -c("BPER BANCA")]










##### Skew-t distribution functions #####
dskewt <- function(y, mu = 0, sigma = 1, nu = 5, xi = 1.5, log = FALSE) {
  # mu: location parameter
  # sigma: scale parameter
  # nu: degrees of freedom
  # xi: skewness parameter

  x <- (y - mu) / sigma # Standardize
  c <- 2 / (xi + xi^(-1)) # Norm constant
  m1 <-  2*sqrt(nu - 2)/(nu - 1)/beta(0.5, 0.5 * nu)
  m2 <- 1
  muXi <- m1*(xi - xi^(-1))
  sigmaXi <- sqrt((m2 - m1^2) * (xi^2 + xi^(-2)) + 2 * m1^2 - m2)
  z <- numeric(length = length(x))
  z[(sigmaXi * x + muXi) < 0] <- xi * (sigmaXi * x + muXi)
  z[(sigmaXi * x + muXi) > 0] <- xi^(-1) * (sigmaXi * x + muXi)
  z[(sigmaXi * x + muXi) == 0] <- (sigmaXi * x + muXi)
  s <- sqrt(nu/(nu - 2))
  dens <- numeric(length = length(z))

  dens <- c*sigmaXi*(dt(z*s, nu))*s
  
  if (log) dens <- log(dens)
  dens
}

?ddist
ddist(distribution = "sstd", y = 0, mu = 0, sigma = 1, skew = 3, shape = 7)
dskewt(0, mu = 0, sigma = 1, nu = 7, xi = 3)
##### end experiment #####




##### State variables #####




