rm(list = ls())
options(scipen = 999, max.print = 1000)
par(mar = c(5, 3, 2, 2) + 0.1) # Better margins

# Load environment variable
readRenviron("credentials.Renviron") # Load environment variables from .Renviron file
working_dir <- Sys.getenv("WORKING_DIR") # WORKING_DIR contains the path to the directory containing the \macro, \stock folders
if (working_dir == "") {
  stop("DATA_DIR environment variable not set. Please check your .Renviron file.")
}
setwd(working_dir)

# Load libraries
library(readxl)
file_dir <- Sys.getenv("FILE_DIR") # FILE_DIR contains the path to the directory containing the .R files
source(paste(file_dir, "/functions.R", sep = "")) # Load functions

# STOXX 600 ---------------------------------------------------------------

# Bank
stoxx600_bank_data <- read_excel("input/stock/STOXX 600 - D.xlsx",
  skip = 3,
  sheet = 1,
  na = "NA"
)
head(stoxx600_bank_data)

# Code and currency extraction
bankCodeFull <- as.character(stoxx600_bank_data[1, -1]) # Useful for extracting the country
bankCurrencyFull <- as.character(stoxx600_bank_data[2, -1])
stoxx600_bank_data <- stoxx600_bank_data[-c(1, 2), ] # Remove code and currency

class(stoxx600_bank_data) # Weird datatype used by read_excel; let us cast it to data.frame
stoxx600_bank_data <- as.data.frame(stoxx600_bank_data)
str(stoxx600_bank_data)

# Insurance
stoxx600_ins_data <- read_excel("input/stock/STOXX 600 - D.xlsx",
  skip = 3,
  sheet = 2,
  na = "NA"
)
head(stoxx600_ins_data)

# Code and currency extraction
insCodeFull <- as.character(stoxx600_ins_data[1, -1]) # Useful for extracting the country
insCurrencyFull <- as.character(stoxx600_ins_data[2, -1])
stoxx600_ins_data <- stoxx600_ins_data[-c(1, 2), ] # Remove code and currency

stoxx600_ins_data <- as.data.frame(stoxx600_ins_data)
str(stoxx600_ins_data)

# Financial services
stoxx600_fs_data <- read_excel("input/stock/STOXX 600 - D.xlsx",
  skip = 3,
  sheet = 3,
  na = "NA"
)
head(stoxx600_fs_data)

# Code and currency extraction
fsCodeFull <- as.character(stoxx600_fs_data[1, -1]) # Useful for extracting the country
fsCurrencyFull <- as.character(stoxx600_fs_data[2, -1])
stoxx600_fs_data <- stoxx600_fs_data[-c(1, 2), ] # Remove code and currency

stoxx600_fs_data <- as.data.frame(stoxx600_fs_data)
str(stoxx600_fs_data)

# NAs?
sum(is.na(stoxx600_bank_data))
sum(is.na(stoxx600_ins_data))
sum(is.na(stoxx600_fs_data)) # Many; most of them refer to the non-existence of a company for a certain date

# Date format
stoxx600_bank_data[1:5, 1:5] # We need to correctly format dates

# Bank
colnames(stoxx600_bank_data)[1] <- "day"

# The function as.Date needs a numeric argument, not a character one; since we also need to cast all the other
# columns, let's do it now

for (j in 1:NCOL(stoxx600_bank_data)) {
  stoxx600_bank_data[, j] <- as.numeric(stoxx600_bank_data[, j])
}
str(stoxx600_bank_data)

# Date casting
stoxx600_bank_data$day <- as.numeric(format(as.Date(stoxx600_bank_data$day, origin = "1899-12-30"), "%Y%m%d")) # Refinitiv's date management

stoxx600_bank_data[1:5, 1:5]

# Insurance
colnames(stoxx600_ins_data)[1] <- "day"

for (j in 1:NCOL(stoxx600_ins_data)) {
  stoxx600_ins_data[, j] <- as.numeric(stoxx600_ins_data[, j])
}
stoxx600_ins_data[1:5, 1:5]

# Date casting
stoxx600_ins_data$day <- as.numeric(format(as.Date(stoxx600_ins_data$day, origin = "1899-12-30"), "%Y%m%d"))

stoxx600_ins_data[1:5, 1:5]

# Financial services
colnames(stoxx600_fs_data)[1] <- "day"

for (j in 1:NCOL(stoxx600_fs_data)) {
  stoxx600_fs_data[, j] <- as.numeric(stoxx600_fs_data[, j])
}
stoxx600_fs_data[1:5, 1:5]

# Date casting
stoxx600_fs_data$day <- as.numeric(format(as.Date(stoxx600_fs_data$day, origin = "1899-12-30"), "%Y%m%d"))

stoxx600_fs_data[1:5, 1:5]

# Daily log-returns

# Banks
stoxx600_bank_logret <- matrix(
  data = NA,
  nrow = NROW(stoxx600_bank_data) - 1, # We lose the first observation
  ncol = NCOL(stoxx600_bank_data) - 1 # Let us work without date
)
dim(stoxx600_bank_logret)

for (j in 1:NCOL(stoxx600_bank_logret)) {
  stoxx600_bank_logret[, j] <- log(stoxx600_bank_data[2:NROW(stoxx600_bank_data), j + 1]) -
    log(stoxx600_bank_data[1:(NROW(stoxx600_bank_data) - 1), j + 1])
}
stoxx600_bank_logret[1:5, 1:5]

# Check NA
sum(is.na(stoxx600_bank_logret))

# Conversion to data.frame
stoxx600_bank_logret <- data.frame(stoxx600_bank_data$day[-1], stoxx600_bank_logret)
colnames(stoxx600_bank_logret) <- colnames(stoxx600_bank_data)
stoxx600_bank_logret[1:5, 1:5]

# Insurance
stoxx600_ins_logret <- matrix(
  data = NA,
  nrow = NROW(stoxx600_ins_data) - 1,
  ncol = NCOL(stoxx600_ins_data) - 1
)
dim(stoxx600_ins_logret)

for (j in 1:NCOL(stoxx600_ins_logret)) {
  stoxx600_ins_logret[, j] <- log(stoxx600_ins_data[
    2:NROW(stoxx600_ins_data),
    j + 1
  ]) - log(stoxx600_ins_data[1:(NROW(stoxx600_ins_data) - 1), j + 1])
}
stoxx600_ins_logret[1:5, 1:5]

# Check NA
sum(is.na(stoxx600_ins_logret))

# Conversion to data.frame
stoxx600_ins_logret <- data.frame(stoxx600_ins_logret) # No day - see below
colnames(stoxx600_ins_logret) <- colnames(stoxx600_ins_data)[-1]
stoxx600_ins_logret[1:5, 1:5]

# Financial services
stoxx600_fs_logret <- matrix(
  data = NA,
  nrow = NROW(stoxx600_fs_data) - 1,
  ncol = NCOL(stoxx600_fs_data) - 1
)
dim(stoxx600_fs_logret)

for (j in 1:NCOL(stoxx600_fs_logret)) {
  stoxx600_fs_logret[, j] <- log(stoxx600_fs_data[
    2:NROW(stoxx600_fs_data),
    j + 1
  ]) - log(stoxx600_fs_data[1:(NROW(stoxx600_fs_data) - 1), j + 1])
}
stoxx600_fs_logret[1:5, 1:5]

# Check NA
sum(is.na(stoxx600_fs_logret))

# Conversion to data.frame
stoxx600_fs_logret <- data.frame(stoxx600_fs_logret) # No day - see below
colnames(stoxx600_fs_logret) <- colnames(stoxx600_fs_data)[-1]
stoxx600_fs_logret[1:5, 1:5]

##### Removal of weekday holidays #####
holiday_wd_bank <- apply(stoxx600_bank_logret[, -1], 1, function(row) sum(row != 0, na.rm = T)) # holidays_wd contains the number of non-zero
# log-returns for each day; therefore we identify as holiday those days in which few log-returns are different from zero
holiday_wd_ins <- apply(stoxx600_ins_logret, 1, function(row) sum(row != 0, na.rm = T)) 
holiday_wd_fs <- apply(stoxx600_fs_logret, 1, function(row) sum(row != 0, na.rm = T))

# Obtain the total of non-zero log-returns
holiday_wd = holiday_wd_bank + holiday_wd_ins + holiday_wd_fs
holiday_indices <- which(holiday_wd < 10) # What does a few mean?
cbind(stoxx600_bank_logret$day[holiday_indices], holiday_wd[holiday_indices]) # Days and corresponding number of non-zero log returns

# Removal
stoxx600_bank_logret <- stoxx600_bank_logret[-holiday_indices, ]
dim(stoxx600_bank_logret)
stoxx600_ins_logret <- stoxx600_ins_logret[-holiday_indices, ]
dim(stoxx600_ins_logret)
stoxx600_fs_logret <- stoxx600_fs_logret[-holiday_indices, ]
dim(stoxx600_fs_logret)

##### Trim series in the period of interest #####
# Start date 01.01.2004, end date 31.12.2022
indices <- which((stoxx600_bank_logret$day >= 20040101) & (stoxx600_bank_logret$day <= 20221231))

# Bank
stoxx600_bank_logret <- stoxx600_bank_logret[indices, ] # Remove rows with day outside the range of interest
stoxx600_bank_logret[1:5, 1:5]
stoxx600_bank_logret[(NROW(stoxx600_bank_logret) - 4):NROW(stoxx600_bank_logret), 1:5]

# Insurance
stoxx600_ins_logret <- stoxx600_ins_logret[indices, ]
stoxx600_ins_logret[1:5, 1:5]
stoxx600_ins_logret[(NROW(stoxx600_ins_logret) - 4):NROW(stoxx600_ins_logret), 1:5]

# Financial services
stoxx600_fs_logret <- stoxx600_fs_logret[indices, ]
stoxx600_fs_logret[1:5, 1:5]
stoxx600_fs_logret[(NROW(stoxx600_fs_logret) - 4):NROW(stoxx600_fs_logret), 1:5]

# Let us remove the column 'day' and store it in a separate object
stoxx600_day <- stoxx600_bank_logret[, 1]
stoxx600_bank_logret <- stoxx600_bank_logret[, -1]
dim(stoxx600_bank_logret)

# Count institutions before any removal
dim(stoxx600_bank_logret)
colnames(stoxx600_bank_logret)
dim(stoxx600_ins_logret)
colnames(stoxx600_ins_logret)
dim(stoxx600_fs_logret)
colnames(stoxx600_fs_logret)

sum(c(
  NCOL(stoxx600_bank_logret),
  NCOL(stoxx600_ins_logret),
  NCOL(stoxx600_fs_logret)
))

##### Selection of the complete series #####

# We need complete series, so let us remove the non-complete ones
# Complete series <-> no NAs

# Bank
na_bank <- apply(stoxx600_bank_logret, 2, function(col) sum(is.na(col)))
na_bank_indices <- which(na_bank != 0)
na_bank_indices
length(na_bank_indices)
stoxx600_bank_logret <- stoxx600_bank_logret[, -na_bank_indices]

# Check
sum(is.na(stoxx600_bank_logret))

# Bank titles for which we have complete series since the start date
NCOL(stoxx600_bank_logret)
sort(colnames(stoxx600_bank_logret))


# Insurance
na_ins <- apply(stoxx600_ins_logret, 2, function(col) sum(is.na(col)))
na_ins_indices <- which(na_ins != 0)
na_ins_indices
length(na_ins_indices)
stoxx600_ins_logret <- stoxx600_ins_logret[, -na_ins_indices]

# Check
sum(is.na(stoxx600_ins_logret))

# Insurance titles for which we have complete series since the start date
NCOL(stoxx600_ins_logret)
sort(colnames(stoxx600_ins_logret))



# Financial services
na_fs <- apply(stoxx600_fs_logret, 2, function(col) sum(is.na(col)))
na_fs_indices <- which(na_fs != 0)
na_fs_indices
length(na_fs_indices)
stoxx600_fs_logret <- stoxx600_fs_logret[, -na_fs_indices]

# Check
sum(is.na(stoxx600_fs_logret))

# Financial services titles for which we have complete series since the start date
NCOL(stoxx600_fs_logret)
sort(colnames(stoxx600_fs_logret))


# Unified dataset
stoxx600_financial_logret <- cbind(stoxx600_bank_logret, stoxx600_ins_logret, stoxx600_fs_logret)
str(stoxx600_financial_logret)

# Percentage returns
stoxx600_financial_logret <- 100 * stoxx600_financial_logret

# Indices to identify sectors by columns
bankInd <- which(colnames(stoxx600_financial_logret) %in% colnames(stoxx600_bank_logret))
insInd <- which(colnames(stoxx600_financial_logret) %in% colnames(stoxx600_ins_logret))
fsInd <- which(colnames(stoxx600_financial_logret) %in% colnames(stoxx600_fs_logret))



##### Weekly log-returns #####

# Weekly log-returns
T <- NROW(stoxx600_financial_logret)
T # Number of daily observations
stoxx600_financial_logret_week <- NULL
stoxx600_week <- NULL

for (i in 1:floor(T / 5)) {
  stoxx600_financial_logret_week <- rbind(
    stoxx600_financial_logret_week,
    apply(stoxx600_financial_logret[(1 + 5 * (i - 1)):(5 * i), ], 2, sum)
  ) # Usual way to compute multi-horizon returns
  stoxx600_week <- c(stoxx600_week, stoxx600_day[5 * i]) # Week index
}

# Last week adjustment
stoxx600_financial_logret_week <- rbind(
  stoxx600_financial_logret_week,
  apply(stoxx600_financial_logret[(floor(T / 5) * 5 + 1):T, ], 2, sum)
)
stoxx600_week <- c(stoxx600_week, stoxx600_day[T])

dim(stoxx600_financial_logret_week) # 975 complete weekly observations + 1

# Pretty names
colnames(stoxx600_financial_logret_week) <- sapply(colnames(stoxx600_financial_logret_week), function(str) {
  strsplit(str, " -")[[1]][1]
})

head(stoxx600_financial_logret_week)
str(stoxx600_financial_logret_week)

##### Extract ID, Country and Currency of the institutions #####

# Create table to transform abbreviation in complete names 
countryMap <- cbind(
  c("UK", "F", "E", "I", "H", "DK", "N", "B",
   "W", "O", "IE", "PO", "S", "D", "M"),
  c("Regno Unito", "Francia", "Spagna", "Italia", "Paesi Bassi", "Danimarca", "Norvegia", "Belgio",
   "Svezia", "Austria", "Irlanda", "Polonia", "Svizzera", "Germania", "Finlandia")
)

# Bank
key <- attr(attr(stoxx600_financial_logret_week, "dimnames")[[2]], "names")
bankCode <- bankCodeFull[which(colnames(stoxx600_bank_data)[-1] %in% key)]
bankID <- character(length = length(bankCode))
bankCountryAbb <- character(length = length(bankCode))
bankCountryComplete <- character(length = length(bankCode))
for(i in 1:length(bankID)){
  if(length(strsplit(bankCode[i], ":")[[1]]) == 2){
    bankCountryAbb[i] <- strsplit(bankCode[i], ":")[[1]][1]
    bankCountryComplete[i] <- countryMap[which(countryMap[, 1] == bankCountryAbb[i]), 2]
    bankID[i] <- strsplit(strsplit(bankCode[i], ":")[[1]][2], "[(]")[[1]][1] # [] needed to split on a special character
    # in regex
  }else{
    bankCountryAbb[i] <- "UK"
    bankCountryComplete[i] <- countryMap[which(countryMap[, 1] == bankCountryAbb[i]), 2]
    bankID[i] <- strsplit(bankCode[i], "[(]")[[1]][1]
  }
}
bankCurrency <- bankCurrencyFull[which(colnames(stoxx600_bank_data)[-1] %in% key)]
# Check
cbind(bankCode, key[bankInd], bankID, bankCountryAbb, bankCountryComplete, bankCurrency)

# Insurance
key <- attr(attr(stoxx600_financial_logret_week, "dimnames")[[2]], "names")
insCode <- insCodeFull[which(colnames(stoxx600_ins_data)[-1] %in% key)]
insID <- character(length = length(insCode))
insCountryAbb <- character(length = length(insCode))
insCountryComplete <- character(length = length(insCode))
for(i in 1:length(insID)){
  if(length(strsplit(insCode[i], ":")[[1]]) == 2){
    insCountryAbb[i] <- strsplit(insCode[i], ":")[[1]][1]
    insCountryComplete[i] <- countryMap[which(countryMap[, 1] == insCountryAbb[i]), 2]
    insID[i] <- strsplit(strsplit(insCode[i], ":")[[1]][2], "[(]")[[1]][1] # [] needed to split on a special character
    # in regex
  }else{
    insCountryAbb[i] <- "UK"
    insCountryComplete[i] <- countryMap[which(countryMap[, 1] == insCountryAbb[i]), 2]
    insID[i] <- strsplit(insCode[i], "[(]")[[1]][1]
  }
}
insCurrency <- insCurrencyFull[which(colnames(stoxx600_ins_data)[-1] %in% key)]
# Check
cbind(insCode, key[insInd], insID, insCountryAbb, insCountryComplete, insCurrency)

# Financial services
key <- attr(attr(stoxx600_financial_logret_week, "dimnames")[[2]], "names")
fsCode <- fsCodeFull[which(colnames(stoxx600_fs_data)[-1] %in% key)]
fsID <- character(length = length(fsCode))
fsCountryAbb <- character(length = length(fsCode))
fsCountryComplete <- character(length = length(fsCode))
for(i in 1:length(fsID)){
  if(length(strsplit(fsCode[i], ":")[[1]]) == 2){
    fsCountryAbb[i] <- strsplit(fsCode[i], ":")[[1]][1]
    fsCountryComplete[i] <- countryMap[which(countryMap[, 1] == fsCountryAbb[i]), 2]
    fsID[i] <- strsplit(strsplit(fsCode[i], ":")[[1]][2], "[(]")[[1]][1] # [] needed to split on a special character
    # in regex
  }else{
    fsCountryAbb[i] <- "UK"
    fsCountryComplete[i] <- countryMap[which(countryMap[, 1] == fsCountryAbb[i]), 2]
    fsID[i] <- strsplit(fsCode[i], "[(]")[[1]][1]
  }
}
fsCurrency <- fsCurrencyFull[which(colnames(stoxx600_fs_data)[-1] %in% key)]
# Check
cbind(fsCode, key[fsInd], fsID, fsCountryAbb, fsCountryComplete, fsCurrency)




# Macroeconomic variables -------------------------------------------------

##### STOXX 600 returns #####
market_data <- read_excel("input/macro/Market Return.xlsx",
  skip = 3,
  sheet = 1,
  na = "NA"
)
head(market_data)

# Remove code and currency
market_data <- market_data[-c(1, 2), ]

market_data <- as.data.frame(market_data)
str(market_data)


# NAs?
sum(is.na(market_data))

# Casting
colnames(market_data)[1] <- "day"

for (j in 1:NCOL(market_data)) {
  market_data[, j] <- as.numeric(market_data[, j])
}
str(market_data)

# Date casting
market_data$day <- as.numeric(format(as.Date(market_data$day, origin = "1899-12-30"), "%Y%m%d"))

str(market_data)
market_data[1:5, 1:4]

# Daily log-returns
market_logret <- matrix(
  data = NA,
  nrow = NROW(market_data) - 1,
  ncol = NCOL(market_data) - 1
)
dim(market_logret)
for (j in 1:NCOL(market_logret)) {
  market_logret[, j] <- log(market_data[
    2:NROW(market_data),
    j + 1
  ]) - log(market_data[1:(NROW(market_data) - 1), j + 1])
}
market_logret[1:5, 1:3]

# Check NA
sum(is.na(market_logret))

# Conversion to data.frame
market_logret <- data.frame(market_data$day[-1], market_logret)
colnames(market_logret) <- colnames(market_data)
market_logret[1:5, 1:4]



# Selection of the same days as the components'
length(market_logret$day)
length(stoxx600_day)

market_indices <- which(market_logret$day %in% stoxx600_day)
market_logret <- market_logret[market_indices, ]

head(market_logret)
tail(market_logret)

# Check
sum(which(market_logret$day != stoxx600_day))
apply(market_logret, 2, function(col) sum(col == 0))

# Remove days column
market_logret$day <- NULL

# Percentage returns
market_logret <- 100 * market_logret


# Weekly log-returns
T <- NROW(market_logret)
market_logret_week <- NULL

for (i in 1:floor(T / 5)) {
  market_logret_week <- rbind(
    market_logret_week,
    apply(market_logret[(1 + 5 * (i - 1)):(5 * i), ], 2, sum)
  )
}

# Last week adjustment
market_logret_week <- rbind(
  market_logret_week,
  apply(market_logret[(floor(T / 5) * 5 + 1):T, ], 2, sum)
)

dim(market_logret_week) # 975 complete weekly observations + 1
head(market_logret_week)


# Split indices
stoxx600_logret_week <- market_logret_week[, 1]
stoxx50_logret_week <- market_logret_week[, 2]
sp500_logret_week <- market_logret_week[, 3]

rm(list = c("market_data", "market_logret", "market_indices", "market_logret_week"))













##### VSTOXX #####
volatility_data <- read_excel("macro/VSTOXX.xlsx",
  skip = 3,
  sheet = 1,
  na = "NA"
)
head(volatility_data)

# Remove code and currency
volatility_data <- volatility_data[-c(1, 2), ]

volatility_data <- as.data.frame(volatility_data)
str(volatility_data)


# NAs?
sum(is.na(volatility_data))

# Remove columns with at least a NA value
volatility_data <- volatility_data[, -c(2:12)]

# Casting
colnames(volatility_data)[1] <- "day"

for (j in 1:NCOL(volatility_data)) {
  volatility_data[, j] <- as.numeric(volatility_data[, j])
}
str(volatility_data)

# Date casting
volatility_data$day <- as.numeric(format(as.Date(volatility_data$day, origin = "1899-12-30"), "%Y%m%d"))

str(volatility_data)
volatility_data[1:5, 1:2]


#  Weekly levels
volatility_week <- as.data.frame(matrix(NA, nrow = length(stoxx600_week), ncol = 2))
volatility_week[, 1] <- stoxx600_week
volatility_week[, 2] <- volatility_data[which(volatility_data$day %in% volatility_week[, 1]), 2]
colnames(volatility_week) <- c("day", "vstoxx")

# Weekly diff
stoxx600_week[1] # First day of the dif series
# -> This has to be the starting point of the diff'ed series

volatility_week_aug <- rbind(
  as.matrix(volatility_data[which(volatility_data$day == volatility_week[1, 1]) - 5, ]),
  as.matrix(volatility_week)
)
volatility_week_diff_1_1 <- cbind(stoxx600_week, diff(volatility_week_aug[, 2], 1, 1))
colnames(volatility_week_diff_1_1) <- c("day", "vstoxx_diff")
volatility_week_diff_1_1 <- as.data.frame(volatility_week_diff_1_1)
# plot(volatility_week_diff_1_1$vstoxx_diff, type = "l")
# cor(volatility_week$vstoxx, volatility_week_diff_1_1$vstoxx_diff)

# # Daily log-returns are not used, but they are another possible feature
# volatility_logret <- log(volatility_data[2:NROW(volatility_data), 2]) - log(volatility_data[1:(NROW(volatility_data) - 1), 2])
# length(volatility_logret)
#
# # Check NA
# sum(is.na(volatility_logret))
#
# # Conversion to data.frame
# volatility_logret <- data.frame(volatility_data$day[-1], volatility_logret)
# colnames(volatility_logret) <- colnames(volatility_data)
# volatility_logret[1:5, 1:2]
#
#
# # Selection of the same days as the components'
# length(volatility_logret$day)
# length(stoxx600_day)
#
# volatility_indices <- which(volatility_logret$day %in% stoxx600_day)
# volatility_logret <- volatility_logret[volatility_indices, ]
#
# head(volatility_logret)
# tail(volatility_logret)
#
# # Check
# sum(which(volatility_logret$day != stoxx600_day))
# apply(volatility_logret, 2, function(col) sum(col == 0))
#
# # Remove days column
# volatility_logret$day <- NULL
#
# # Weekly log-returns
# T <- NROW(volatility_logret)
# volatility_logret_week <- NULL
#
# for(i in 1:floor(T/5)){
#   volatility_logret_week <- c(volatility_logret_week,
#                            sum(volatility_logret[(1 + 5*(i - 1)):(5*i), 1]))
# }
#
# # Last week adjustment
# volatility_logret_week <- c(volatility_logret_week,
#                          sum(volatility_logret[(floor(T/5)*5 + 1):T, 1]))
#
# length(volatility_logret_week) # 975 complete weekly observations + 1

rm(list = c("volatility_data", "volatility_logret", "volatility_indices"))








##### Par Yield AAA EZ #####
# Used to compute a short term funding liquidity risk measure,

py_3m10y_aaa_data <- read.csv("macro/PY 3M10Y AAA.csv", header = TRUE)
head(py_3m10y_aaa_data)

# Second column is redundant + colnames adjustments
py_3m10y_aaa_data <- py_3m10y_aaa_data[, -2]
colnames(py_3m10y_aaa_data) <- c("day", "py_10y_aaa", "py_3m_aaa")
head(py_3m10y_aaa_data)

# Check datatypes
str(py_3m10y_aaa_data)

# NAs?
sum(is.na(py_3m10y_aaa_data)) # 1 NA
na_id <- apply(py_3m10y_aaa_data, 2, is.na)
apply(na_id, 2, sum)
which(is.na(py_3m10y_aaa_data[, 2]))
py_3m10y_aaa_data[4415:4420, ] # 10years py on decemeber 14th 2021 is missing; let us impute it as
# equal to the value of the corrsponding spot rate (the difference between the the yield curves is minimal - at least
# in that period)
py_3m10y_aaa_data[4417, 2] <- -0.344182 # After ECB correction -0.346887 from ECB spot rate yield curve
py_3m10y_aaa_data[4415:4420, ]


# Date casting
py_3m10y_aaa_data$day <- as.numeric(format(as.Date(py_3m10y_aaa_data$day, origin = "1899-12-30"), "%Y%m%d"))
head(py_3m10y_aaa_data)

#  Weekly levels
py_3m10y_aaa_week <- as.data.frame(matrix(NA, nrow = length(stoxx600_week), ncol = 3))
colnames(py_3m10y_aaa_week) <- colnames(py_3m10y_aaa_data)
py_3m10y_aaa_week[, 1] <- stoxx600_week
py_3m10y_aaa_week[which(py_3m10y_aaa_week[, 1] %in% py_3m10y_aaa_data$day), -1] <- py_3m10y_aaa_data[which(py_3m10y_aaa_data$day %in% py_3m10y_aaa_week[, 1]), -1]


# PY_AAA_3M_week[which(PY_AAA_3M_week[, 1] %in% py_3m10y_aaa_data$day), 2] <- py_3m10y_aaa_data[which(py_3m10y_aaa_data$day %in% PY_AAA_3M_week[, 1]), 2]
head(py_3m10y_aaa_week)
tail(py_3m10y_aaa_week)
dim(py_3m10y_aaa_week)

# Check NAs (occur in those days in which ECB doesn't provide the values for the par yield, but stock markets are open)
sum(is.na(py_3m10y_aaa_week[py_3m10y_aaa_week$day > py_3m10y_aaa_data$day[1], -1]))
na_id <- which(apply(apply(py_3m10y_aaa_week[py_3m10y_aaa_week$day > py_3m10y_aaa_data$day[1], -1], 2, is.na), 1, sum) != 0)
py_3m10y_aaa_week[py_3m10y_aaa_week$day > py_3m10y_aaa_data$day[1], ][na_id, ]

# Let us impute those
for (i in 1:NROW(py_3m10y_aaa_week[py_3m10y_aaa_week$day > py_3m10y_aaa_data$day[1], ][na_id, ])) {
  na_day <- py_3m10y_aaa_week[py_3m10y_aaa_week$day > py_3m10y_aaa_data$day[1], ][na_id, ]$day[i]
  prev_day <- max(py_3m10y_aaa_data$day[py_3m10y_aaa_data$day < na_day])
  py_3m10y_aaa_week[py_3m10y_aaa_week$day == na_day, -1] <- py_3m10y_aaa_data[py_3m10y_aaa_data$day == prev_day, -1]
}

# Weekly diff
py_3m10y_aaa_week_diff_1_1 <- apply(py_3m10y_aaa_week[, -1], 2, function(col) diff(col, 1, 1))
py_3m10y_aaa_week_diff_1_1 <- cbind(py_3m10y_aaa_week$day[-1], py_3m10y_aaa_week_diff_1_1)
colnames(py_3m10y_aaa_week_diff_1_1) <- c(colnames(py_3m10y_aaa_week)[1], paste(colnames(py_3m10y_aaa_week_diff_1_1)[-1], "_diff_1_1", sep = ""))
py_3m10y_aaa_week_diff_1_1 <- as.data.frame(py_3m10y_aaa_week_diff_1_1)
# plot(py_3m10y_aaa_week_diff_1_1$py_10y_aaa_diff_1_1, type = "l")

rm(list = c("py_3m10y_aaa_data"))









##### Par Yield all issuers EZ #####
# Used to compute a short term funding liquidity risk measure

py_3m10y_all_data <- read.csv("macro/PY 3M10Y all.csv", header = TRUE)
head(py_3m10y_all_data)

# Second column is redundant + colnames adjustments
py_3m10y_all_data <- py_3m10y_all_data[, -2]
colnames(py_3m10y_all_data) <- c("day", "py_10y_all", "py_3m_all")
head(py_3m10y_all_data)

# Check datatypes
str(py_3m10y_all_data)

# NAs?
sum(is.na(py_3m10y_all_data))

# Date casting
py_3m10y_all_data$day <- as.numeric(format(as.Date(py_3m10y_all_data$day, origin = "1899-12-30"), "%Y%m%d"))
head(py_3m10y_all_data)

#  Weekly levels
py_3m10y_all_week <- as.data.frame(matrix(NA, nrow = length(stoxx600_week), ncol = 3))
colnames(py_3m10y_all_week) <- colnames(py_3m10y_all_data)
py_3m10y_all_week[, 1] <- stoxx600_week
py_3m10y_all_week[which(py_3m10y_all_week[, 1] %in% py_3m10y_all_data$day), -1] <- py_3m10y_all_data[which(py_3m10y_all_data$day %in% py_3m10y_all_week[, 1]), -1]

head(py_3m10y_all_week)
tail(py_3m10y_all_week)
dim(py_3m10y_all_week)

# Check NAs (occur in those days in which ECB doesn't provide the values for the par yield, but stock markets are open)
sum(is.na(py_3m10y_all_week[py_3m10y_all_week$day > py_3m10y_all_data$day[1], -1]))
na_id <- which(apply(apply(py_3m10y_all_week[py_3m10y_all_week$day > py_3m10y_all_data$day[1], -1], 2, is.na), 1, sum) != 0)
py_3m10y_all_week[py_3m10y_all_week$day > py_3m10y_all_data$day[1], ][na_id, ]

# Let us impute those values to the most recent available yield
for (i in 1:NROW(py_3m10y_all_week[py_3m10y_all_week$day > py_3m10y_all_data$day[1], ][na_id, ])) {
  na_day <- py_3m10y_all_week[py_3m10y_all_week$day > py_3m10y_all_data$day[1], ][na_id, ]$day[i]
  prev_day <- max(py_3m10y_all_data$day[py_3m10y_all_data$day < na_day])
  py_3m10y_all_week[py_3m10y_all_week$day == na_day, -1] <- py_3m10y_all_data[py_3m10y_all_data$day == prev_day, -1]
}

# Weekly diff
py_3m10y_all_week_diff_1_1 <- apply(py_3m10y_all_week[, -1], 2, function(col) diff(col, 1, 1))
py_3m10y_all_week_diff_1_1 <- cbind(py_3m10y_all_week$day[-1], py_3m10y_all_week_diff_1_1)
colnames(py_3m10y_all_week_diff_1_1) <- c(colnames(py_3m10y_all_week)[1], paste(colnames(py_3m10y_all_week_diff_1_1)[-1], "_diff_1_1", sep = ""))
py_3m10y_all_week_diff_1_1
# plot(py_3m10y_all_week_diff_1_1[, 2], type = "l")

rm(list = c("py_3m10y_all_data"))











##### Euribor 3M #####
# Used to compute a short term funding liquidity risk
euribor_3M_data <- read_excel("macro/Euribor 3M.xlsx", skip = 6, col_names = FALSE)
head(euribor_3M_data)

# Casting to df
euribor_3M_data <- as.data.frame(euribor_3M_data)
str(euribor_3M_data)


# NAs?
sum(is.na(euribor_3M_data)) # No NA's

# Date casting
colnames(euribor_3M_data) <- c("day", "euribor_3M")
euribor_3M_data$day <- as.numeric(format(as.Date(euribor_3M_data$day, origin = "1899-12-30"), "%Y%m%d"))
head(euribor_3M_data)

# Weekly levels
euribor_3M_week <- as.data.frame(matrix(NA, nrow = length(stoxx600_week), ncol = 2))
colnames(euribor_3M_week) <- colnames(euribor_3M_data)
euribor_3M_week$day <- stoxx600_week
euribor_3M_week$euribor_3M <- euribor_3M_data[which(euribor_3M_data$day %in% euribor_3M_week$day), 2]

# # Plot
# plot(euribor_3M_week$euribor_3M, type = 'l')

rm(list = c("euribor_3M_data"))







##### Par Yield 10Y Corporate Bonds - issuers at several grades #####
# Relevant to compute a credit risk spread measure
py_corp_10y_data <- read_excel("macro/RY CORP 10Y.xlsx",
  col_names = TRUE
)
head(py_corp_10y_data)

# Remove code
py_corp_10y_data <- py_corp_10y_data[-1, ]

py_corp_10y_data <- as.data.frame(py_corp_10y_data)
str(py_corp_10y_data)

# NAs?
sum(is.na(py_corp_10y_data)) # No NAs

# Casting
colnames(py_corp_10y_data) <- c("day", paste("py_10y_", c("bbb", "aaa", "a", "aa"), sep = ""))

for (j in 1:NCOL(py_corp_10y_data)) {
  py_corp_10y_data[, j] <- as.numeric(py_corp_10y_data[, j])
}
str(py_corp_10y_data)

# Date casting
py_corp_10y_data$day <- as.numeric(format(as.Date(py_corp_10y_data$day, origin = "1899-12-30"), "%Y%m%d"))
head(py_corp_10y_data)

# Weekly levels
py_corp_10y_week <- as.data.frame(matrix(NA, nrow = length(stoxx600_week), ncol = ncol(py_corp_10y_data)))
colnames(py_corp_10y_week) <- colnames(py_corp_10y_data)
py_corp_10y_week$day <- stoxx600_week
py_corp_10y_week[, -1] <- py_corp_10y_data[which(py_corp_10y_data$day %in% py_corp_10y_week$day), -1]

# Plot
plot(py_corp_10y_week[, 2],
  type = "l", col = 2,
  ylim = c(min(py_corp_10y_week[, 2:5]), max(py_corp_10y_week[, 2:5])),
  ylab = "",
  xlab = ""
)
for (i in 3:5) {
  points(py_corp_10y_week[, i], type = "l", col = i)
}
legend("topright", lty = rep(1, 4), lwd = rep(1, 4), col = 2:5, legend = c("BBB", "AAA", "A", "AA"))
# Some value for aaa series is missing -> flat curve

rm(list = c("py_corp_10y_data"))









##### ETF Real Estate #####
housing_data <- read_excel("macro/ETF Real Estate.xlsx",
  skip = 3,
  sheet = 1,
  na = "",
  col_names = TRUE
)
head(housing_data)

# Cating to df
housing_data <- as.data.frame(housing_data)
str(housing_data)

# NAs?
sum(is.na(housing_data)) # Start date: 2005-11-23

# Date casting
colnames(housing_data) <- c("day", "real_estate_tr")

housing_data$day <- as.numeric(format(as.Date(housing_data$day, origin = "1899-12-30"), "%Y%m%d"))
housing_data[1:5, 1:2]

# Daily log-returns
housing_logret <- 100 * (log(housing_data[2:NROW(housing_data), 2]) - log(housing_data[1:(NROW(housing_data) - 1), 2]))
length(housing_logret)

# Check NA
sum(is.na(housing_logret))

# Conversion to data.frame
housing_logret <- data.frame(housing_data$day[-1], housing_logret)
colnames(housing_logret) <- colnames(housing_data)
housing_logret[1:5, 1:2]


# Selection of the same days as the components'
length(housing_logret$day)
length(stoxx600_day)

housing_indices <- which(housing_logret$day %in% stoxx600_day)
housing_logret <- housing_logret[housing_indices, ]

head(housing_logret)
tail(housing_logret)

# Check
sum(which(housing_logret$day != stoxx600_day))
apply(housing_logret, 2, function(col) sum(col == 0, na.rm = TRUE))

# Weekly log-returns
T <- NROW(housing_logret)
housing_logret_week <- NULL

for (i in 1:floor(T / 5)) {
  housing_logret_week <- c(
    housing_logret_week,
    sum(housing_logret[(1 + 5 * (i - 1)):(5 * i), 2])
  )
}

# Last week adjustment
housing_logret_week <- c(
  housing_logret_week,
  sum(housing_logret[(floor(T / 5) * 5 + 1):T, 2])
)

# NA's
sum(is.na(housing_logret_week)) # The time series starts 98 weeks after the origin

# Casting to df
housing_logret_week <- as.data.frame(cbind(stoxx600_week, housing_logret_week))
head(housing_logret_week)
tail(housing_logret_week)

rm(list = c("housing_data", "housing_logret", "housing_indices"))


# ##### STOXX Real Estate #####
# Wrong real estate data

# housing_data <- read_excel("macro/STOXX Real Estate.xlsx",
#   skip = 3,
#   sheet = 1,
#   na = "NA"
# )
# head(housing_data)

# # Remove code and currency
# housing_data <- housing_data[-c(1, 2), ]

# housing_data <- as.data.frame(housing_data)
# str(housing_data)


# # NAs?
# sum(is.na(housing_data))

# # We have to remove Total Return Index - other solutions?
# housing_data <- housing_data[, -2]

# # Casting
# colnames(housing_data)[1] <- "day"

# for (j in 1:NCOL(housing_data)) {
#   housing_data[, j] <- as.numeric(housing_data[, j])
# }
# str(housing_data)

# # Date casting
# housing_data$day <- as.numeric(format(as.Date(housing_data$day, origin = "1899-12-30"), "%Y%m%d"))

# str(housing_data)
# housing_data[1:5, 1:2]

# # Daily log-returns
# housing_logret <- log(housing_data[2:NROW(housing_data), 2]) - log(housing_data[1:(NROW(housing_data) - 1), 2])
# length(housing_logret)

# # Check NA
# sum(is.na(housing_logret))

# # Conversion to data.frame
# housing_logret <- data.frame(housing_data$day[-1], housing_logret)
# colnames(housing_logret) <- colnames(housing_data)
# housing_logret[1:5, 1:2]


# # Selection of the same days as the components'
# length(housing_logret$day)
# length(stoxx600_day)

# housing_indices <- which(housing_logret$day %in% stoxx600_day)
# housing_logret <- housing_logret[housing_indices, ]

# head(housing_logret)
# tail(housing_logret)

# # Check
# sum(which(housing_logret$day != stoxx600_day))
# apply(housing_logret, 2, function(col) sum(col == 0))

# # Remove days column
# housing_logret$day <- NULL

# # Weekly log-returns
# T <- NROW(housing_logret)
# housing_logret_week <- NULL

# for (i in 1:floor(T / 5)) {
#   housing_logret_week <- c(
#     housing_logret_week,
#     sum(housing_logret[(1 + 5 * (i - 1)):(5 * i), 1])
#   )
# }

# # Last week adjustment
# housing_logret_week <- c(
#   housing_logret_week,
#   sum(housing_logret[(floor(T / 5) * 5 + 1):T, 1])
# )

# length(housing_logret_week) # 975 complete weekly observations + 1

# rm(list = c("housing_data", "housing_logret", "housing_indices"))


# State Variables and corresponding plots --------------------------------------------------------

# Years for plots
stoxx600_week
x_label_years <- c(as.character(seq(2004, 2022, by = 2)))
x_at_years <- NULL
for (i in 1:length(x_label_years)) {
  x_at_years[i] <- min(which(floor(stoxx600_week / 10000) == as.numeric(x_label_years[i]))) # Index of the first day of the years in x_label_years
}

##### Change in the slope of the yield curve #####
tail(py_3m10y_aaa_week)
change_slope_yc_aaa_week <- as.data.frame(matrix(NA,
  nrow = length(stoxx600_week),
  ncol = 2
))
colnames(change_slope_yc_aaa_week) <- c("day", "change_slope_yc")
change_slope_yc_aaa_week$day <- stoxx600_week
change_slope_yc_aaa_week$change_slope_yc <- py_3m10y_aaa_week$py_10y_aaa - py_3m10y_aaa_week$py_3m_aaa
plot(change_slope_yc_aaa_week$change_slope_yc,
  type = "l",
  xlab = "",
  ylab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)


##### Short-term liquidity funding risk measure #####
tail(py_3m10y_aaa_week)
tail(euribor_3M_week)
stlfr_week <- as.data.frame(matrix(NA,
  nrow = length(stoxx600_week),
  ncol = 2
))
colnames(stlfr_week) <- c("day", "stlfr")
stlfr_week$day <- stoxx600_week
stlfr_week$stlfr <- euribor_3M_week$euribor_3M - py_3m10y_aaa_week$py_3m_aaa
plot(stlfr_week$stlfr,
  type = "l",
  xlab = "",
  ylab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)


##### Credit Spread #####
tail(py_3m10y_aaa_week)
tail(py_corp_10y_week)
cs_week <- as.data.frame(matrix(NA,
  nrow = length(stoxx600_week),
  ncol = 2
))
colnames(cs_week) <- c("day", "cs")
cs_week$day <- stoxx600_week
cs_week$cs <- py_corp_10y_week$py_10y_bbb - py_3m10y_aaa_week$py_10y_aaa
plot(cs_week$cs,
  type = "l",
  xlab = "",
  ylab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)



##### Already computed #####
# Market measure
plot(stoxx600_logret_week,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)

plot(stoxx50_logret_week,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)

plot(sp500_logret_week,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)

# Volatility measure
plot(volatility_week$vstoxx,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)

plot(volatility_week_diff_1_1$vstoxx_diff,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)


# Change in the three month yield by aaa issuers
plot(py_3m10y_aaa_week_diff_1_1$py_3m_aaa_diff_1_1,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)

# Housing returns
plot(housing_logret_week$housing_logret_week,
  type = "l",
  ylab = "",
  xlab = "",
  xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)
