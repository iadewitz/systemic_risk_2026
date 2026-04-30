# Power law distribution
minX <- 1e-1
maxX <- 10
n <- 1000
x <- seq(minX, maxX, length = n)

densityPower <- function(x, alpha) {
    return(x^(-alpha))
}

alpha <- 2
dValues <- densityPower(
    x = x,
    alpha = alpha
)

cNorm <- integrate(
    function(x) densityPower(x, alpha),
    lower = minX,
    upper = maxX
)
plot(x, dValues / cNorm$value, type = "l")

# Cumulative distribution function
cdfPower <- function(x, alpha) {
    out <- integrate(
        function(t) densityPower(t, alpha) / cNorm$value,
        lower = minX,
        upper = x
    )

    return(out$value)
}
cdfPower(6, 2)
cdfValues <- sapply(
    x,
    function(t) cdfPower(x = t, alpha = alpha)
)
plot(x, cdfValues, type = "l")

# Complementary cumulative distribution function
ccdfPower <- function(x, alpha){
    out <- 1 - cdfPower(x, alpha)
    return(out)
}

ccdfValues <- sapply(
    x,
    function(t) ccdfPower(x = t, alpha = alpha)
)
plot(x, ccdfValues, type = "l")