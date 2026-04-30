library(latex2exp)
check_loss = function(u, alpha) {
    if (u >= 0) {
        y = u * alpha
    } else {
        y = u * (alpha - 1)
    }
    return(y)
}

# u values
u = seq(-5, 5, length = 1000)

# 1. \alpha = 0.25
alpha = 0.25
y_0.25 = sapply(u, function(u) check_loss(u, alpha = alpha))
out = cbind(u, y_0.25)
write.table(out,
    file = "main/chapter_1/pictures_chapter_1/check_0_25.dat",
    col.names = FALSE,
    row.names = FALSE
)

# 2. \alpha = 0.50
alpha = 0.5
y_0.5 = sapply(u, function(u) check_loss(u, alpha = alpha))
out = cbind(u, y_0.5)
write.table(out,
    file = "main/chapter_1/pictures_chapter_1/check_0_50.dat",
    col.names = FALSE,
    row.names = FALSE
)

# 3. \alpha = 0.75
alpha = 0.75
y_0.75 = sapply(u, function(u) check_loss(u, alpha = alpha))
out = cbind(u, y_0.75)
write.table(out,
    file = "main/chapter_1/pictures_chapter_1/check_0_75.dat",
    col.names = FALSE,
    row.names = FALSE
)