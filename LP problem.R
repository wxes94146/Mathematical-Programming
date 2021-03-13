# Max z = 3 * x1 + 5 * x2
# ST 1 * x1 + 0 * x2 <= 4
#    0 * x1 + 2 * x2 <= 12
#    3 * x1 + 2 * x2 <= 18
#    x1, x2 >= 0

install.packages("lpSolve")
library(lpSolve)
objective_coefficient <- c(3, 5)
ST_matrix <- matrix(c(1, 0, 0, 2, 3, 2), nrow = 3, byrow = TRUE)
ST_1 <- 4
ST_2 <- 12
ST_3 <- 18
ST_rhs <- c(ST_1, ST_2, ST_3)
ST_dir <- c("<=", "<=", "<=")
optimal <- lp(direction = "max", objective_coefficient, ST_matrix, ST_dir, ST_rhs)
optimal
optimal$solution


plot(c(0,10), c(0,10), type = "n", xlab = "x1", ylab = "x2", asp = 1)
abline(v = 4, col = "red")
abline(h = 6, col = "green")
abline(a = 9, b = -3/2, col="blue")
abline(a = 1, b = -3/5, col="purple") # a = 36/5