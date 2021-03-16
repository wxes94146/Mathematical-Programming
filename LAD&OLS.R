### n = 3, 6, 9, 12, Find the maximum n where Least Absolute Deviations(LAD) is advantageous over Ordinary Least Squares(OLS)

# Soft thresholding function
st.t <- function(aa, t){
 yy <- aa
 yy[aa > t] <- aa[aa > t] - t
 yy[aa < -t] <- aa[aa < -t] + t
 yy[(aa >= -t) & (aa <= t)] <- 0
 return(yy)
}

rho <- 1

### Exercise: Test using data with outliers
N <- 50
p <- 5
n <- 3 # Number of outliers
p1 <- p # Number of real explatiners
s1 <- 1 # Error std

# Data generating model: y = b0 + b1 * x1 + b2 * x2 + e
Err.OLS <- Err.LAD <- NULL
for (i1 in 1:10){
 set.seed(i1 + 3149) # The last four digits of the student ID(3149)
 X1 <- matrix(runif(N*p, 0, 1), N, p) # Input data matrix
 b0 <- 1 # Intercept
 bb <- runif(p, -1, 1)
 e1 <- rnorm(N, 0, s1)
 y1 <- b0 + X1 %*% bb + e1 # Response vector
 #
 y1.out <- rep(0, N)
 y1.out[1:n] <- runif(n, 10, 20)
 y1 <- y1 + y1.out # Outliers

 # Column centering the data
 A <- scale(X1[1:N/2, ], center = T, scale = F)
 b <- scale(y1[1:N/2, ], center = T, scale = F)

 # Huber regression
 rho <- 1
 x1 <- matrix(rep(0, ncol(A)), ncol = 1)
 z1 <- matrix(rep(0, nrow(A)), ncol = 1)
 u1 <- matrix(rep(0, nrow(A)), ncol = 1)

 for (i1 in 1: 1000){
  x1 <- solve(t(A) %*% A) %*% t(A) %*% (b + (z1 - u1))
  z1 <- st.t(A %*% x1 - b + u1, 1 / rho)
 # z1 <- rho/(rho+1) * (A %*% x1 - b + u1) + 1 / (1+rho) * st.t(A %*% x1 - b + u1, 1 + 1/rho)
  r1 <- A %*% x1 - z1 - b
  u1 <- u1 + (A %*% x1 - z1 - b)
  if (sum(abs(r1)) < 10^-6) break
 }
 x.lad <- c(x1)
 sum(abs(r1))
 b0.lad <- mean(y1[1:N/2] - X1[1:N/2, ] %*% x.lad)

 # OLS
 x.ols <- c(solve(t(A) %*% A) %*% t(A) %*% b)
 b0.ols <- mean(y1[1:N/2] - X1[1:N/2, ] %*% x.ols)

 # Test
 y1.lad <- b0.lad + X1[-(1:N/2), ] %*% x.lad
 Err.LAD <- c(Err.LAD, sum((y1[-(1:N/2)] - y1.lad)^2))

 y1.ols <- b0.ols + X1[-(1:N/2), ] %*% x.ols
 Err.OLS <- c(Err.OLS, sum((y1[-(1:N/2)] - y1.ols)^2))
}
mean(Err.LAD)
mean(Err.OLS)