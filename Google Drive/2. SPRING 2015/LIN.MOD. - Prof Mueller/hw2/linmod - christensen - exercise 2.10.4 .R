library(MASS)

# Linear Modeling - Christensen Ex. 2.10.4

# PART A ---------------------------------------------------------------------#
# Estimate b1, b2, and e.var

# Set up given data.
x <- matrix(c(15, 374.5, 374.5, 9482.75), 2, 2)
x1 <- matrix(x[,1], 2, 1)
x2 <- matrix(x[,2], 2, 1)
y <- matrix(c(6.03, 158.25))
id <- matrix(c(1,0,0,1), 2, 2)

# Compute perpendicular projection matrix onto x1.
m1 <- x1 %*% ginv(t(x1)%*%x1) %*% t(x1)

# Compute estimates for betas using similar form from regular beta estimators.
b2.hat <- solve(t(x2)%*%(id-m1)%*%x2) %*% t(x2)%*%(id-m1)%*%y
b1.hat <- solve(t(x1)%*%x1) %*% t(x1) %*% (y-x2%*%b2.hat)

# Check if betas are reasonable estimates, by seeing if x%*%b == y
b <- matrix(c(b1.hat, b2.hat), 2, 1)
x%*%b == y

# Compute variance of error term using Theorem 2.2.6.
n <- 15
r <- 2
m <- x%*%ginv(t(x)%*%x)%*%t(x)
e.var.hat <- (t(y)%*%(id-m)%*%y) / (n-r)

# PART B ---------------------------------------------------------------------#
# Give 98% confidence intervals for b2 and b2-b1.
#
# Note: If errors are uncorrelated and homoscedastic, beta has distribution
# b.hat ~ N(b, e.var*solve(t(x)%*%x), and the partitioned form (with b1, b2)
# borrows from the definitions in Part A.
#
# To do a Normal confidence interval (CI), use: z = (obs - e(obs))/(std error).
# In general, CI = (+/-z) * (std error) + e(obs).

# Compute 98% CI for beta2.
b2.var.hat <- e.var.hat * ginv(t(x2)%*%(id-m1)%*%x2)
b2.se.hat <- sqrt(b2.var.hat)
twotail.z.score <- 2.33
low.CI <- -twotail.z.score * b2.se.hat + b2.hat
high.CI <- twotail.z.score * b2.se.hat + b2.hat
b2.CI <- c(low.CI, high.CI)

# Compute 98% CI for beta2 - beta1. To do this, need std error for b2-b1.
# Remember that E[b2-b1] = E[b2] - E[b1], and var(b2-b1) = var(b2)+var(b1).
b1.var.hat <- e.var.hat * ginv(t(x1)%*%x1)
dif.hat <- b2.hat - b1.hat
dif.var.hat <- b2.var.hat + b1.var.hat
dif.se.hat <- sqrt(dif.var.hat)
low.CI <- -twotail.z.score * dif.se.hat + dif.hat
high.CI <- twotail.z.score * dif.se.hat + dif.hat
dif.CI <- c(low.CI, high.CI)

b2.hat
b2.CI

dif.hat
dif.CI

# PART C ---------------------------------------------------------------------#
# Perform an α = 0.05 test for H0 : β1 = 0.5.

# Find z-score associated with b1.hat, as compared to H0: b1=0.5.
b1.se.hat <- sqrt(b1.var.hat)
b1.z.score <- (b1.hat - 0.5) / b1.se.hat
b2.z.score <- (b2.hat - 0.5) / b2.se.hat


