library(MASS)

# Linear Modeling - Christensen Ex. 2.10.4

# PART A ---------------------------------------------------------------------#
# Estimate b1, b2, and e.var

# Set up given data.
x.t.x <- matrix(c(15, 374.5, 374.5, 9482.75), 2, 2)
x.t.y <- matrix(c(6.03, 158.25))
id <- matrix(c(1,0,0,1), 2, 2)

b.hat <- ginv(x.t.x) %*% x.t.y
b1.hat <- b.hat[1]
b2.hat <- b.hat[2]

m <- ginv(x.t.x)
n <- 15
r <- 2
y.t.y <- 3.03
e.var.hat <- (y.t.y - t(x.t.y)%*%ginv(x.t.x)%*%x.t.y) / (n-r)

# PART B ---------------------------------------------------------------------#
# Give 98% confidence intervals for b2 and b2-b1.
#
# Note: If errors are uncorrelated and homoscedastic, beta has distribution
# b.hat ~ N(b, e.var*ginv(t(x)%*%x).
#
# To do a Normal confidence interval (CI), use: z = (obs - e(obs))/(std error).
# In general, CI = (+/-z) * (std error) + e(obs).

# Compute 98% CI for beta2.
# p. 32 has equation for distribution of lambda'beta.

# Set helper variables.
t.score.98 <- 2.65
mse <- e.var.hat
lambda1 <- matrix(c(1, 0), 2, 1)
lambda2 <- matrix(c(0, 1), 2, 1)
lambda3 <- matrix(c(-1, 1), 2, 1)

# Test for b2.
CI.side.b2 <- sqrt(mse*t(lambda2)%*%ginv(x.t.x)%*%lambda2)*t.score.98
CI.beta2 <- c(b2.hat-CI.side.b2, b2.hat+CI.side.b2)

# Compute 98% CI for beta2 - beta1. To do this, need std error for b2-b1.
# Remember that E[b2-b1] = E[b2] - E[b1], and var(b2-b1) = var(b2)+var(b1).
dif.hat <- b2.hat - b1.hat
CI.side.dif <- sqrt(mse*t(lambda3)%*%ginv(x.t.x)%*%lambda3)*t.score.98
CI.beta.dif <- c(dif.hat-CI.side.dif, dif.hat+CI.side.dif)

# PART C ---------------------------------------------------------------------#
# Perform an α = 0.05 test for H0 : β1 = 0.5.

# Find t-score associated with b1.hat, as compared to H0: b1=0.5.
t.score.95 <- 2.16
b1.t.score <- (b1.hat - 0.5) / sqrt(mse*t(lambda1)%*%ginv(x.t.x)%*%lambda1)
b1.t.score


