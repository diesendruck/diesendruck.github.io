# Linear Models Midterm

library(MASS)

# QUESTION 4A

x <- matrix(c(10,9,9,11,11,10,10,12, 15,14,13,15,14,14,16,13), 8, 2)
y <- matrix(c(82,79,74,83,80,81,84,81), 8, 1)
bhat <- ginv(t(x)%*%x)%*%t(x)%*%y
yhat <- x%*%bhat

n <- 8
r <- 2
mse <- ( t(y)%*%y - (t(y)%*% x %*% ginv(t(x)%*%x) %*% t(x) %*% y) ) / (n-r)

# QUESTION 4B
L1 <- matrix(c(1,0), 2, 1)
L2 <- matrix(c(1,1), 2, 1)
t.score <- qt(p=0.975, df=n-r)
b1.conf <- c(t(L1)%*%bhat - sqrt(mse%*%t(L1)%*%ginv(t(x)%*%x)%*%L1) * t.score,
             t(L1)%*%bhat + sqrt(mse%*%t(L1)%*%ginv(t(x)%*%x)%*%L1) * t.score)
sum.conf <- c(t(L2)%*%bhat - sqrt(mse%*%t(L2)%*%ginv(t(x)%*%x)%*%L2) * t.score,
              t(L2)%*%bhat + sqrt(mse%*%t(L2)%*%ginv(t(x)%*%x)%*%L2) * t.score)

# QUESTION 4C
L3 <- matrix(c(0,1), 2, 1)
null <- 3
test.t.score3 <- (t(L3)%*%bhat - null) / sqrt(mse%*%t(L3)%*%ginv(t(x)%*%x)%*%L3)
critical.tscore <- qt(p=0.995, df=n-r)

# QUESTION 4D
L4 <- matrix(c(1,-1), 2, 1)
null <- 0
test.t.score4 <- (t(L4)%*%bhat - null) / sqrt(mse%*%t(L4)%*%ginv(t(x)%*%x)%*%L4)
