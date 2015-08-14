install.packages("monomvn")
install.packages("lars")

# USEFUL:
# https://ariddell.org/horseshoe-prior-with-stan.html#fnref:4

library(monomvn)
library(lars)      # you might have to install that one too..

########### data
data(diabetes)  
yf = diabetes$y
Xf = diabetes$x2  # 64 variables from all 10 main effects,
                  # two-way interactions and quadradics
Xf = scale(Xf)    # rescale so that all variables have mean 0 and sd 1
n = length(yf)
n.test = 100

########### initialize for simulation
nsim= 25
mse.ols = rep(NA, nsim)
mse.lasso = rep(NA, nsim)
mse.bhs = rep(NA, nsim)
mse.bl = rep(NA, nsim)
for (i in 1:nsim) { # repeat simulation
  
  ########### split data into test & training data
  in.test = sample(1:n, n.test)  # random test cases
  in.train = (1:n)[-in.test]  # remaining training data
  y = yf[in.train]
  x = Xf[in.train,]
  ########### run LASSO
  db.lars = lars(x,y, type="lasso")
  Cp = summary(db.lars)$Cp
  best = (1:length(Cp))[Cp == min(Cp)]     # step with smallest Cp
  y.pred = predict(db.lars, s=best, newx=Xf[in.test,])
  mse.lasso[i] =  sum((y.pred$fit - yf[in.test])^2)/n.test

  ########### run OLS
  y.pred = predict(lm(y ~ x), Xf[in.test,]) # old predictions    
  mse.ols[i] = sum((y.pred - yf[in.test])^2)/n.test

  ########### run horseshoe
  bhs = blasso(x, y, case="hs", RJ=FALSE, normalize=F) #already normalized
  y.pred = mean(bhs$mu) + Xf[in.test,] %*% apply(bhs$beta, 2, mean)
  mse.bhs[i] = sum((y.pred - yf[in.test])^2)/n.test

  ########### run Bayesian LASSO
  bl = blasso(x, y, RJ=FALSE, normalize=F) #already normalized
  y.pred = mean(bl$mu) + Xf[in.test,] %*% apply(bl$beta, 2, mean)
  mse.bl[i] = sum((y.pred - yf[in.test])^2)/n.test
  
  print(c(i, mse.ols[i], mse.lasso[i], mse.bhs[i], mse.bl[i]))
}

########## boxplot of MSE's
boxplot(mse.ols, mse.lasso, mse.bhs, mse.bl)

########## plot estimated coefficients for the last run
ols = lm(y ~ x)
plot(coef(ols)[-1], apply(bhs$beta, 2, mean))
plot(coef(ols)[-1], db.lars$beta[best,])
plot(db.lars$beta[best,],apply(bhs$beta, 2, mean) )

abline(0,1)
pairs(data.frame(coef(ols)[-1],
                 db.lars$beta[best,],
                 apply(bhs$beta, 2, mean),
                 apply(bl$beta, 2, mean)
                 ))


