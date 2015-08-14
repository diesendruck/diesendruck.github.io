# LinMod M2: 3C

library(stats)
library(knitr)

# Create grid of Betas.
betas <- seq(-3, 3, length=1000)

# Define function to integrate. Integrate function for each Beta_i.
INTEGRAND <- function(beta.i) {
  integrand <- function(x) {
    (1/(x+x^3))*(exp((-1/2)*beta.i^2/x^2))
  }
  return (integrand)
}

# Do integration on Lambda from 0 to Inf, and plot results.
len <- length(betas)
results <- matrix(0, nrow=len, ncol=1)
for (i in 1:len) {
  results[i,] <- integrate(INTEGRAND(betas[i]), lower=0, upper=Inf)$value
}

plot(betas,results, xlab="Betas",
     main=expression(paste("Numerical Evaluation of P(", beta[i], "|",
                           tau, ",", sigma^2, ")")),
     ylab=expression(paste("P(", beta[i], "|", tau, ",", sigma^2, ")")))
