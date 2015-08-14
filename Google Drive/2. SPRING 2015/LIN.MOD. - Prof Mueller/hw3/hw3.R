# Linear Models - Prof Mueller - HW3

library(Matrix)

RunModel <- function(data) {
  # Gets file and data, and runs linear model.
  #
  # Args:
  #   data: The data frame used for analysis.
  #
  # Returns:
  #   model: A linear model fit object.
  setwd("~/Google Drive/2. SPRING 2015/LIN.MOD. - Prof Mueller/hw3")
  file.contents <- read.table("hw3data.txt", header=T)
  data <- file.contents
  attach(data)
  model <- lm(deltaT ~ factor(proxy)*poly(latitude, 2), data=data, weights=1/sdev^2)
  m2 <- lm(deltaT ~ T.M*poly(latitude, 2), data=data, weights=1/sdev^2)
  
  X = model.matrix(deltaT ~ factor(proxy)*poly(latitude, 2), data=data)
  
  boxplot(deltaT ~ proxy)
  
  return (model)
}
  
PrepData <- function() {
  # Gets file and data, and prepares extra columns needed for analysis.
  #
  # Args:
  #   NA: None.
  #
  # Returns:
  #   data: A data frame ready for analysis.
  setwd("~/Google Drive/2. SPRING 2015/LIN.MOD. - Prof Mueller/hw3")
  file.contents <- read.table("hw3data.txt", header=T)
  data <- file.contents
  attach(data)
  
  # Add different intercept for each proxy requires a dummy variable per proxy.
  data$p1 <- ifelse(proxy==1, 1, 0)
  data$p2 <- ifelse(proxy==2, 1, 0)
  data$p3 <- ifelse(proxy==3, 1, 0)
  data$p4 <- ifelse(proxy==4, 1, 0)
  data$p5 <- ifelse(proxy==5, 1, 0)
  data$p6 <- ifelse(proxy==6, 1, 0)
  data$p7 <- ifelse(proxy==7, 1, 0)
  data$p8 <- ifelse(proxy==8, 1, 0)
  
  # Add latitude squared.
  data$latsq <- latitude*latitude
  
  # Add weight, where unnormalized weight = 1/var, and then normalize.
  data$var <- sdev*sdev
  data$inv.var <- 1/(data$var)
  sum.of.inv.vars <- sum(data$inv.var)
  data$weight <- data$inv.var/sum.of.inv.vars
  
  return (data)
}

data <- PrepData()
model <- RunModel(data)
