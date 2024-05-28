
#------------------------------------------------------------------------------#
# This file outlines how to use a for-loop to estimate a linear regression on
# different subsets of a dataset. The data used for this analysis is a set of
# randomly generated numbers, with no contextual meaning. Lines 9-15 create the
# dataset. You need to run those lines of code, but you can then consider the x
# dataframe as a dataset.
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Simulate Data
N <- 10000
set.seed(82084)
v1 <- rnorm(N, 150, 30)
v2 <- sample(c('Red','Orange','Yellow','Green','Indigo','Violet'), N, replace=T, prob=c(.09,.17,.31,.22,.13,.08))
v3 <- 220 + 2.6*v1 + rnorm(N,0,80)

x <- data.frame(y=v3,x=v1,category=v2)
rm(v1,v2,v3,N)
#------------------------------------------------------------------------------#

# This analysis will estimate a regression of y on x separately for each color.

# load library
library(dplyr)

# Define size of loop
gps <- unique(x$category)
k <- length(gps)

# Create dataframe for coefficient and sample size
out <- data.frame('coef'=rep(0,k),'n'=rep(0,k))

# Begin loop
for(i in 1:k){
  
  # get data
  temp.data <- filter(x,category==gps[i])
  
  # estimate regression
  fit  <- lm(y ~ x, data=temp.data)
  
  # get output
  sum.fit <- summary(fit)
  beta <- sum.fit$coefficients[2,1]
  sz <- nobs(fit)
  
  # store output
  out[i,] <- c(beta,sz)
}

# Summarize coefficients
b <- out$coef
beta.sum <- c(min(b),mean(b),median(b),max(b))
names(beta.sum) <- c('min','mean','median','max')
