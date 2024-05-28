# In-Class Exercises (InClass_R11.pdf)

#----------------------------------------------------------
# ECON 270
# R Exercises: Two-Stage Least Squares (InClass_R10.pdf)

# load data
x <- readRDS('~/Data/ColonialOrigins.rds')

# 0. Clean Data
x <- x[complete.cases(x[,c('logpgp95','avexpr','lat_abst','logem4')]),]

# 1. OLS
fit1 <- lm(logpgp95 ~ avexpr + lat_abst, data=x)

# 2. 2SLS: First Stage
fit2a <- lm(avexpr ~ logem4 + lat_abst, data=x)

# 3. 2SLS: Second Stage
temp <- predict(fit2a)
x[row.names(model.frame(fit2a)),'pred.x'] <- temp
fit2b <- lm(logpgp95 ~ pred.x + lat_abst, data=x)

# 4. 2SLS: Simultaneous Estimation
library(ivreg)
fit4 <- ivreg(logpgp95 ~ avexpr + lat_abst | logem4 + lat_abst, data=x)
