#----------------------------------------------------------
# ECON 270
# R Exercises: Difference-in-Difference (InClass_R08.pdf)

# load data
x <- readRDS('~/Data/MinimumWage.rds')

# 1. Table of Means
aggregate(x$emptot, by=list('NJ'=x$nj,'Post'=x$post), FUN=mean)

# 2. NJ data only: compare pre and post
fit2 <- lm(emptot ~ chainr + coowned + post, data=x, subset=(nj==1))

# 3. Post data only: compare NJ to PA
fit3 <- lm(emptot ~ chainr + coowned + nj, data=x, subset=(post==1))

# 4. PA data only: compare pre and post
fit4 <- lm(emptot ~ chainr + coowned + post, data=x, subset=(nj==0))
dif <- fit2$coefficients[4] - fit4$coefficients[4]

# 5. Dif-in-Dif
fit5 <- lm(emptot ~ chainr + coowned + nj + post + nj:post, data=x)

#-------------------------------------------------------
# Note: Analysis without including other X covariates
fitn <- lm(emptot ~ post, data=x, subset=(nj==1))
fitp <- lm(emptot ~ post, data=x, subset=(nj==0))
dif.coefs <- fitn$coefficients[2] - fitp$coefficients[2]
fit.reg <- lm(emptot ~ post + nj + post:nj, data=x)
dif.regression <- fit.reg$coefficients[4]