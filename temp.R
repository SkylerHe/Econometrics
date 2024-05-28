#------------------------------------------------------------------------------#
# ECON 270                                                                     #
# R Assignments (Solutions)                                                    #
#------------------------------------------------------------------------------#

#------------------------------------------------------------#
#       Assignment 1                                         #

# load data
x <- readRDS('~/Data/WorldData.rds')

# create per capita variable
x$CO2_pc <- x$CO2/x$Pop

# get summary stats 
m1 <- mean(x$CO2_pc,na.rm=T)
m2 <- mean(x$AgForFish,na.rm=T)
md1 <- median(x$CO2_pc,na.rm=T)
md2 <- median(x$AgForFish,na.rm=T)
s1 <- sd(x$CO2_pc,na.rm=T)
s2 <- sd(x$AgForFish,na.rm=T)

# create dataframe
coef <- data.frame('mean'=c(m1,m2),'median'=c(md1,md2),'sd'=c(s1,s2))
rownames(coef) <- c('CO2_pc','AgForFish')


#------------------------------------------------------------#
#       Assignment 2                                         #

# load data
x <- readRDS('~/Data/Wooldridge_Smoking.rds')

# create incomek variable
x$incomek <- x$income/1000

# fit regression
fit <- lm(cigs ~ incomek + cigpric + age, data=x)
sum.fit <- summary(fit)

# 95% ci
ci_low <- sum.fit$coefficients[2,1] - sum.fit$coefficients[2,2]*qt(0.975,fit$df.residual)

# hypothesis test
ts <- sum.fit$coefficients[2,3]
pv <- sum.fit$coefficients[2,4]

# predict change
ci_h <- sum.fit$coefficients[2,1] + sum.fit$coefficients[2,2]*qt(.975,fit$df.residual)
pred_high <- 3*ci_h  

c(ci_low,ts,pv,pred_high)


#------------------------------------------------------------#
#       Assignment 3                                         #

# load data
x <- readRDS('~/Data/HousingIncinerator.rds')

# create distance dummy variable
x$catdist <- 'dist.med'
x$catdist <- ifelse(x$dist <= 10000, 'dist.close', x$catdist)
x$catdist <- ifelse(x$dist > 20000, 'dist.far', x$catdist)

# fit regression
x$catdist <- relevel(factor(x$catdist), ref='dist.far')
fit <- lm(price ~ age + area + factor(catdist) + factor(catdist):area, data=x)
sum.fit <- summary(fit)

# predict difference
dif <- (sum.fit$coefficients[5,1] - sum.fit$coefficients[4,1]) +
  (sum.fit$coefficients[7,1] - sum.fit$coefficients[6,1])*2000

# p-value on interaction
pv.ac <- sum.fit$coefficients[6,4]

#------------------------------------------------------------#
#       Assignment 4                                         #

# load data
x <- read.csv('~/Data/LaborForce.csv')

# fit regression
fit <- lm(gdp.growth ~ lrate_male + lrate_female + urban.pop, data=x)

# hypothesis test: b1=b2
b1 <- fit$coefficients[2]
b2 <- fit$coefficients[3]
v <- vcov(fit)
se <- sqrt(v[2,2] + v[3,3] - 2*v[2,3])
ts.equality <- (b1-b2)/se

# restricted least squares: b1 + b2 = 0.1
x$new.y <- x$gdp.growth - 0.1*x$lrate_male
x$new.x <- x$lrate_female - x$lrate_male
fit.r <- lm(new.y ~ new.x + urban.pop, data=x)

ssr.ur <- sum((fit$residuals)^2)
ssr.r <- sum((fit.r$residuals)^2)
m <- 1
df <- fit$df.residual
ts.sum <- ((ssr.r-ssr.ur)/m) / (ssr.ur/df)
