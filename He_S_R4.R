#load dataset
x <- read.csv('~/Data/LaborForce.csv')
fit1 <- lm(gdp.growth ~ lrate_male + lrate_female + urban.pop, data=x)

#1: ß1lrate_male = ß2lrate_female
sum.fit1 <- summary(fit1)
dif <- sum.fit1$coefficients[2,1]-sum.fit1$coefficients[3,1] 
vc <- vcov(fit1) #create variance covariance matrix
v.dif <- (vc[2,2])+(vc[3,3])-(2*vc[2,3])
se.dif <- sqrt(v.dif)
#calcualte t
ts.equality <- dif/se.dif

#2:ß1lrate_male + ß2lrate_female = 0.1(RLS)
#new x variable = -lrate_name+lrate_female
#new y variable = gdp.growth-0.1lrate_male
x$newvarx <- -x$lrate_male + x$lrate_female
x$newvary <- x$gdp.growth - 0.1*x$lrate_male
fit2 <- lm(newvary ~ newvarx + urban.pop, data=x)
ssr.r <- sum((fit2$residuals)^2)
ssr.ur <- sum((fit1$residuals)^2)
m <- 1
#test statistic
ts.sum <- ((ssr.r-ssr.ur)/m)/(ssr.ur/fit1$df.residual)