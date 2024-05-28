#----------------------------------------------------------
# ECON 270
# R Exercises: Multiple Linear Regression (InClass_R03.pdf)

# load data
x <- readRDS('~/Data/Wooldridge_Wages.rds')

# 1. Regress wage on IQ
fit1  <- lm(wage ~ IQ, data=x)

# 2. Regress wage on educ
fit2  <- lm(wage ~ educ, data=x)

# 3. Regress wage on IQ and educ
fit3 <- lm(wage ~ IQ + educ, data=x)
sum.fit3 <- summary(fit3)

# 4. Coefficients
b <- sum.fit3$coefficients[1:3,1]

# 5. Standard error of coefficient estimates
fullb <- sum.fit3$coefficients[,1:3]

# 6. R-squared
r2 <- sum.fit3$r.squared

# 7. Variance of residual (two ways: v1 or v2)
uhat <- fit3$residuals
n <- nobs(fit3)
k <- sum.fit3$df[1] - 1
v1 <- sum(uhat^2)/(n-k-1)

s <- sum.fit3$sigma
v2 <- s^2

# 8. 14 vs. 12 years of education 
dy8 <- b[3]*(14-12)

# 9. 14 years of education and 95 IQ vs. 12 years of education and 110 IQ
dy9 <- b[2]*(95-110) + b[3]*(14-12)

# 10. Correlation between residuals and independent variables
calc.cor <- c(cor(uhat,x$IQ), cor(uhat,x$educ))
