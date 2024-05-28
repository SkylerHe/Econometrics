#----------------------------------------------------------
# ECON 270
# R Exercises: Simple Linear Regression (InClass_R01.pdf)

# load data
x <- readRDS('~/Data/CovidCounties.rds')

# 1. summary statistics
summary(x)

# 2. plot
plot(x$pcincome,x$cases)

# 3. regress cases on per capita income
fit <- lm(cases ~ pcincome, data=x)
sum.fit <- summary(fit)

# 4. interpret
b <- coefficients(fit)

# 5. correlation coefficient
cc <- cor(x$pcincome,x$cases)

# 6. regress per capita cases on per capita income
x$pccases <- x$cases/x$population
fit2 <- lm(pccases ~ pcincome, data=x)
sum.fit2 <- summary(fit2)

# 7. R-squared
r2 <- sum.fit2$r.squared

# 8. statistical significance
coef.table <- sum.fit2$coefficients

# 9. death rate
x$dr <- x$deaths/x$population
fit3 <- lm(dr ~ pcincome, data=x)

# 10. plot trend line
plot(x$pcincome,x$pccases)
abline(fit2,col=c('blue'),lwd=3)


