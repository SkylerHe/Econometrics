#----------------------------------------------------------
# ECON 270
# R Exercises: Multicollinearity and Heteroskedasticity (InClass_R07.pdf)

# load data
x <- readRDS('~/Data/SavingsRate.rds')

# 1. Estimate regression
fit <- lm(savings.rate ~ age + income, data=x)

# 2. VIF
fitx1 <- lm(age ~ income, data=x)
r2.x1 <- summary(fitx1)$r.squared
vif1 <- 1/(1-r2.x1)
fitx2 <- lm(income ~ age, data=x)
r2.x2 <- summary(fitx2)$r.squared
vif2 <- 1/(1-r2.x2)

# 3. plot residuals (x1,x2,yhat)
u <- fit$residuals
plot(x$age,u)
plot(x$income,u)
plot(predict(fit),u)

# 4. Breusch-Pagan test
x$u2 <- u^2
fit.bp <- lm(u2 ~ age + income, data=x)
fstat.bp <- summary(fit.bp)$fstatistic
p.bp <- pf(fstat.bp[1], fstat.bp[2], fstat.bp[3], lower.tail=FALSE)

# 5. White test (A)
fit.wa <- lm(u2 ~ age + income + I(age^2) + I(income^2) + age*income, data=x)
fstat.wa <- summary(fit.wa)$fstatistic
p.wa <- pf(fstat.wa[1], fstat.wa[2], fstat.wa[3], lower.tail=FALSE)

# 5. White test (B)
x$pred <- predict(fit)
fit.wb <- lm(u2 ~ pred + I(pred^2), data=x)
fstat.wb <- summary(fit.wb)$fstatistic
p.wb <- pf(fstat.wb[1], fstat.wb[2], fstat.wb[3], lower.tail=FALSE)

# 6. Heteroskedasticity-robust standard errors
library(sandwich)
vcovHC(fit, type='HC0')

# 7. GLS: var(u) -- w*income
x$rti <- sqrt(x$income)
x$newy <- x$savings/x$rti
x$newx1 <- 1/x$rti
x$newx2 <- x$age/x$rti
x$newx3 <- x$rti
fit.gls <- lm(newy ~ 0 + newx1 + newx2 + newx1, data=x)