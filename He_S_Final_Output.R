#load the dataset
x <- readRDS('~/Data/SalesCities.rds')

#Q1: Estimate the regression
#log income
x$lnInc <- log(x$income)
fit1 <- lm(sales ~ lnInc + pop + under18 + repub + dist.coast + dist.coast.sq + factor(region), data=x)
sum.fit1 <- summary(fit1)
dof <- sum.fit1$df[2]

#Q2: Interpretation*
ans2.1 <- paste0('100% increases in mean income leads to 3569 increasing in sales')
ans2.2 <- paste0('As moving to inland, the sales starts to decrease. However, with the time goes by, the sales will increases as moving away from coast')
ans2 <- c(ans2.1,ans2.2)

#Q3: 95% CI
ts <- qt(.025,dof)
ci_low <- sum.fit1$coefficients[3,1] + ts*sum.fit1$coefficients[3,2]
ci_high <- sum.fit1$coefficients[3,1] - ts*sum.fit1$coefficients[3,2]
ci3 <- c(ci_low,ci_high)

#Q4: ß4 >= 7000 (test lower tail)
betaH <- 7000
t <- (sum.fit1$coefficients[5,1] - betaH) / sum.fit1$coefficients[5,2]
out.p1 <- (pt(t,dof,lower.tail = TRUE))

#Q5: ß7=ß8=ß9=ß10=0 (RLS)
ssr.ur <- sum(fit1$residuals^2)
fit2 <- lm(sales ~ lnInc + pop + under18 + repub + dist.coast + dist.coast.sq, data=x)
ssr.r1 <- sum(fit2$residuals^2)
m = 4
f1 <- ((ssr.r1-ssr.ur)/m) / (ssr.ur/fit1$df.residual)
out.p2 <- pf(f1,m,fit1$df.residual,lower.tail=F)

#Q6: Re-estimate the regression
ssr.ur2 <- sum(fit3$residuals^2)
fit3 <- lm(sales ~ lnInc + pop + under18 + repub + dist.coast + dist.coast.sq + factor(region) + repub*factor(region), data=x)
#ß11=ß12=ß13=ß14=0 (RLS)
ssr.r2 <- sum(fit1$residuals^2)
f2 <- ((ssr.r2-ssr.ur2)/m) / (ssr.ur2/fit3$df.residual)
out.p3 <- pf(f2,m,fit3$df.residual,lower.tail=F)

#Q7: Variance inflating factor log(income)
fit4 <- lm(lnInc ~ pop + under18 + repub + dist.coast + dist.coast.sq + factor(region), data=x)
r.fit4 <- summary(fit4)$r.squared
vif <- 1/(1-r.fit4)

#Q8: Test Heteroskedasticity by using Breusch-Pagan test 
u <- fit1$residuals
x$u2 <- u^2
fit.bp <- lm(u2 ~ lnInc + pop + under18 + repub + dist.coast + dist.coast.sq + factor(region), data=x)
sum.fitbp <- summary(fit.bp)
fstat.bp <- sum.fitbp$fstatistic
p.bp <- pf(fstat.bp[1], fstat.bp[2], fstat.bp[3], lower.tail=FALSE)
#Heteroskedasticity-robust standard errors
library(sandwich)
vHC <- vcovHC(fit1, type='HC0')
ci_low2 <- sum.fit1$coefficients[3,1] + ts*sqrt(vHC[3,3])
ci_high2 <- sum.fit1$coefficients[3,1] - ts*sqrt(vHC[3,3])
ci8 <- c(ci_low2,ci_high2)

#output for Q4,5,6,8
out.p <- c(out.p1,out.p2,out.p3,ci8)

#Q9: measurement error
ans9 <- paste0('Yes, it could. Because it has a small sample')
