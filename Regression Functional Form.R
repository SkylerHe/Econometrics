#----------------------------------------------------------
# ECON 270
# R Exercises: Regression Functional Form (InClass_R04.pdf)

# load data
x <- readRDS('~/Data/Charity.rds')

# 1. Regress Donations on Income and Expenditures
fit  <- lm(Char.Donations ~ Income + Exp, data=x)
sum.fit <- summary(fit)
dof <- sum.fit$df[2]

# 2. Various Function Forms

# a. log Income
x$lnInc <- log(x$Income)
fit2a <- lm(Char.Donations ~ lnInc + Exp, data=x)

# b. Income quadratic
x$Inc2 <- x$Income^2
fit2b <- lm(Char.Donations ~ Income + Inc2 + Exp, data=x)

# c. inverse Income
x$invInc <- 1/(x$Income)
fit2c <- lm(Char.Donations ~ invInc + Exp, data=x)

# d. square root of Income
x$sqrtInc <- sqrt(x$Income)
fit2d <- lm(Char.Donations ~ sqrtInc + Exp, data=x)

# e. dummy for political party
fit2e <- lm(Char.Donations ~ Income + Exp + factor(Pol.Party), data=x)

# f. interact income with political party
fit2f <- lm(Char.Donations ~ Income + Exp + Income*factor(Pol.Party), data=x)

# g. log Charitable Donations
x$lnChar <- log(x$Char.Donations)
fit2g <- lm(lnChar ~ Income + Exp, data=x)

# h. log Charitable Donations and log Income
fit2h <- lm(lnChar ~ lnInc + Exp, data=x)

# 3. H0: B2 = 0
test.stat3 <- summary(fit2b)$coefficients[3,1]/summary(fit2b)$coefficients[3,2]
pvalue3 <- 2*pt(abs(test.stat3), fit2b$df.residual, lower.tail=FALSE)

# 4. R-squared

# 5. H0: Bint =0
test.stat5 <- summary(fit2f)$coefficients[5,1]/summary(fit2f)$coefficients[5,2]
pvalue5 <- 2*pt(abs(test.stat5), fit2b$df.residual, lower.tail=FALSE)

