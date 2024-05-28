#----------------------------------------------------------
# ECON 270
# R Exercises: Measurement Error (InClass_R08.pdf)

# load data
x <- readRDS('~/Data/SavingsRateB.rds')

# 1. Estimate regression
fit <- lm(savings.rate ~ age + income, data=x)
out <- c(NA,fit$coefficients[3])
# 2. error correlation and linear model

# savings.net
e <- x$savings.net - x$savings.rate
cor(e,x$income)
f1 <- lm(savings.net ~ age + income, data=x)
out1 <- c(cor(e,x$income),f1$coefficients[3])

# savings.round
e <- x$savings.round - x$savings.rate
cor(e,x$income)
f2 <- lm(savings.round ~ age + income, data=x)
out2 <- c(cor(e,x$income),f2$coefficients[3])

# savings.deflate
e <- x$savings.deflate - x$savings.rate
cor(e,x$income)
f3 <- lm(savings.deflate ~ age + income, data=x)
out3 <- c(cor(e,x$income),f3$coefficients[3])

# income.mid
e <- x$income.mid - x$income
cor(e,x$income.mid)
f4 <- lm(savings.rate ~ age + income.mid, data=x)
out4 <- c(cor(e,x$income.mid),f4$coefficients[3])

# income.deflate
e <- x$income.deflate - x$income
cor(e,x$income)
f5 <- lm(savings.rate ~ age + income.deflate, data=x)
out5 <- c(cor(e,x$income.deflate),f5$coefficients[3])

models.out <- data.frame(out,out1,out2,out3,out4,out5)
models.out <- round(models.out,4); names(models.out) <- c('Baseline','y.shifted','y.rounded','y.deflated','x.rounded','x.deflated'); row.names(models.out) <- c('Cor.','Inc.Coef')
