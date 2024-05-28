#load the dataset
x <-readRDS('SavingsRateB.rds')
fit <- lm(savings.rate ~ age + income, data = x)
#Baseline: hatßincome = 0.019520; hatß0 = 3.6368 (Good estimate)

e1 <- x$savings.net - x$savings.rate
#no correlation because e is a constant 
cor(e1,x$income)
#hatßincome is unbiased;  hatß0 is biased
# hatßincome = 0.019520 (unchanged); hatß0 = 1.6368 
#y has measurement error
fit1 <- lm(savings.net ~ age + income, data = x)

#2b in y; correlated with x 
e2 <- x$savings.round - x$savings.rate
cor(e2,x$income)
#mostly uncorrelated (close to 0)
#hatßincome = 0.020095 (tiny bit biased); hatß0 = 3.485579 (two good estimate for one thing)
fit2 <- lm(savings.round ~ age + income, data = x)

#2c in y; uncorrelated with x
e3 <- x$savings.deflate - x$savings.rate
#high correlation -> biased
cor(e3,x$income)
#hatßincome = -0.051136 (way off); hatß0 = 4.820935
fit3 <- lm(savings.deflate ~ age + income, data = x) 

#2d in x: the midpoint of 10k intervals
e4 <- x$income.mid - x$income
cor(e4,x$income)
#so close to 0, mostly uncorrelated
#hatßincome.mid = 0.018562; hatß0 = 3.689060
fit4 <- lm(savings.rate ~ age + income.mid, data = x) 

#2e in x: under-report income to avoid tax liabilities
e5 <- x$income.deflate - x$income
cor(e5,x$income)
#hatßincome.mid = 0.027014; hatß0 = 3.697214
fit5 <- lm(savings.rate ~ age + income.deflate, data = x) 
