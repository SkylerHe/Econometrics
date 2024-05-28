#load the dataset
x <- readRDS('~/Data/SchoolingReturns.rds')
# 0. Clean Data
x <- x[complete.cases(x[,c('wage','education','experience','ethnicity','smsa','south')]),]

# 1. OLS
fit1 <- lm(wage ~ education + experience + ethnicity + smsa + south,data=x)
sum.fit1 <- summary(fit1)
dof <- sum.fit1$df[2]
#95% CI
ts <- qt(.025,dof)
ci_low1 <- sum.fit1$coefficients[2,1] + ts*sum.fit1$coefficients[2,2]
ci_high1 <- sum.fit1$coefficients[2,1] - ts*sum.fit1$coefficients[2,2]
ci.ols <- c(ci_low1,ci_high1)

# 2. 2SLS: First Stage
fit2a <- lm(education ~ nearcollege + experience + ethnicity + smsa + south,data=x)

# 3. 2SLS: Second Stage
temp <- predict(fit2a)
x[row.names(model.frame(fit2a)),'pred.x'] <- temp
fit2b <- lm(wage ~ pred.x + experience + ethnicity + smsa + south,data=x)
sum.fit2 <- summary(fit2b)
#95% CI
ci_low2 <- sum.fit2$coefficients[2,1] + ts*sum.fit2$coefficients[2,2]
ci_high2 <- sum.fit2$coefficients[2,1] - ts*sum.fit2$coefficients[2,2]
ci.iv <- c(ci_low2,ci_high2)
