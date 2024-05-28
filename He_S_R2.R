#use the dataset wooldridge_smoking.rds
x <- readRDS('~/Data/Wooldridge_Smoking.rds')

#multiple linear regression
fit <- lm(cigs ~ income + cigpric + age, data=x)
sum.fit <- summary(fit)

#the lower-bound of the confidence interval
# df = 803
ts <- qt(.025,803)
ci_low <- sum.fit$coefficients[2,1] + ts*sum.fit$coefficients[2,2]

#the test statistic for the hypothesis test
t <- sum.fit$coefficients[2,1]/sum.fit$coefficients[2,2]

#the p-value for the hypothesis test 
pv <- (pt(t,803,lower.tail = FALSE))*2

#the predicted increase in cigarettes smoked per week if income increases by $3000,predicted using the upper-bound of the confidence interval
ci_high <- sum.fit$coefficients[2,1] - ts*sum.fit$coefficients[2,2]
pred_high = 3000*ci_high