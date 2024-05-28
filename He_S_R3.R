#load the dataset
x <- readRDS('HousingIncinerator.rds')

#set the model
x$dist.close <- ifelse(x$dist<= 10000, 'yes','no')
x$dist.med <- ifelse(x$dist> 10000 & x$dist <= 20000, 'yes','no')
fit <- lm(price ~ age + area + dist.close + dist.med + area*dist.close + area*dist.med, data = x)
sum.fit <- summary(fit)

#create dif variable
dif <- 5000 * fit$coefficients["area:dist.closeyes"] - 1300 * fit$coefficients["area:dist.closeyes"]

#create pv.ac
#df = 314
t <- sum.fit$coefficients[6,1]/sum.fit$coefficients[6,2]
#the p-value for the hypothesis test 
pv.ac <- (pt(t,314,lower.tail = FALSE))*2
