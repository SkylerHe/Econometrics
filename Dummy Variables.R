# In-Class Exercises (InClass_R05.pdf)

# load data
x <- readRDS('~/Data/HousingPrices01.rds')

# 1. Regress Value on Rooms and age
x$age <- 2023 - x$yearbuilt
fit1 <- lm(Value ~ Rooms + age, data=x)

# 2. Include dummy variables for metropolitan area
fit2 <- lm(Value ~ Rooms + age + factor(MET2013), data=x)

# 3. Hypothesis test for difference between Richmond and Boston
city <- 'factor(MET2013)Richmond'
row.get <- which(names(fit2$coefficients)==city)
ts <- summary(fit2)$coefficients[row.get,3]
pv <- summary(fit2)$coefficients[row.get,4]

# 4. Re-define dummy reference level as Richmond
x$MET2013 <- relevel(x$MET2013, ref='Richmond')
fit3 <- lm(Value ~ Rooms + age + factor(MET2013), data=x)

# 5. Include dummy variables interactions with Rooms
fit4 <- lm(Value ~ Rooms + age + factor(MET2013) + Rooms*factor(MET2013), data=x)

# 6. Hypothesis test for difference in price effect of rooms (Boston v. Richmond; Providence v. Richmond)
city <- 'Rooms:factor(MET2013)Boston'
row.get <- which(names(fit4$coefficients)==city)
ts.boston <- summary(fit4)$coefficients[row.get,3]
pv.boston <- summary(fit4)$coefficients[row.get,4]

city <- 'Rooms:factor(MET2013)Providence'
row.get <- which(names(fit4$coefficients)==city)
ts.prov <- summary(fit4)$coefficients[row.get,3]
pv.prov <- summary(fit4)$coefficients[row.get,4]
