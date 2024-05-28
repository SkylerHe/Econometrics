#----------------------------------------------------------
# ECON 270
# R Exercises: Regression Inference (InClass_R02.pdf)

# load data
x <- readRDS('~/Data/Charity.rds')

# 1. Regress wage on IQ
fit  <- lm(Char.Donations ~ Income + Exp, data=x)
sum.fit <- summary(fit)
dof <- sum.fit$df[2]

# 2. H0: B1 = 0
betaH <- 0
alpha <- 0.01
ts2 <- (sum.fit$coefficients[2,1] - betaH) / sum.fit$coefficients[2,2]
cv2 <- qt((1-alpha/2),dof)
p2 <- 2*(1 - pt(ts2,dof))

# 3. H0: B1 <= 0
betaH <- 0
alpha <- 0.01
cv3 <- qt((1-alpha),dof)
p3 <- 1 - pt(ts2,dof)

# 4. 95% Confidence Interval for B1
alpha <- 0.05
moe <- qt(alpha/2,dof) * sum.fit$coefficients[2,2]
ci4 <- c(sum.fit$coefficients[2,1] + moe, sum.fit$coefficients[2,1] - moe)

# 5. 95% Confidence Interval for B1
alpha <- 0.1
moe <- qt(alpha/2,dof) * sum.fit$coefficients[2,2]
ci5 <- c(sum.fit$coefficients[2,1] + moe, sum.fit$coefficients[2,1] - moe)

# 6. 95% Confidence Interval for $1,500 increase in income
dy.ci <- 1.5*ci4

# 7. # 3. H0: B2 <= -1
betaH <- -1
alpha <- 0.01
ts7 <- (sum.fit$coefficients[3,1] - betaH) / sum.fit$coefficients[3,2]
cv7 <- qt((1-alpha),dof)
p7 <- 1 - pt(ts7,dof)