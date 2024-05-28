# In-Class Exercises (InClass_R06.pdf)

#----------------------------------------------------------
# ECON 270
# R Exercises: Hypothesis Testing on Multiple Parameters (InClass_R06.pdf)

# load data
x <- readRDS('~/Data/ConcreteStrength.rds')

# 1. Estimate regression
fit <- lm(Strength ~ Cement + Ash + Water + Slag + Superplasticizer, data=x)

# 2. H0: B_ash = B_slag
diff <- summary(fit)$coefficients[3,1] - summary(fit)$coefficients[5,1]
mat <- vcov(fit)
se <- sqrt(mat[3,3] + mat[5,5] - 2*mat[3,5])
t <- diff/se
pval2 <- 2*pt(abs(t),fit$df.residual,lower.tail=F)
  
# 3. H0: B_super = 5*B_cement
ssr.ur <- sum(fit$residuals^2)
x$newvar <- 5*x$Superplasticizer + x$Cement
fit3 <- lm(Strength ~ newvar + Ash + Water + Slag, data=x)
ssr.r <- sum(fit3$residuals^2)
f3 <- ((ssr.r-ssr.ur)/1) / (ssr.ur/fit$df.residual)
pval3 <- pf(f3,1,fit$df.residual,lower.tail=F)

# 4. H0: B_cement = B_ash = B_slag
x$newvar <- x$Cement + x$Ash + x$Slag
fit4 <- lm(Strength ~ newvar + Water + Superplasticizer, data=x)
ssr.r <- sum(fit4$residuals^2)
f4 <- ((ssr.r-ssr.ur)/2) / (ssr.ur/fit$df.residual)
pval4 <- pf(f4,1,fit$df.residual,lower.tail=F)

# 5. Chow Test (We did not cover this)
fit5a <- lm(Strength ~ Cement + Ash + Water + Slag + Superplasticizer, data=x, subset=(Temperature<=72))
fit5b <- lm(Strength ~ Cement + Ash + Water + Slag + Superplasticizer, data=x, subset=(Temperature>72))  
ssr.r5 <- ssr.ur
ssr.ur5 <- sum(fit5a$residuals^2) + sum(fit5b$residuals^2)
f5 <- ((ssr.r5-ssr.ur5)/6) / (ssr.ur5/(fit5a$df.residual + fit5b$df.residual))
pval5 <- pf(f5,6,(fit5a$df.residual + fit5b$df.residual),lower.tail=F)

# 6. Interact water with temperature: H0: B_inter = 0
fit6 <- lm(Strength ~ Cement + Ash + Water + Water:Temperature + Slag + Superplasticizer, data=x)
pval6 <- summary(fit6)$coefficients[7,4]

# 7. Chow Test
fit7a <- lm(Strength ~ Cement + Ash + Water + Slag + Superplasticizer, data=x, subset=(Temperature<=72 & Temperature>=55))
fit7b <- lm(Strength ~ Cement + Ash + Water + Slag + Superplasticizer, data=x, subset=(Temperature>72 | Temperature<55))  
ssr.r7 <- ssr.ur
ssr.ur7 <- sum(fit7a$residuals^2) + sum(fit7b$residuals^2)
f7 <- ((ssr.r7-ssr.ur7)/6) / (ssr.ur7/(fit7a$df.residual + fit7b$df.residual))
pval7 <- pf(f7,6,(fit7a$df.residual + fit7b$df.residual),lower.tail=F)
