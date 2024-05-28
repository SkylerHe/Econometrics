# R Exercises: Introcution to R

# Part I
w <- 12
z <- 32
w*z
w^2 + z^2
(w^2 + z^2)/10
sqrt(w^2 + z^2)

x <- c(8,22,15,19,34)
mean(x)
var(x)
zscore <- (22-mean(x))/sd(x)
pnorm(zscore)

y <- x^2
fin <- x+y

# Part II
setwd('~/Data')
dat <- readRDS('CarbonFootprint.rds')
mean(dat$co2_emissions)
var(dat$co2_emissions)

pork <- dat[dat$food_category=='Pork',]
mean(pork$co2_emissions)
var(pork$co2_emissions)

cor(dat$consumption,dat$co2_emissions)
fit <- lm(co2_emissions ~ consumption, data=dat)

# Part III (separate script)