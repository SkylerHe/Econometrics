#load the dataset
x1 <- read.csv('~/Data/PUMA_demographic.csv')
x2 <- read.csv('~/Data/PUMA_economic.csv')

#load dplyr package
library(dplyr)
#Merging using uniqiue ID variabels: left_join() from dplyr
z <- left_join(x1,x2, by=c('STATEFIP','PUMA','YEAR'))
rm(x1,x2)

#create pop and white variables: mutate() from dplyr
z <- mutate(z, Pop = pop.white + pop.black + pop.ai + pop.asian + pop.othrace + pop.tworace)
z <- mutate(z, White = ifelse(pop.white/Pop >=.75,1,0))

#Estimate regression
fit1 <- lm(value.mean.real ~ inc.mean.real + Pop + White, data = z)

#Estimate using only data from 2008: filter () from dplyr
w <- filter(z, YEAR == 2008)
fit2 <- lm(value.mean.real ~ inc.mean.real + Pop + White, data = w)

#For loop
#Define main set of data 
yrs <- unique(z$YEAR)
#Define lenght of loop
N <- length(yrs)
#Define a placeholder for our output
out.I <- data.frame('coef.inc' = rep(0,N),'std.err.inc' = rep(0,N),'sample_size'= rep(0,N))
out.W <- data.frame('coef.white' = rep(0,N),'std.err.white' = rep(0,N),'sample_size'= rep(0,N))

#Start loop
for(i in 1:N){
  dffiltered <- filter(z, YEAR == yrs[i])
  fit3 <- lm(value.mean.real ~ inc.mean.real + Pop + White, data = dffiltered)
  #find coef and std on coefficient table for each year
  sum.fit3 <- summary(fit3)
  #locate coef, std, and sample size
  coef1 <- sum.fit3$coefficients[2,1]
  std1 <- sum.fit3$coefficients[2,2]
  size1 <- fit3$df.residual
  coef2 <- sum.fit3$coefficients[4,1]
  std2 <- sum.fit3$coefficients[4,2]
  size2 <- fit3$df.residual
  #create output
  out.I[i,] <- c(coef1,std1,size1)
  out.W[i,] <- c(coef2,std2,size2)
}

#Summary
coef.I <- out.I$coef.inc
coef.W <- out.W$coef.white
betaI.sum = c(min(coef.I),mean(coef.I),median(coef.I),max(coef.I))
betaW.sum = c(min(coef.W),mean(coef.W),median(coef.W),max(coef.W))









