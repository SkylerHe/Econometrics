#load the data
x <- readRDS('~/Data/MinimumWage.rds')

aggregate(x$emptot,by=list(x$nj, x$post),FUN=mean)

#nj only, before vs. after
fitnj <- lm(emptot ~ chainr + coowned + post, data=x, subset=(nj==1))

#nj only, before vs. after
fitpa <- lm(emptot ~ chainr + coowned + post, data=x, subset=(nj==0))

#post only, nj vs. pa
fitpost <- lm(emptot ~ chainr + coowned + nj, data=x, subset=(post==1))

#diff in diff
