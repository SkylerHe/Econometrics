#load data TO x
x <- readRDS('HouseholdIncome.rds')

#filter data to get households in metro = 2,3,4
z <- x[which(x$METRO==2 | x$METRO==3 | x$METRO==4),]

xbar <- mean(z$HHINCOME)
s2 <- var(z$HHINCOME)

out <- data.frame(xbar,s2)

colnames(out) <- c('Mean_Inc','Var_Inc')

