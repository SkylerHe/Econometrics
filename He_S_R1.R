#load data TO x
x <- readRDS('WorldData.rds')

#create a new variable CO2_pc
CO2_pc <-(x$CO2/x$Pop)

#Calculate mean, median, and standard deviation for both CO2_pc and AgForFish.
cbar <- mean(CO2_pc,na.rm = T)
cmid <- median(CO2_pc,na.rm = T)
cstan <- sd(CO2_pc,na.rm = T)

abar <- mean(x$AgForFish,na.rm = T)
amid <- median(x$AgForFish,na.rm = T)
astan <- sd(x$AgForFish,na.rm = T)


#start with columns
col1 <- c(cbar,abar)
col2 <- c(cmid,amid)
col3 <- c(cstan,astan)
#create a single dataframe
coef <- data.frame(col1,col2,col3)
#rename columns and rows
colnames(coef) <- c('mean','median','sd')
rownames(coef) <- c('CO2_pc','AgForFish')
coef


