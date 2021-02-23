setwd("G:/My Drive/ST 541/R files/Data")
# setwd("C:/Users/qwang57/Google Drive/ST 541/R files/Data")
nyc <- read.csv("nyc.csv", header=TRUE)
attach(nyc)

dim(nyc)
head(nyc)

summary(nyc[,3:6])
boxplot(nyc[,3:6])
table(nyc[7])

tapply(Price, East, summary)

## Multiple linear regression
## model interpretation

#Regression output on pages 138 & 139
m1 <- lm(Price~Food+Decor+Service+East)
summary(m1)

## Is Service really not important?
m11 <- lm(Price~Service)
summary(m11)            # Service is HIGHLY significant!!! What's going on?

cor(nyc[,3:7])          # Service is highly correlated with two other predictors, "Food" and "Decor".
pairs(nyc[,3:6])


#Regression output on page 139
# Drop the insignificant predictor "Service"
m2 <- lm(Price~Food+Decor+East)
summary(m2)


#An alterntive way to obtain m2 is to use the update command
m2 <- update(m1,~.-Service)
summary(m2)


## Is Service really not important?
m11 <- lm(Price~Service)
summary(m11)            # Service is HIGHLY significant!!! What's going on?



# Type I and Type III SS
anova(m2)
anova(lm(Price~Food+Decor+East))
anova(lm(Price~East+Food+Decor))
anova(lm(Price~Decor+Food+East))

#What's going on here? Should the order of predictors matter?
library(car)
Anova(lm(Price~Food+Decor+East), type='III')
Anova(lm(Price~East+Food+Decor), type='III')
Anova(lm(Price~Decor+Food+East), type='III')

# Compare to 
summary(m2)

## more tests on the slopes
library(car)
linearHypothesis(m2, "Food=1.5")
## this is equivelent to 
2*pt((1.5363-1.5)/0.2632, 164, lower.tail=FALSE)


linearHypothesis(m2, "East=3.58")
## this is equivelent to 
2*pt((2.0670-1.5)/0.9318, 164, lower.tail=FALSE)   

## test if \beta_1=\beta_2, same effects on Food and Decor   
linearHypothesis(m2, "Food=Decor")




######################################
# parameter estimation and inference
coef(m2)
confint(m2)

confint(m2, level=0.95)
confint(m2, level=0.90)
confint(m2, level=0.99)

# significance of individual predictors
summary(m2)




########################################
#Regression output on page 145
mfull <- lm(Price~Food+Decor+Service+East+Food:East+Decor:East+Service:East)
summary(mfull)

m_second <- lm(Price~Food+Decor+Service+East+Food:East+Service:East)
summary(m_second)

m_third <- lm(Price~Food+Decor+Service+East+Service:East)
summary(m_third)

m_fourth <- lm(Price~Food+Decor+Service+East)
summary(m_fourth)

#Regression output on page 146
mreduced <- lm(Price~Food+Decor+East)
summary(mreduced)

#Regression output on page 146
anova(mreduced,mfull)


detach(nyc)




#########################################################
## Model with interaction(s) / ANCOVA model


## Example 1 Amount spent on travel Page 141
setwd("D:/My Drive/ST 541/R files/Data")
# setwd("C:/Users/qwang57/Google Drive/ST 541/R files/Data")

travel <- read.table("travel.txt",header=TRUE)
attach(travel)

names(travel)
dim(travel)
head(travel)

plot(Age, Amount, col=travel$C+1)

#Regression output on page 141
mfull <- lm(Amount~Age+C+C:Age)
summary(mfull)

m1<-lm(Amount~Age)
summary(m1)

#Figure 5.7 on page 142
par(mfrow=c(1,1))
plot(Age[C==0],Amount[C==0],pch=c("A"),col=c("black"),ylab="Amount Spent",xlab="Age")
points(Age[C==1],Amount[C==1],pch=c("C"),col=c("red"))



##################################################
## Back to Auto.mpg data
# model with interaction

#auto.mpg <- read.table("C:/Users/qwang57/Google Drive/ST 552/R files/auto-mpg.data", 
#                       col.names=c("mpg", "cylinders", "displacement", "horsepower", 
#                                   "weight", "acceleration", "year", "origin", "model"),
#                       quote="\"", comment.char="")

auto.mpg <- read.table("D:/My Drive/ST 541/R files/auto-mpg.data", 
                       col.names=c("mpg", "cylinders", "displacement", "horsepower", 
                                   "weight", "acceleration", "year", "origin", "model"),
                       quote="\"", comment.char="")

dim(auto.mpg)

head(auto.mpg)

summary(auto.mpg[,1:8])

attach(auto.mpg)

weight=weight/1000
plot(weight, mpg)
plot(weight, mpg, pch=20, col=origin)


# The default color palette is: 
# 1 = "black"
# 2 = "red"
# 3 = "green"
# 4 = "blue"
# 5 = "cyan"
# 6 = "magenta"
# 7 = "yellow"
# 8 = "gray" 


# simple linear regression model between 'mpg' and 'weight'
m1 <- lm(mpg~weight)
summary(m1)
abline(lm(mpg~weight), lwd=2)

abline(lm(mpg[origin==3]~weight[origin==3]), col=3, lwd=4)
abline(lm(mpg[origin!=3]~weight[origin!=3]), col=2, lwd=4)


## A second order model
m3 <- lm(mpg~weight+I(weight^2))
summary(m3)
anova(m3)

plot(weight, mpg)
par(new=TRUE)
plot(weight, predict(m3), col = "red")


# model with both "weight" and "origin"
m2<-lm(mpg~weight+factor(origin))
summary(m2)

## parallel regression lines 
plot(weight, mpg, col=origin, pch=origin)
legend("topright",legend=c("US","Europe", "Japan"),pch=c(1,2,3),
       col=c(1,2,3), bty="n", cex=0.8)

abline(43.6959, -7.0234, lwd=2)
abline(44.9114, -7.0234, col=2, lwd=2)
abline(46.0513, -7.0234, col=3, lwd=2)

## or
coef(m2)
plot(weight, mpg, col=origin, pch=origin)
legend("topright",legend=c("US","Europe", "Japan"),pch=c(1,2,3),
       col=c(1,2,3), bty="n", cex=0.8)

abline(coef(m2)[1], coef(m2)[2], lwd=2)
abline(coef(m2)[1]+coef(m2)[3], coef(m2)[2], col=2, lwd=2)
abline(coef(m2)[1]+coef(m2)[4], coef(m2)[2], col=3, lwd=2)


#### model with interactions
m21 <- lm(mpg~weight + factor(origin) + weight:factor(origin))
summary(m21)

# the same model
m22 <- lm(mpg~weight*factor(origin))
summary(m22)

# be cautious to use this one (assume the same intercept)
m23 <- lm(mpg~weight:factor(origin))
summary(m23)

## geometric display of the interaction model
coef(m22)

plot(weight, mpg, col=origin, pch=origin)
legend("topright",legend=c("US","Europe", "Japan"),pch=c(1,2,3),
       col=c(1,2,3), bty="n", cex=0.8)

abline(coef(m22)[1], coef(m22)[2], lwd=2)
abline(coef(m22)[1]+coef(m22)[3], coef(m22)[2]+coef(m22)[5], col=2, lwd=2)
abline(coef(m22)[1]+coef(m22)[4], coef(m22)[2]+coef(m22)[6], col=3, lwd=2)


anova(m2, m22)   # partial F-test

# we noticed factor(origin)2 and weight:factor(origin)2 not significant
# a better model
jp<-(origin==3)*1

m24 <- lm(mpg~weight*jp)
summary(m24)


## geometric display of the interaction model
coef(m24)

plot(weight, mpg, col=jp+1, pch=jp+1)
legend("topright",legend=c("US or Europe", "Japan"),pch=c(1,2),
       col=c(1,2), bty="n", cex=0.8)

abline(coef(m24)[1], coef(m24)[2], lwd=2)
abline(coef(m24)[1]+coef(m24)[3], coef(m24)[2]+coef(m24)[4], col=2, lwd=2)




### Albuquerque Home Price Data   ###
library(HH)
data(houseprice)
houseprice
attach(houseprice)

plot(sqft, price)
plot(sqft, price, col=corner+1)

m1 <- lm(price~sqft*corner)
summary(m1)

coef(m1)
plot(sqft, price, col=corner+1, pch=corner+1)
legend("bottomright",legend=c("non-corner", "corner"),pch=c(1,2),
       col=c(1,2), bty="n", cex=0.8)

abline(coef(m1)[1], coef(m1)[2], lwd=2)
abline(coef(m1)[1]+coef(m1)[3], coef(m1)[2]+coef(m1)[4], col=2, lwd=2)

## corner house is cheaper!



####################################
## Simpson's paradox in practice

setwd("G:/My Drive/ST 541")
library(readr)
#Credit_data<-read_csv("C:/Users/qwang57/Google Drive/ST 541/Datasets/credit.csv")
Credit_data<-read_csv("D:/My Drive/ST 541/Datasets/credit.csv")


attach(Credit_data)
dim(Credit_data)
head(Credit_data)
names(Credit_data)

m1<-lm(Balance~Income)
summary(m1)
plot(Income, Balance)
abline(m1, col=2)

m2<-lm(Balance~Limit)
summary(m2)
plot(Limit, Balance)
abline(m2, col=3)

## model with both predictors
m3<-lm(Balance~Income+Limit)
summary(m3)

# notice the sign of "Income" is negative!
# sign reverse for Income in SLR and MLR.
# Income and Limit interact in the prediction of Balance
summary(lm(Balance~Income*Limit))   # how to interpret?

summary(Limit)

limit_indicator <- 1 + (Limit>3088) + (Limit>4622) + (Limit>5873)

plot(Income, Balance, col=limit_indicator, pch=2*limit_indicator)
legend("bottomright",legend=c("Low","Medium low", "Medium high", "High"),pch=c(2,4,6,8),col=c(1,2,3,4),
       bty="n", cex=0.8)

# The default color palette is: 
# 1 = "black" -- Limit<3088
# 2 = "red"   -- 3088<Limit<4622
# 3 = "green"  -- 4622<Limit<5873
# 4 = "blue"   -- Limit>5873

m31 <- lm(Balance~Income, data=subset(Credit_data, Limit<3088))
summary(m31)
abline(m31, col=1)

m32 <- lm(Balance~Income, data=subset(Credit_data, Limit>3088 & Limit<4622))
summary(m32)
abline(m32, col=2)

m33 <- lm(Balance~Income, data=subset(Credit_data, Limit>4622 & Limit<5873))
summary(m33)
abline(m33, col=3)

m34 <- lm(Balance~Income, data=subset(Credit_data, Limit>5873))
summary(m34)
abline(m34, col=4)

## aggregation bias ##

m3<-lm(Balance~Income+Limit)
summary(m3)




#################################################################
##  Baseball hitters' salary data revisit

# setwd("C:/Users/qwang57/Google Drive/ST 541/R files")

setwd("G:/My Drive/ST 541/R files")
baseball_data <- read.csv(file = "baseball.csv", header=TRUE)

dim(baseball_data)
head(baseball_data)

summary(baseball_data[,3:22])

baseball_data_new <- na.omit(baseball_data)
dim(baseball_data_new)

summary(baseball_data_new[,3:22])

attach(baseball_data_new)
names(baseball_data_new)

pairs(cbind(CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks, Years, Salary))





