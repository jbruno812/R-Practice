setwd("G:/My Drive/ST 541/R files/Data")
# setwd("C:/Users/qwang57/Google Drive/ST 541/R files/Data")

bridge <- read.table("bridge.txt", header=TRUE)
head(bridge)
dim(bridge)
attach(bridge)

#Figure 7.1 on page 235
m1 <- lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(m1)

anova(m1)          # just focus on RSS=3.8436
AIC(m1, k=2)        # n=45, p=5, AIC=nlog(RSS/n)+2p
   n<-dim(bridge)[1]
   p<-5
   rss<-deviance(m1)
   n+n*log(2*pi)+n*log(rss/n)+2*p+4

AIC(m1, k=log(n))   #BIC
BIC(m1)
    n+n*log(2*pi)+n*log(rss/n)+log(n)*p+2*log(n)


res.m1 <- resid(m1)
pres.m1 <- resid(m1)/(1 - lm.influence(m1)$hat) #predictively adjusted

sum(res.m1^2)     # RSS
sum(pres.m1^2)    # PRESS -- predicted RSS


## All subset selection
logDArea <- log(DArea)
logCCost <- log(CCost)
logDwgs <- log(Dwgs)
logLength <- log(Length)
logSpans <- log(Spans)
X <- cbind(logDArea,logCCost,logDwgs,logLength,logSpans)

n <- length(m1$residuals)

library(leaps)
b <- regsubsets(as.matrix(X),log(Time), nbest=2)
summary(b)

b <- regsubsets(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans), data=bridge)
rs <- summary(b)

par(mfrow=c(1,2))
plot(1:5,rs$rsq, xlab="Subset Size",ylab="R-squared")
plot(1:5,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")

rs$adjr2   # the largest value
rs$rss
rs$rsq
rs$bic     # the smallest value


## Backward elimination using AIC
backAIC <- step(m1,direction="backward", data=bridge)
backAIC$coefficients
backAIC$anova

## Backward elimination using BIC
backBIC <- step(m1,direction="backward", data=bridge, k=log(n))
backBIC$coefficients
backBIC$anova


## Forward selection using AIC

mint <- lm(log(Time)~1,data=bridge)  # intercept only model
forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans)),
                   direction="forward", data=bridge)
forwardAIC$coefficients
forwardAIC$anova

## Forward selection using BIC
forwardBIC <- step(mint,scope=list(lower=~1, 
                                   upper=~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans)),
                   direction="forward", data=bridge,k=log(n))
forwardBIC$coefficients
forwardBIC$anova


## Stepwise using BIC
stepwiseBIC <- step(m1,direction="both", data=bridge, k=log(n))
stepwiseBIC$coefficients
stepwiseBIC$anova

## Stepwise using AIC
stepwiseAIC <- step(m1,direction="both", data=bridge, k=2)
stepwiseAIC$coefficients
stepwiseAIC$anova


## Lasso
library(glmnet)

fit <- glmnet(as.matrix(X),log(Time))
plot(fit, "norm", label = TRUE)

cvfit <- cv.glmnet(as.matrix(X),log(Time))
plot(cvfit)

coef(fit, s=cvfit$lambda.min)


detach(bridge)




############################################
# cross-validataion
##
library(DAAG)
houseprices

lm.house<-lm(sale.price~area, data=houseprices)
summary(lm.house)
anova(lm.house)

res.m1 <- resid(lm.house)  # raw residual y-y_hat
pres.m1 <- resid(lm.house)/(1 - lm.influence(lm.house)$hat) #predictively adjusted

sum(res.m1^2)/13     # MSE
sum(pres.m1^2)      # PRESS

CVlm(houseprices, lm.house, m=5, seed=54)   # add seed= for randomness

CVlm(houseprices, lm.house, m=15)  # LOOCV
sum(pres.m1^2)/15


sres <- rstandard(lm.house)  # standardized residuals
sres[which(abs(sres) > 2)]

sdelres <- rstudent(lm.house)  # studentized residuals
sdelres[which(abs(sdelres) > 2)]







###########################
## Car seat position example


library(MASS)
library(HH)
library(car)
library(faraway)

?seatpos
pairs(seatpos, col="blue")

lm.hip <- lm(hipcenter ~ ., data = seatpos)
summary(lm.hip)                  # model significant, but no significant predictor

vif(lm.hip)

step <- stepAIC(lm.hip, direction="backward")
step$anova
summary(step)

n<-length(seatpos$hipcenter)
step <- step(lm.hip, direction="backward", k=log(n))
step$anova
summary(step)


#################################################################
##  Prostate cancer data example
##   textbook p239-249

setwd("G:/My Drive/ST 552/R files/Data")
prostateTraining <- read.table("prostateTraining.txt", header=TRUE)
attach(prostateTraining)

#Figure 7.2 on page 240
pairs(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)

#Figure 7.3 on page 241
m1 <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
StanRes1 <- rstandard(m1)
par(mfrow=c(3,3))
plot(lcavol,StanRes1, ylab="Standardized Residuals")
plot(lweight,StanRes1, ylab="Standardized Residuals")
plot(age,StanRes1, ylab="Standardized Residuals")
plot(lbph,StanRes1, ylab="Standardized Residuals")
plot(svi,StanRes1, ylab="Standardized Residuals")
plot(lcp,StanRes1, ylab="Standardized Residuals")
plot(gleason,StanRes1, ylab="Standardized Residuals")
plot(pgg45,StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted values")

#Figure 7.4 on page 241
par(mfrow=c(1,1))
plot(m1$fitted.values,lpsa,xlab="Fitted Values")
abline(lsfit(m1$fitted.values,lpsa))

#Figure 7.5 on page 242
par(mfrow=c(2,2))
plot(m1)
abline(v=2*9/67,lty=2)

#Regression output on pages 242 and 243
summary(m1)

#Figure 7.6 page 243
library(alr3)
par(mfrow=c(3,3))
mmp(m1,lcavol,key=NULL)
mmp(m1,lweight,key=NULL)
mmp(m1,age,key=NULL)
mmp(m1,lbph,key=NULL)
mmp(m1,lcp,key=NULL)
mmp(m1,gleason,key=NULL)
mmp(m1,pgg45,key=NULL)
mmp(m1,m1$fitted.values,xlab="Fitted Values",key=NULL)

#R output on page 244
library(car)
vif(m1)

#Figure 7.7 on page 244
library(car)
par(mfrow=c(2,4))
avp(m1,variable=lcavol,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=lweight,ask=FALSE,identify.points=TRUE, main="")
avp(m1,variable=age,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=lbph,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=svi,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=lcp,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=gleason,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=pgg45,ask=FALSE,identify.points=FALSE, main="")

#Figure 7.8 on page 245
X <- cbind(lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa)
rs <- summary(b)
par(mfrow=c(1,2))
library(car)
subsets(b,statistic=c("adjr2"),min.size=1,max.size=4,cex.subsets=0.7)
subsets(b,statistic=c("adjr2"),min.size=5,max.size=8,cex.subsets=0.7,legend=FALSE)

#Table 7.2 on page 245
#Calculate adjusted R-squared
rs$adjr2
om1 <- lm(lpsa~lcavol)
om2 <- lm(lpsa~lcavol+lweight)
om3 <- lm(lpsa~lcavol+lweight+svi)
om4 <- lm(lpsa~lcavol+lweight+svi+lbph)
om5 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45)
om6 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp)
om7 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp+age)
om8 <- m1
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))
#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))
#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))
#Subset size=4
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))
#Subset size=5
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om5,k=log(n))
#Subset size=6
npar <- length(om6$coefficients) +1
#Calculate AIC
extractAIC(om6,k=2)
#Calculate AICc
extractAIC(om6,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om6,k=log(n))
#Subset size=7
npar <- length(om7$coefficients) +1
#Calculate AIC
extractAIC(om7,k=2)
#Calculate AICc
extractAIC(om7,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om7,k=log(n))
#Subset size=8
npar <- length(om8$coefficients) +1
#Calculate AIC
extractAIC(om8,k=2)
#Calculate AICc
extractAIC(om8,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om8,k=log(n))

#Regression output on page 246
summary(om2)
summary(om4)
summary(om7)

detach(prostateTraining)


prostateTest <- read.table("prostateTest.txt", header=TRUE)
attach(prostateTest)

#Regression output on page 247
om2 <- lm(lpsa~lcavol+lweight)
summary(om2)
om4 <- lm(lpsa~lcavol+lweight+svi+lbph)
summary(om4)
om7 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp+age)
summary(om7)

detach(prostateTest)


prostateTraining <- read.table("prostateTraining.txt", header=TRUE)
attach(prostateTraining)

#Figure 7.9 on page 249
X <- cbind(lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa)
rs <- summary(b)
par(mfrow=c(1,2))
library(car)
subsets(b,statistic=c("adjr2"),main="With Case 45",min.size=1,max.size=5,cex.subsets=0.7)

m2 <- update(m1, subset=(1:67)[-c(45)])
lcavol1 <- lcavol[-c(45)]
lweight1 <- lweight[-c(45)]
age1 <- age[-c(45)]
lbph1 <- lbph[-c(45)]
svi1 <- svi[-c(45)]
lcp1 <- lcp[-c(45)]
gleason1 <- gleason[-c(45)]
pgg451 <- pgg45[-c(45)]
X <- cbind(lcavol1,lweight1,age1,lbph1,svi1,lcp1,gleason1,pgg451)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa[-c(45)])
rs <- summary(b)
library(car)
subsets(b,statistic=c("adjr2"),main="Without Case 45",min.size=1,max.size=5,cex.subsets=0.7,legend=FALSE)

detach(prostateTraining) 


prostateAlldata <- read.table("prostateAlldata.txt", header=TRUE)
attach(prostateAlldata)

#Figure 7.10 on page 249
par(mfrow=c(1,1))
plot(lweight[train==FALSE],lpsa[train==FALSE],pch=2,col=1,xlab="lweight",ylab="lpsa",ylim=c(-1,6),xlim=c(2,6.5))
abline(lsfit(lweight[train==FALSE],lpsa[train==FALSE]),lty=1,col=1)
points(lweight[train==TRUE],lpsa[train==TRUE],pch=3,col=2)
abline(lsfit(lweight[train==TRUE],lpsa[train==TRUE]),lty=2,col=2)
legend(4.5,2,legend=c("Training","Test"),pch=3:2,col=2:1,title="Data Set")

detach(prostateAlldata)


prostateTest <- read.table("prostateTest.txt", header=TRUE)
attach(prostateTest)

#Figure 7.11 on page 250
m1 <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
library(car)
par(mfrow=c(1,1))
avp(m1,variable=lweight,ask=FALSE,identify.points=TRUE, main="")

detach(prostateTest)





#################################################################
##  Baseball hitters' salary data revisit
##  Model Selection

library(ISLR)

summary(Hitters)

## There are some missing values here, so before we proceed we will remove them:

Hitters=na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

## Best Subset regression
## We will now use the package 'leaps' to evaluate all the best subset models

library(leaps)

regfit.full=regsubsets(Salary~., data=Hitters)
summary(regfit.full)

### It gives by default best subsets up to size 8, lets increase that to 19, with all the predictors

regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)

## Cp 
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], pch=20, col="red")

plot(regfit.full, scale="Cp")
coef(regfit.full, 10)

## BIC 
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], pch=20, col="red")

plot(regfit.full, scale="bic")
coef(regfit.full, 6)


## Stepwise Selection
## Backward elimination using BIC
m.full <- lm(Salary~., data=Hitters)
backBIC <- step(m.full,direction="backward", data=Hitters, k=log(n))
backBIC$coefficients
backBIC$anova

## Forward selection using BIC
mint <- lm(Salary~1,data=Hitters)  # intercept only model
forwardBIC <- step(mint,scope=list(lower=~1, 
                                   upper=~AtBat+Hits+HmRun+Runs+RBI+Walks+Years+CAtBat+CHits+CHmRun+
                                     CRuns+CRBI+CWalks+League+Division+PutOuts+Assists+Errors+NewLeague),
                   direction="forward", data=Hitters, k=log(n))
forwardBIC$coefficients
forwardBIC$anova


## Model selection using a validation set
## Lets make a training and validation set, so that we can choose a good subset model. 

dim(Hitters)

set.seed(2020)
train=sample(seq(263), 180, replace=FALSE)
train

regfit.fwd=regsubsets(Salary~., data=Hitters[train,], nvmax=19, method="forward")
### Now we make predictions on the obs not used for training. 
### We know there are 19 models, so we set up some vectors to record the errors. 
### We have to do a bit of work here, because there is no predict method for regsubsets.

val.errors=rep(NA, 19)
x.text=model.matrix(Salary~., data=Hitters[-train,]) # notice the -index!
for(i in 1:19){
  coefi=coef(regfit.fwd, id=i)
  pred=x.text[, names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}
plot(sqrt(val.errors), ylab="RootMSE", ylim=c(250, 400), pch=19, type="b")
points(sqrt(regfit.fwd$rss[-1]/180), col="blue", pch=19, type="b")
legend("bottomleft", legend=c("Training", "Validation"), col=c("blue", "black"), pch=19)

### As we expect, training error goes down monotonically as the model gets bigger, 
### but not for the validation error.



predict.regsubsets=function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  mat[, names(coefi)]%*%coefi
}



## Model selection by cross validation
## We will do 10 fold cross validation. 

set.seed(2020)
folds=sample(rep(1:10, length=nrow(Hitters)))
folds

table(folds)

cv.errors=matrix(NA, 10, 19)
for (k in 1:10){
  best.fit=regsubsets(Salary~., data=Hitters[folds!=k,], nvmax=19, method="forward")
  for (i in 1:19){
    pred=predict(best.fit, Hitters[folds==k,], id=i)
    cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type="b")

summary(best.fit)


## Ridge Regression and the Lasso ---------- 

library(glmnet)

x=model.matrix(Salary~.-1, data=Hitters)
y=Hitters$Salary
## First we fit ridge. we call glmnet with alpha=0(see helpfile). 
## There is also a cv.glmnet function which will do the cross validation for use.

fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge, xvar="lambda", label=TRUE)


cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

## Now use Lasso; for this we use the default alpha=1

fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda", label=TRUE)


plot(fit.lasso,xvar="dev", label=TRUE)


cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)


coef(cv.lasso)




