setwd("G:/My Drive/ST 541/R files/Data")
# setwd("C:/Users/qwang57/Google Drive/ST 541/R files/Data")

## credit default
library(ISLR)
?Default

summary(Default)



## heart disease
heart1<-read.table(file = "G:/My Drive/ST 541/R files/SAheart.dat", header=T, sep=",")
chd1 <- heart1$chd
heart1$chd <- as.factor(heart1$chd)

heart <- heart1
attach(heart)

dim(heart)

plot(x=ldl, y = age, pch = as.character(chd), col = chd)

heart.logit=glm(chd~sbp+ldl+alcohol+age, family=binomial(link=logit))
summary(heart.logit)

eta = predict(heart.logit,type="link")
prob.predicted = plogis(eta)

plot(age, prob.predicted, col = chd1+1)



############################################
#----------------------------------------#
# UC Berkeley Admission Study
#----------------------------------------#

odds.ratio=function(x,addtocounts=0){
  x=x+addtocounts
  (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
}

data(UCBAdmissions)
dimnames(UCBAdmissions)
ftable(UCBAdmissions,row.vars="Dept",col.vars=c("Gender","Admit"))
#---------------------------------------------------#
########## Start with exploratory analysis ##########
#---------------------------------------------------#

ftable(round(prop.table(UCBAdmissions,c(2,3)),2),row.vars="Dept",col.vars=c("Gender","Admit"))
round(apply(UCBAdmissions,3,odds.ratio),2) # individual departmental odds ratio

UCBGbyA=margin.table(UCBAdmissions,c(2,1))
UCBGbyA
round(prop.table(UCBGbyA,1),2)
odds.ratio(UCBGbyA) # Marginal odds ratio. 
# Note that Simpson's paradox is present, will be illutrated better when GLM is fitted and we obtain
# conditional odds ratio e^beta

# Test for independence between admission status and gender
chisq.test(UCBGbyA,correct=FALSE)

round(prop.table(margin.table(UCBAdmissions,c(2,3)),1),2)
round(prop.table(margin.table(UCBAdmissions,c(1,3)),2),2)

# Most males apply to Dept A and B where acceptance has a higher rate while more females apply to
# Dept C,D,E,F where acceptance is lower


Dept=rep(c("A","B","C","D","E","F"),each=2)
Gender=rep(c("Male","Female"),6)
Counts=matrix(UCBAdmissions,ncol=2,byrow=TRUE,dimnames=list(NULL,c("Admit","Reject")))
berk=data.frame(Dept,Gender,Counts)
berk

UCB.logit=glm(cbind(Admit,Reject)~Gender+Dept,family=binomial(link=logit),
              contrasts=list(Dept=contr.treatment(6,base=6,contrasts=TRUE)),data=berk)
summary(UCB.logit)


UCB.logit1=glm(cbind(Admit,Reject)~Gender,family=binomial(link=logit), data=berk)
summary(UCB.logit1)

UCB.logit2=glm(cbind(Admit,Reject)~Gender+Dept+Gender*Dept,family=binomial(link=logit),
               contrasts=list(Dept=contr.treatment(6,base=6,contrasts=TRUE)),data=berk)
summary(UCB.logit2)




###################################
## grouped data
##
MichelinFood <- read.table("MichelinFood.txt", header=TRUE)
MichelinFood

attach(MichelinFood)

#Figure 8.1 on page 266
plot(Food,proportion,ylab="Sample proportion",xlab="Zagat Food Rating")

#R output on page 267
m1 <- glm(cbind(InMichelin,NotInMichelin)~Food,family=binomial)
summary(m1)

#Figure 8.2 on page 268
x <- seq(15,28,0.05)
y <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*x)))
plot(Food,proportion,ylab="Probability of inclusion in the Michelin Guide",xlab="Zagat Food Rating")
lines(x,y)

# LR confidence interval
confint(m1, level=0.95, type="LR") # -- likelihood ratio CI

#Table 8.2 on page 269
thetahat <- m1$fitted.values
odds_ratio <- m1$fitted.values/(1-m1$fitted.values)
cbind(Food,proportion, round(thetahat,3),round(odds_ratio,3))


# CI for predicted 
pred <- predict(m1, type="response")
pred


eta=predict(m1,newdata=data.frame(Food=20),type="link",se.fit=TRUE)

eta.ci=eta$fit+c(-1,1)*qnorm(0.975)*eta$se.fit
eta.ci         # This is (l,u) interval for eta
plogis(eta.ci) # This is (exp(l)/(1+exp(l)),exp(u)/(1+exp(u)))

  ## CI without the model
   (8/33)+c(-1.96, 1.96)*sqrt((8/33*(25/33)/33))

#p-value on page 272
pchisq(m1$deviance,m1$df.residual,lower=FALSE)

#Value of the difference in devinace and associated p-value on page 273
m1$null.deviance-m1$deviance
pchisq(m1$null.deviance-m1$deviance,1,lower=FALSE)

#Logistic regression output on page 274
print(paste("Pearson's X^2 =",round(sum(residuals(m1,type="pearson")^2),3)))

#Table 8.3 on page 276
cbind(round(residuals(m1,"response"),3),round(residuals(m1,"pearson"),3),round(residuals(m1,"deviance"),3))

#Figure 8.3 on page 276
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)
par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-2,2))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-2,2))

detach(MichelinFood)



############################
## ungrouped data

MichelinNY <- read.csv("MichelinNY.csv", header=TRUE)
attach(MichelinNY)

y <- InMichelin

#Figure 8.4 on page 278
par(mfrow=c(1,1))
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
     ylab="In Michelin Guide? (0=No, 1=Yes)")

#Figure 8.5 on page 279
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")

#Logistic regression output on page 279
m1 <- glm(y~Food,family=binomial,data=MichelinNY)
summary(m1)      ## the same output as the above grouped data analysis

#Figure 8.6 on page 281
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
#Alternatively we could use 
#stanresDeviance < rstandard(m1)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)
par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))

#Figure 8.7 on page 282
par(mfrow=c(1,1))
xx <- seq(15,28.2,0.05)
yy <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*xx)))
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
     ylab="In Michelin Guide? (0=No, 1=Yes)")
lines(xx,yy)
lines(xx,predict(loessfit1,data.frame(Food=xx)),lty=2)

#Figure 8.8 on page 286
par(mfrow=c(2,2))
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Decor~y, ylab="Decor Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Service~y, ylab="Service Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Price~y, ylab="Price",xlab="In Michelin Guide? (0=No, 1=Yes)")

#Figure 8.9 on page 288
m2 <- glm(y~Food+Decor+Service+Price+log(Price),family=binomial(),data=MichelinNY)
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
loessfit2 <- loess(m2$fitted.values ~ Food,degree=1,span=2/3)
xx <- seq(15,28.2,0.05)
summary(m2)
par(mfrow=c(1,2))
plot(Food,y,xlab="Food Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx,predict(loessfit1,data.frame(Food=xx)))
#lines(lowess(Food,y,iter=1,f=2/3))
plot(Food,m2$fitted.values,ylab=expression(hat(Y)),xlab="Food Rating, x1")
lines(xx,predict(loessfit2,data.frame(Food=xx)))

#Figure 8.10 on page 288
library(alr4)
mmps(m2,layout=c(2,3))


#Figure 8.11 on page 289
par(mfrow=c(1,1))
plot(Decor,Service,pch=y+1,col=y+1,xlab="Decor Rating",ylab="Service Rating")
abline(lsfit(Decor[y==0],Service[y==0]),lty=1,col=1)
abline(lsfit(Decor[y==1],Service[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Michelin Guide?")

#Figure 8.12 on page 290
m3 <- glm(y~Food+Decor+Service+Price+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
mmps(m3,layout=c(2,3))

#Output from R on page 290
anova(m2,m3,test="Chisq")

#Figure 8.13 on page 291
par(mfrow=c(1,1))
hvalues <- influence(m3)$hat
stanresDeviance <- residuals(m3)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.7))
abline(v=2*7/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

#Output from R on pages 291 and 292
summary(m3)

#Output from R on pages 292 and 293
m4 <- glm(y~Food+Decor+Service+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
anova(m4,m3,test="Chisq")
summary(m4)

#Figure 8.14 on page 294
mmps(m4,layout=c(2,3))

#Figure 8.15 on page 295
par(mfrow=c(1,1))
hvalues <- influence(m4)$hat
stanresDeviance <- residuals(m4)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.35))
abline(v=2*6/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

#Table 8.5 on page 295
fits4 <- m4$fitted.values
round(fits4[c(14,37,69,133,135,138,160)],3)


# LR confidence interval
confint(m4, level=0.95, type="LR")



#######################################
## Model selection
michelin.full <- glm(y~Food*Decor*Service+log(Price),family=binomial(),data=MichelinNY)
summary(michelin.full)

## Stepwise selection
## backward elimination is default
michelin.step <- step(michelin.full)
summary(michelin.step)

## forward selection
michelin.null <- glm(y ~ 1 , family=binomial(link=logit) ,data=MichelinNY)
michelin.forward <- step(michelin.null,
                     scope=list(lower=formula(michelin.null),upper=formula(michelin.full)), 
                     direction="forward",  data=MichelinNY)
summary(michelin.forward)


m5<-glm(y~Food+Service+log(Price)+Service:Food,family=binomial(),data=MichelinNY)
summary(m5)



########################################
## prediction and ROC
pred <- predict(m4, type="response")
pred
cbind(y, pred)

table(round(pred))
sum(pred>=0.5)

class.michelin <- data.frame(response = y, predicted = round(pred,0))
xtabs(~ predicted + response, data = class.michelin)


library(pROC)
michelin.roc <- roc(y ~ pred, plot = TRUE, print.auc = TRUE)

library(ROCR)    
pred <- prediction(pred, y)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve Michelin Zagat", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line


## goodness-of-fit
library(ResourceSelection)
hoslem.test(y, fitted(m4), g=10)


detach(MichelinNY)