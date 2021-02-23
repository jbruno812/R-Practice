rm(list = ls()) # remove all the variables from the workspace
library(forecast)
require(NTS)
library(Hmisc)
library(neuralnet)
ft<-read.csv(file="/Users/JohnBruno/Documents/fetchme data.csv", header=TRUE, sep=",")

we<-read.csv(file="/Users/JohnBruno/Downloads/weather.csv", header=TRUE, sep=",")


ft$wc<-we$PRCP
ft$wc.d<-0
ft$wc.d[ft$wc>0]<-1

ft$wc.t1<-0
ft$wc.t2<-0
ft$wc.t3<-0
ft$wc.t2[ft$wc>0 & ft$wc<=0.07]<-1
ft$wc.t3[ft$wc>0.07]<-1

date<-c("M","T","W","R","F","SA","SU")
ft$date<-c(rep(date, 52), "M")
ft$time<-seq(1,365,1)
ft$time2<-ft$time*ft$time
ft$Mon<-0
ft$Mon[ft$date=="M"]<-1
ft$Tue<-0
ft$Tue[ft$date=="T"]<-1
ft$Wed<-0
ft$Wed[ft$date=="W"]<-1
ft$Thu<-0
ft$Thu[ft$date=="R"]<-1
ft$Fri<-0
ft$Fri[ft$date=="F"]<-1
ft$Sat<-0
ft$Sat[ft$date=="SA"]<-1
ft$Sun<-0
ft$Sun[ft$date=="SU"]<-1
ft$logy<-log(ft$Total.Order+1)


# change column name
colnames(ft)[colnames(ft)=="Average.Dinner.Order"] <- "ado" 
colnames(ft)[colnames(ft)=="Average.Lunch.Order"] <- "alo"
colnames(ft)[colnames(ft)=="Average.Breakfast.Order"] <- "abo"

colnames(ft)[colnames(ft)=="Average.Daily.Delivrery.Time"] <- "addt"
colnames(ft)[colnames(ft)=="Average.Dinner.Delivery.Time"]<-"adt"
colnames(ft)[colnames(ft)=="Average.Lunch.Delivery.Time"]<-"alt"
colnames(ft)[colnames(ft)=="Average.Breakfast.Delivery.Time"]<-"abt"

colnames(ft)[colnames(ft)=="Dinner.Drivers"]<-"dd"
colnames(ft)[colnames(ft)=="Lunch.Drivers"]<-"ld"
colnames(ft)[colnames(ft)=="Breakfast.Drivers"]<-"bd"

colnames(ft)[colnames(ft)=="Dinner"]<-"d"
colnames(ft)[colnames(ft)=="Lunch"]<-"l"
colnames(ft)[colnames(ft)=="Breakfast"]<-"b"



# create lagged variable of total order
ft$lag.y1<-Lag(ft$Total.Order, +1) #lag  order 1
ft$lag.y2<-Lag(ft$Total.Order, +2) #lag  order 2
ft$lag.y3<-Lag(ft$Total.Order, +3) #lag  order 3
# create lagged variable of weather conditions
ft$lag.logy<-Lag(ft$logy, +1)
ft$lag.wc.d1<-Lag(ft$wc.d, +1)      #lag order 1
ft$lag.wc.d2<-Lag(ft$wc.d,+2)       #lag order 2
ft$lag.wc.d3<-Lag(ft$wc.d,+3)       #lag order 3
ft$lag.wc.d4<-Lag(ft$wc.d,+4)       #lag order 4
ft$lag.wc.d5<-Lag(ft$wc.d,+5)       #lag order 5
ft$lag.wc.d6<-Lag(ft$wc.d,+6)       #lag order 6
ft$lag.wc.t2<-Lag(ft$wc.t2, +1)      #lag order 1
ft$lag.wc.t3<-Lag(ft$wc.t3,+1) #lag order 1

# create lagged variable for  average daily delivery time
ft$addt1<-as.numeric(ft$addt)
ft$lag.addt11<-Lag(ft$addt1,+1) #lag order 1

#create lagged variable for drivers
ft$lag.dd1<-Lag(ft$dd,+1) #lag order 1
ft$lag.dd2<-Lag(ft$dd,+2) #lag order 2
ft$lag.dd3<-Lag(ft$dd,+3) #lag order 3

ft$lag.ld1<-Lag(ft$ld,+1) #lag order 1
ft$lag.ld2<-Lag(ft$ld,+2) #lag order 2
ft$lag.ld3<-Lag(ft$ld,+3) #lag order 3


ft$lag.bd1<-Lag(ft$bd,+1) #lag order 1
ft$lag.bd2<-Lag(ft$bd,+2) #lag order 2
ft$lag.bd3<-Lag(ft$bd,+3) #lag order 3
ft$lag.bd4<-Lag(ft$bd,+4) #lag order 4


#create lagged variable for B,L,& Dinner
ft$lag.b1<-Lag(ft$b,+1) #lag order 1
ft$lag.b2<-Lag(ft$b,+2) #lag order 2
ft$lag.b3<-Lag(ft$b,+3) #lag order 3
ft$lag.b4<-Lag(ft$b,+4) #lag order 4


ft$lag.l1<-Lag(ft$l,+1) #lag order 1
ft$lag.l2<-Lag(ft$l,+2) #lag order 2
ft$lag.l3<-Lag(ft$l,+3) #lag order 3
ft$lag.l4<-Lag(ft$l,+4) #lag order 4


ft$lag.d1<-Lag(ft$d,+1) #lag order 1
ft$lag.d2<-Lag(ft$d,+2) #lag order 2
ft$lag.d3<-Lag(ft$d,+3) #lag order 3
ft$lag.d4<-Lag(ft$d,+4) #lag order 4






#create lagged variable for overall average 
ft$lag.oa<-Lag(ft$Overall.Average,+1) #lag order 1
ft$lag.oa1<-Lag(ft$Overall.Average,+2) #lag order 2
ft$lag.oa2<-Lag(ft$Overall.Average,+3) #lag order 3
ft$lag.oa3<-Lag(ft$Overall.Average,+4) #lag order 4


#create lagged variable for breakfast,lunch and dinner ratios
ft$Br<-as.numeric(ft$Breakfast.Ratio..orders.to.drivers.)
ft$Lr<-as.numeric(ft$Lunch.Ratio..orders.to.drivers.)
ft$Dr<-as.numeric(ft$Dinner.Ratio..orders.to.drivers.)
ft$lag.Br1<-Lag(ft$Br,+1) #lag order 1
ft$lag.Lr1<-Lag(ft$Lr,+1) #lag order 1
ft$lag.Dr1<-Lag(ft$Dr,+1) #lag order 1
ft$lag.Br2<-Lag(ft$Br,+2) #lag order 2
ft$lag.Lr2<-Lag(ft$Lr,+2) #lag order 2
ft$lag.Dr2<-Lag(ft$Dr,+2) #lag order 2
ft$lag.Lr3<-Lag(ft$Lr,+3) #lag order 3
ft$lag.Dr3<-Lag(ft$Dr,+3) #lag order 3

#create lagged variables for average dinner,lunch and breakfast delivery time
ft$abt1<-as.numeric(ft$abt)
ft$lag.abt11<-Lag(ft$abt1,+1) #lag order 1
ft$lag.abt12<-Lag(ft$abt1,+2) #lag order 2
ft$lag.abt13<-Lag(ft$abt1,+3) #lag order 3
ft$adt1<-as.numeric(ft$adt)
ft$lag.adt11<-Lag(ft$adt1,+1) #lag order 1
ft$lag.adt12<-Lag(ft$adt1,+2) #lag order 2
ft$lag.adt13<-Lag(ft$adt1,+3) #lag order 3
ft$alt1<-as.numeric(ft$alt)
ft$lag.alt11<-Lag(ft$alt1,+1) #lag order 1
ft$lag.alt12<-Lag(ft$alt1,+2) #lag order 2
ft$lag.alt13<-Lag(ft$alt1,+3) #lag order 3


#create lagged variables for average dinner,lunch and breakfast orders
ft$abo1<-as.numeric(ft$abo)
ft$alo1<-as.numeric(ft$alo)
ft$ado1<-as.numeric(ft$ado)

ft$lag.abo1<-Lag(ft$abo1,+1) #lag order 1
ft$lag.alo1<-Lag(ft$alo1,+1) #lag order 1
ft$lag.ado1<-Lag(ft$ado1,+1) #lag order 1
ft$lag.abo2<-Lag(ft$abo1,+2) #lag order 2
ft$lag.alo2<-Lag(ft$alo1,+2) #lag order 2
ft$lag.ado2<-Lag(ft$ado1,+2) #lag order 2
ft$lag.abo3<-Lag(ft$abo1,+3) #lag order 3
ft$lag.alo3<-Lag(ft$alo1,+3) #lag order 3
ft$lag.ado3<-Lag(ft$ado1,+3) #lag order 3

#create time series plot for total order data set and check whether they are normal or not with the histogram
Total.Order<-ts(ft$Total.Order, freq=7)
plot(Total.Order, xlab="Time", ylab="Total Order",
     ylim=c(0,175), bty="l")
hist(ft$Total.Order)

log.Total.Order<-ts(ft$logy, freq=7)
ts.plot(log.Total.Order, xlab="Time", ylab="Total Order",bty="l")
hist(ft$logy)




## extract a set to train the NN
trainset <- ft[1:285, ];
trainset<-trainset[2:285,]
## select the test set; forecasting 80 observations
testset <- ft[286:365, ]

#regression with dates and lagged dependent of total order
fm0<-lm(Total.Order~as.factor(date)+lag.y1, data=trainset)
summary(fm0)
yhat0<-predict(fm0, new=testset)
er0 <- yhat0-testset$Total.Order 
mean(er0^2); mean(abs(er0))

#regression of lagged dependent of total order + lag of average breakfast and dinner delivery time+ lag of averages+ lag of breakfast and dinner ratios
fm1<-lm(Total.Order~lag.y1+as.factor(date)+lag.abt11+lag.adt12+lag.oa2+lag.Dr1+lag.Br2, data=trainset)
summary(fm1)
yhat1<-predict(fm1, new=testset)
er1 <- yhat1-testset$Total.Order 
mean(er1^2); mean(abs(er1))

#regression of lagged dependent of total order + lag of average breakfast and dinner delivery time+ lag of averages+ lag of breakfast and dinner ratios+ lag of weather conditions
fm2<-lm(Total.Order~lag.y1+as.factor(date)+lag.abt11+lag.adt12+lag.oa2+lag.Dr1+lag.Br2+lag.wc.d1+lag.wc.d2+lag.wc.d4+wc.t3+lag.wc.t2, data=trainset)
summary(fm2)
yhat2<-predict(fm2, new=testset)
er2 <- yhat2-testset$Total.Order 
mean(er2^2); mean(abs(er2))

#regression of lagged dependent of total order + lag of average breakfast and dinner delivery time+ lag of averages+ lag of breakfast and dinner ratios+ lag of weather conditions+ lag of dinner and breakfast drivers
fm3<-lm(Total.Order~lag.y1+as.factor(date)+lag.abt11+lag.adt12+lag.oa2+lag.bd3+lag.dd2+lag.wc.d1+lag.wc.d2+lag.wc.d4+wc.t3+lag.wc.t2+lag.Dr1+lag.Br2, data=trainset)
summary(fm3)
yhat3<-predict(fm3,new=testset)
er3<-yhat3-testset$Total.Order
mean(er3^2);mean(abs(er3))



plot.ts(trainset$Total.Order, xlab="Time", ylab="Total Order",
        ylim=c(0,175), bty="l")
lines(predict(fm3), lwd=2, col="red")

plot.ts(testset$Total.Order, xlab="Time", ylab="Total Order",
        ylim=c(0,175), bty="l")
lines(predict(fm3, new=testset), lwd=1, lty=2, col="blue")
