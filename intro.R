data<-read.csv('/Users/viren/Desktop/regression_miniproject/insurance.csv')
data

data2<-read.csv('/Users/viren/Desktop/regression_miniproject/insurance 2.csv')
data2

data3<-read.csv('/Users/viren/Desktop/regression_miniproject/Chennai houseing sale.csv')
data3

names(data3)
summary(data3)

data4<-read.csv('/Users/viren/Desktop/regression_miniproject/winequality.csv')
data4
names(data4)
summary(data4)

names<-list(names(data4))
names

model<-lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+
            free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=data4)
summary(model)

residuals<-studres(model)
predictions<-model$fitted.values
res<-data.frame(residuals)
names(res)
outliers<-data.frame(res[which(res$residuals>2),])
View(outliers)
