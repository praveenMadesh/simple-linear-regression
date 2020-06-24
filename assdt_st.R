# In the given data we have to predict the delivery time by using sorting time
# so Y <- delivery time as output and x <- sorting time as input
# Data type: Both input and output are continuous
library(readr)
dt_st<-read.csv(file.choose())
View(dt_st)
attach(dt_st)
str(dt_st)
## Measures of Central Tendency : mean,median,SD,var,IQR
sd(Delivery.Time)
sd(Sorting.Time)
#variances
var(Delivery.Time)
var(Sorting.Time)
#IQR
quantile(Delivery.Time)
quantile(Sorting.Time)
summary(dt_st)
# 2.visulization
qqnorm(Delivery.Time)
qqline(Delivery.Time)#by seeing QQplot data are normally distributed
qqnorm(Sorting.Time)
qqline(Sorting.Time)#by seeing QQplot data are normally distributed

#####simple model
plot(Sorting.Time,Delivery.Time)#by this direction of data is +ve,strength of data is moderate
cor(Sorting.Time,Delivery.Time)#correlation is 0.82 =strong

reg_exp<-lm(Delivery.Time~Sorting.Time,data = dt_st)
summary(reg_exp) #in this R^2 value=0.6823 and also pvalue <0.05

red<-predict(reg_exp)
pred ###predict values 

reg_exp$residuals###predicted value - actual value =erroe or residuals
sqrt(sum(reg_exp$residuals^2)/nrow(dt_st))
sqrt(mean(reg_exp$residuals^2))

confint(reg_exp,level=0.90)######confidence interval values
predict(reg_exp,interval="confidence")#In this line we can see that fit value,lower and upper value (intervals)
#######
library(ggplot2)

ggplot(data=dt_st,aes(x=Sorting.Time,y=log(Delivery.Time)))+
  geom_point(color="red")+
  geom_line(color="black",data=dt_st,aes(x=Sorting.Time,y=pred))

