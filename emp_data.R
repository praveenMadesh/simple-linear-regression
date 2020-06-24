### In the given data we have to predict the Churn_out_rate  by using Salary_hike
# so Y <- Churn_out_rate and x <- Salary_hike
# Data type: Both input and output are continuous
library(readr)
churn<-read.csv(file.choose())
View(churn)
summary(churn)
plot(churn$Churn_out_rate,churn$Salary_hike)##if churn out rate increases salary hile decreases
attach(churn)
Salary_hike
Churn_out_rate
cor(Churn_out_rate,Salary_hike)####  -ve correlation between y and x

############ Measures of Central Tendency : mean,median,SD,var,IQR
sd(Salary_hike)
sd(Churn_out_rate)
#variances
var(Salary_hike )
var(Churn_out_rate)
#IQR
quantile(Salary_hike)
quantile(Churn_out_rate)
summary(churn)
# 2.visulization
qqnorm(Salary_hike)
qqline(Salary_hike)# by seeing QQplot data are  normally distributed and +ve direction
qqnorm(Churn_out_rate)
qqline(Churn_out_rate) #by seeing QQplot data are normally distributed and +ve direction

###histogram
hist(Salary_hike) #right skewed 
hist(Churn_out_rate)#right skewed

#####sikmple linear regression
reg<-lm(Churn_out_rate~Salary_hike) #lm(y~x)
summary(reg)   ####### p value less then 0.05 and R^2 value= 0.8312 data are significant 
pred<-predict(reg)
pred    ##### predicted value 
reg$residuals ## predicted value - actual value =erroe or residuals
sqrt(sum(reg$residuals^2)/nrow(churn))
sqrt(mean(reg$residuals^2))
confint(reg,level=0.90) ####confidence interval values
predict(reg,interval = "predict") ####In this line we can see that fit value,lower and upper value (intervals)  
#######
library(ggplot2)

ggplot(data=churn,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(color="red")+
  geom_line(color="black",data=churn,aes(x=Salary_hike,y=pred))
