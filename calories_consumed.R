### In the given data we have to predict the weight gain by using calories consumed
# so Y <- weight gain as output and x <- calories consumed
# Data type: Both input and output are continuous
library(readr)
c_w<-read.csv(file.choose())
View(c_w)
summary(c_w)
plot(c_w)
attach(c_w)
cor(Weight.gained..grams.,Calories.Consumed)##correlation 
############ Measures of Central Tendency : mean,median,SD,var,IQR
sd(Weight.gained..grams.)
sd(Calories.Consumed)
#variances
var(Weight.gained..grams.)
var(Calories.Consumed)
#IQR
quantile(Weight.gained..grams.)
quantile(Calories.Consumed)
summary(c_w)
# 2.visulization
qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)#by seeing QQplot data are slightly normally distributed and +ve direction
qqnorm(Calories.Consumed)
qqline(Calories.Consumed)#by seeing QQplot data are normally distributed and +ve direction

###histogram
hist(Weight.gained..grams.) #right skewed 
hist(Calories.Consumed)#right skewedS

##simple linear regression
c_w1<-lm(Calories.Consumed~Weight.gained..grams.)###lm(y~x)
summary(c_w1)## p value less then 0.05 and R^2 value= 0.8968 data are significant
pred<-predict(c_w1)
pred  ###predicted values for calories consumed
c_w1$residuals### predicted value - actual value =erroe or residuals
sqrt(sum(c_w1$residuals^2)/nrow(c_w))
sqrt(mean(c_w1$residuals^2))
confint(c_w1,level=0.95)####confidence interval values
predict(c_w1,interval = "predict")##In this line we can see that fit value,lower and upper value (intervals)  
#########
library(ggplot2)
ggplot(data=c_w,aes(x=Weight.gained..grams.,y=Calories.Consumed))+
  geom_point(color="blue")+
  geom_line(color="red",data=c_w,aes(x=Weight.gained..grams.,y=pred))

