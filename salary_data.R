### In the given data we have to predict the salary by using year of experience
# so Y <- salary and x <- YearsExperience
# Data type: Both input and output are continuous
library(readr)
ye_sd<-read.csv(file.choose())
View(ye_sd)

summary(ye_sd)

plot(ye_sd$Salary,ye_sd$YearsExperience)### data are in +ve direction and linear relationship
attach(ye_sd)
cor(YearsExperience,Salary)### +ve correlation coefficient

############ Measures of Central Tendency : mean,median,SD,var,IQR
sd(Salary)
sd(YearsExperience)
#variances
var(Salary)
var(YearsExperience)
#IQR
quantile(Salary)
quantile(YearsExperience)
summary(ye_sd)
# 2.visulization
qqnorm(Salary)
qqline(Salary)# by seeing QQplot data are slight normally distributed and +ve direction
qqnorm(YearsExperience)
qqline(YearsExperience) #by seeing QQplot data are normally distributed and +ve direction

###histogram
hist(Salary_hike) #right skewed 
hist(Churn_out_rate)#right skewed

######simple linear regression
ye_sd1<-lm(Salary~YearsExperience) ##lm(y~x)
summary(ye_sd1) ##### p value less then 0.05 and R^2 value= 0.957 data are significant
##R^2 value is better in simple linear regression
pred <- predict(ye_sd1)
pred  ###predicted values
ye_sd1$residuals ### residuals= predicted values - actual value
sqrt(ye_sd1$residuals^2)/nrow(ye_sd1)
sqrt(mean(ye_sd1$residuals^2))
confint(ye_sd1,level=0.98) ###confidence interval values 
predict(ye_sd1,interval="predict")#####In this line we can see that fit value,lower and upper value (intervals)  

############logrithamric
plot(log(YearsExperience),Salary) ###applying log to y and ploting
cor(log(YearsExperience),Salary) ###applying log to y and correlation coefficient value is 0.924
ye_sd_log<-lm(Salary~log(YearsExperience))## lm(log(y)~x)
summary(ye_sd_log)### R^2 value is 0.8539
predict(ye_sd_log)
ye_sd_log$residuals
sqrt(sum(ye_sd_log$residuals^2)/nrow(ye_sd))
sqrt(mean(ye_sd_log$residuals^2))
confint(ye_sd_log,level=0.95)
predict(ye_sd_log,interval = "confidence")

#############Exponential 
plot(YearsExperience,log(Salary))##applying log to x 
cor(YearsExperience,log(Salary))##applying log to x and cor value is 0.9653

ye_sd_exp<-lm(YearsExperience~log(Salary))### lm(y~log(x))
summary(ye_sd_exp)### R^2 value is 0.932 
predict(ye_sd_exp)
sqrt(sum(ye_sd_exp$residuals^2)/nrow(ye_sd))
sqrt(mean(ye_sd_exp$residuals^2))
confint(ye_sd_exp,level=0.99)
predict(ye_sd_exp,interval="confidence")
#######
library(ggplot2)

ggplot(data=ye_sd_exp,aes(x=YearsExperience,y=Salary))+
  geom_point(color="red")+
  geom_line(color="black",data=ye_sd_exp,aes(x=YearsExperience,y=pred))

