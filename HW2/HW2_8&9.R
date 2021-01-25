library(ggplot2)
library(ISLR)

linear_model=lm(data=Auto,mpg~horsepower)
summary(linear_model)
newdata=cbind(predict(linear_model,interval = "prediction"),Auto)
ggplot(newdata,aes(x=horsepower,y=mpg))+geom_point()+geom_smooth(method="lm")+geom_line(aes(y=lwr))+geom_line(aes(y=upr))
predict(linear_model,data.frame(horsepower=c(98)),interval = "confidence")
predict(linear_model,data.frame(horsepower=c(98)),interval = "prediction")
plot(linear_model)

#----#

pairs(Auto[,c(-9)])
cor(Auto[,c(-9)])
linear_model=lm(data=Auto,mpg~.-name)
summary(linear_model)
plot(linear_model)

linear_model_with_intersection=lm(data=Auto,mpg~horsepower:acceleration+displacement*origin-displacement+weight+horsepower)
summary(linear_model_with_intersection)
plot(linear_model_with_intersection)

linear_model_with_polyandlog=lm(data=Auto,mpg~horsepower:acceleration+displacement*origin-displacement+weight+horsepower+I(horsepower^2)+log(weight))
summary(linear_model_with_polyandlog)
plot(linear_model_with_polyandlog)
