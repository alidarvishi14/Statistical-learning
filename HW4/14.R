#a
set.seed (1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#b
cor(x1,x2)
library(ggplot2)
ggplot(mapping = aes(x=x1,y=x2))+geom_point()

#c
lm_c=lm(formula = y~x1+x2)
summary(lm_c)

#d
lm_d=lm(formula = y~x1)
summary(lm_d)

#e
lm_e=lm(formula = y~x2)
summary(lm_e)

#g
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)
par(mfrow=c(2,2))

lm_cg=lm(formula = y~x1+x2)
summary(lm_cg)
plot(lm_cg)

lm_dg=lm(formula = y~x1)
summary(lm_dg)
plot(lm_dg)

lm_eg=lm(formula = y~x2)
summary(lm_eg)
plot(lm_eg)