library(numbers)
set.seed(100)
A = rnorm(n=100, mean=0, sd = 1)
B = rcauchy(n=100, location = 0, scale = 1)


#A
var_kfold <- function(data,f,k) {
  x=rep(0,k)
  n=length(data)
  for(i in 1:k){
    validation_samples = 1:(n/k)+(i-1)*n/k
    train_data = data[-validation_samples]
    test_data = data[validation_samples]
    theta = f(train_data)
    test_error=c()
    for (i in 1:length(test_data)){
      test_error[i]=f(test_data[i])-theta
    }
    
    x[i] = mean(test_error^2)
  }
  return(mean(x))
}

#B
var_kfold(A,mean,20)
var_kfold(B,mean,20)
var_kfold(A,median,20)
var_kfold(B,median,20)


#C
vars=c()
ks = divisors(100)
for (i in 1:length(ks)){
  vars[i]=var_kfold(A,mean,ks[i])
}
plot(x = ks,y=vars)
abline(a=0.01,b=0)
i_opt=which.min((vars - 0.01)^2)
points(ks[i_opt],vars[i_opt],pch=3)


#BOOTSTRAP
var_bootstrap <- function(data,f) {
  m=200
  n=length(data)
  x=rep(0,m)
  for(i in 1:m){
    sample = sample(x = data,size = n ,replace = TRUE)
    x[i]=f(sample)
  }
  return(var(x))
}

var_bootstrap(A,mean)
