#Problem1
# Define a function

gadilijiang = function(X)   
{a= X[,1] * X[,1]      # "*" element-wise multiplication, matrix multiplication is "%*%"
b = X[,2] * a
c = sum(b)
return(c)
}

X1 = cbind( c(2,4,6,8), c(0.1,0.2,0.4,0.3) )
gadilijiang(X1)
X2 = cbind( c(0,3,6,9,12), c(0.20,0.10,0.30,0.15,0.25) )
gadilijiang(X2)



#Problem2
x = c(0,12,24,36,48,60,72)
y = c(141,127,141,163,145,179,161)

N = length(x)
SXX = sum((x-mean(x))^2); SXX
SXY = sum((x-mean(x))*(y-mean(y))); SXY
SYY = sum((y-mean(y))^2); SYY
beta1hat = SXY/SXX; beta1hat
beta0hat = mean(y)-beta1hat*mean(x); beta0hat

fit = lm(y ~ x)
fit
summary(fit)
names(fit)
names(summary(fit))

fit$fitted.values
fit$residuals
y-fit$fitted.values
summary(fit)$r.squared
summary(fit)$sigma

SXX = sum((x-mean(x))^2); SXX
SXY = sum((x-mean(x))*(y-mean(y))); SXY
SYY = sum((y-mean(y))^2); SYY
beta1hat = SXY/SXX; beta1hat
beta0hat = mean(y)-beta1hat*mean(x); beta0hat

sum(fit$residuals^2)
s2 = sum(fit$residuals^2)/(N-2); s2
s = sqrt(s2); s
R2 = 1- sum(fit$residuals^2)/SYY; R2
Radj2 = 1- (sum(fit$residuals^2)/(N-2))/(SYY/(N-1)); Radj2

confint(fit, level=0.95)

new <- data.frame(x=136)
predict.lm(fit,new,interval=c("confidence"),level=0.90)
predict.lm(fit,new,interval=c("prediction"),level=0.90)

plot(x,y)
abline(fit$coefficients)


#Problem3
x <- c(0.7,0.5,0.9,1.1,1.5,1.3)
y <- c(7.0,4.8,12.0,14.0,18.4,14.9)
fit = lm(y ~ 0+x)
summary(fit)
plot(x,y)
abline(a=0, b=fit$coefficients)  #a=intercept, b=slope, they are single values
