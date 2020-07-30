# Problem 3
# a)
x1 <- c(1,2,3,3,4,5)
x2 <- c(35,15,50,30,35,75)
Y <- c(17,20,11,14,14,8)
N = length(y)
X = cbind(rep(1,N),x1,x2)
X
C = solve( t(X) %*% X)
C
betahat = C %*% t(X) %*% Y
betahat

# b)
fit = lm(Y ~ x1+x2)
anova(lm(Y~1),fit)
summary(fit)   #also get F test result and others for parameters

# c)
summary(fit)
fita = lm(Y ~ x2) # or
anova(fita,fit)

# d)
beta2hat = -0.15000  # hat & stderr values all come from summary(fit)
beta2stderr = 0.04815
t2 = beta2hat/beta2stderr; t2
pt(t2,3,lower.tail=TRUE)

# e)
beta0hat = 22.70000
beta0stderr = 1.71809
t0 = (beta0hat-25)/beta0stderr; t0
pt(t0,3,lower.tail=TRUE)

# f)
beta1hat = -0.90000
beta1stderr = 0.69772
left = beta1hat - qt(0.975,3)*beta1stderr; left
right = beta1hat + qt(0.975,3)*beta1stderr; right

# g)
predict.lm(fit,data.frame(x1=5,x2=60),interval=c("prediction"),level=0.9)

# h)
SYY = sum((Y-mean(Y))^2)
RSS = sum(fit$residuals^2)
R2 = 1-RSS/SYY; R2
