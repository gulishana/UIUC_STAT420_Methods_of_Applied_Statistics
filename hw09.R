# Problem 1
library(faraway)
data(longley)
nrow(longley)
longley[1:3,]
attach(longley)

# can save table file and easily use it next time
save(longley,file="HW9_longley.Rdata")
load("HW9_longley.Rdata")
attach(longley)


### a)
options(digits=3)
round(cor(longley),2)

### b)
pairs(longley)

### c)
options(digits=5)
fit = lm(Employed~.,data = longley)
vif(fit)


### d)
fit1 = lm(Employed~.-Population,data = longley)
fit2 = lm(Population~.-Employed,data = longley)
cor(fit2$residuals,fit1$residuals)

plot(fit2$residuals, fit1$residuals)
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(lm(fit1$residuals ~ fit2$residuals))


### e)
summary(fit)
fit_new = lm(Employed~Unemployed+Armed.Forces+Year)
vif(fit_new)


### f)
anova(fit_new,fit)


# Problem 2
library(MASS)
data(mammals)
nrow(mammals)
mammals[1:3,]
attach(mammals)

# can save table file and easily use it next time
save(mammals,file="HW9_mammals.Rdata")
load("HW9_mammals.Rdata")
attach(mammals)


### a)
plot(mammals$body,mammals$brain)

### b)
fit = lm(brain~log(body),data = mammals)
boxcox(fit,plotit = TRUE)
boxcox(fit,plotit = TRUE,lambda = seq(-0.1,0.1,by=0.001))

### c)
plot(log(mammals$body),log(mammals$brain))
fit_log = lm(log(brain)~log(body),data = mammals)
abline(fit_log)

new=data.frame(body=254)
predict.lm(fit_log,new,interval=c("prediction"),level=0.95)
exp(predict.lm(fit_log,new,interval=c("prediction"),level=0.95))

new=data.frame(body=2300)
predict.lm(fit_log,new,interval=c("prediction"),level=0.95)
exp(predict.lm(fit_log,new,interval=c("prediction"),level=0.95))
