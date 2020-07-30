## Problem 1
## (a)
ISEs=read.table(file="HW10_ISEs.dat",header=TRUE)
colnames(ISEs)=c("concentration","migration")
head(ISEs,n=3)
nrow(ISEs)
fit1=lm(migration~concentration,data = ISEs)
plot(ISEs$concentration,ISEs$migration,xlab = "Concentration(ppm)",ylab = "Migration(mV)")
abline(fit1)
plot(fitted(fit1),resid(fit1))
abline(h=0)

## (b)
fit2=lm(migration~concentration+I(concentration^2),data = ISEs)
anova(fit1,fit2)
plot(fitted(fit2),resid(fit2))
abline(h=0)

## (c)
plot(ISEs$concentration,ISEs$migration,xlab = "Concentration(ppm)",ylab = "Migration(mV)")
abline(fit1,lty=2)
xplot=seq(0,200,by=0.1)
lines(xplot, predict(fit2,newdata=data.frame(concentration=xplot)),lwd=2)


## Probelm 2
library(faraway)
data(odor)
nrow(odor)
odor[1:3,]
save(odor,file="HW10_odor.Rdata")

## (a)
fit=lm(odor~temp+gas+pack+I(temp^2)+I(gas^2)+I(pack^2),data=odor)
summary(fit)

## (b)
names(summary(fit))
summary(fit)$r.squared

## (c)
summary(fit)$adj.r.squared
extractAIC(fit)

## Problem 3
library(faraway)
data(prostate)
nrow(prostate)
prostate[1:3,]

fit=lm(lpsa~.,data=prostate)
summary(fit)

## (a)
library(broom)
n=length(resid(fit))

fit_bac_AIC=step(fit,direction = "backward");fit_bac_AIC
summary(fit_bac_AIC)
extractAIC(fit_bac_AIC)

fit_bac_BIC=step(fit,direction = "backward",k=log(n));fit_bac_BIC
summary(fit_bac_BIC)


## (b)
n=length(resid(fit))
fit_for_AIC=step(fit,direction = "forward");fit_for_AIC
summary(fit_for_AIC)

fit_for_BIC=step(fit,direction = "forward",k=log(n));fit_for_BIC
summary(fit_for_BIC)

## (c)
library(leaps)
all_fits=regsubsets(lpsa~.,data=prostate)
all_fits_sum=summary(all_fits)
all_fits_sum$which

# AIC
p=length(coef(fit))
n=length(resid(fit))
AIC=n*log(all_fits_sum$rss/n) + 2*(2:p)
AIC
which.min(AIC)

# BIC
BIC=n*log(all_fits_sum$rss/n) + log(n)*(2:p)
BIC
which.min(BIC)

# R2adj
R_adj=all_fits_sum$adjr2
R_adj
which.max(R_adj)


## (d)
fit1=lm(lpsa~.,data=prostate)
fit2=lm(lpsa~.-gleason,data=prostate)
fit3=lm(lpsa~lcavol+lweight+age+lbph+svi,data=prostate)
fit4=lm(lpsa~lcavol+lweight+svi,data=prostate)

rbind(
  summary(fit1)$adj.r.squared,
  summary(fit2)$adj.r.squared,
  summary(fit3)$adj.r.squared,
  summary(fit4)$adj.r.squared
)

rbind(
  extractAIC(fit1),
  extractAIC(fit2),
  extractAIC(fit3),
  extractAIC(fit4)
)

rbind(
  sum( (resid(fit1) / (1 - hatvalues(fit1)))^2 ),
  sum( (resid(fit2) / (1 - hatvalues(fit2)))^2 ),
  sum( (resid(fit3) / (1 - hatvalues(fit3)))^2 ),
  sum( (resid(fit4) / (1 - hatvalues(fit4)))^2 )
)
