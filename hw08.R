# Problem 2
library(faraway)
data(package="faraway")
data(prostate)
nrow(prostate)
prostate[1:5,]
attach(prostate)

# can save table file and easily use it next time
save(prostate,file="prostate.Rdata")
load("prostate.Rdata")
attach(prostate)


### a)
fit = lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45) #Full model
summary(fit)

### b)
fit0 = lm(lpsa~1) # Null model: lpsa=beta0 + error
anova(fit0,fit) # anova(Nullmodel,Fullmodel)

### c)
#stupid way to calculate parameter interval
beta_age_hat = -0.019637
beta_age_stderr = 0.011173
df = 97 - 10
#90% Confidence Interval
left = beta_age_hat - qt(0.95,df)*beta_age_stderr; left
right = beta_age_hat + qt(0.95,df)*beta_age_stderr; right
#95% Confidence Interval
left = beta_age_hat - qt(0.975,df)*beta_age_stderr; left
right = beta_age_hat + qt(0.975,df)*beta_age_stderr; right

#smart way to calculate parameter interval
confint(fit,"age",level=0.90)
confint(fit,"age",level=0.95)


### d)
plot(fit$fitted.values,fit$residuals)
abline(h = 0)

library(lmtest)
bptest(fit)


### e)
hist(fit$residuals)

histNorm <- function(x, densCol = "darkblue"){
  m <- mean(x)
  std <- sqrt(var(x))
  h <- max(hist(x,plot=FALSE)$density)
  d <- dnorm(x, mean=m, sd=std)
  maxY <- max(h,d)
  hist(x, prob=TRUE,
       xlab="x", ylim=c(0, maxY),
       main="Histogram with Normal Density Curve")
  curve(dnorm(x, mean=m, sd=std),
        col=densCol, lwd=2, add=TRUE)
}
histNorm(fit$residuals, "orange")

qqnorm(fit$residuals)
qqline(fit$residuals)

shapiro.test(fit$residuals)



### f)
diag_H = hatvalues(fit)
diag_H
sum(diag_H)
2*mean(diag_H) # = 2*9/97
diag_H[which(diag_H>2*mean(diag_H))]
diag_H[diag_H>2*mean(diag_H)]
sum(diag_H>2*mean(diag_H))

# or calculate H 
Y = prostate[,"lpsa"]
X = cbind(rep(1,97), prostate[,1:8])
X[1:5,]
X = as.matrix(X)
H = X %*% solve(t(X)%*%X) %*% t(X)
diag(H)
sum(diag(H))
2*mean(diag(H)) # = 2*9/97
diag(H)[diag(H)>2*mean(diag(H))]
sum(diag(H)>2*mean(diag(H)))




### g)
stu_r = rstudent(fit)
stu_r
hist(stu_r)
n = length(stu_r)
p = length(fit$coefficients)
df = n-p-1
alpha = 0.05 ## suppose use significance level 5% to perform the test
max(abs(stu_r))

# without Bonferroni adjustment
t = qt(1-alpha/2,df)
sum(abs(stu_r)>t)
stu_r[abs(stu_r)>t]


# with Bonferroni adjustment
t_B = qt(1-(alpha/2)/n,df)
sum(abs(stu_r)>t_B)
stu_r[abs(stu_r)>t_B]

# or use package"car"
library(car)
outlierTest(fit)

# compare with fdr
pvals = pt(-abs(stu_r),df)*2
p_Bonferroni = p.adjust(pvals,method = "bonferroni")
p_FDR = p.adjust(pvals,method = "fdr")
cbind(pvals,p_Bonferroni,p_FDR)




### h)
cook = cooks.distance(fit)
sum(cook>4/n)
cook[cook>4/n]




### i)
# remove predictors with p-value > 0.05
fit_new = lm(lpsa~lcavol+lweight+svi)
anova(fit_new,fit)

