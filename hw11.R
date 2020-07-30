## Probelm 2
library(faraway)
data(odor)
nrow(odor)
odor[1:3,]

## (a)
fit1=lm(odor~temp+gas+pack+I(temp^2)+I(gas^2)+I(pack^2)
       +I(temp*gas)+I(temp*pack)+I(gas*pack),data=odor)
summary(fit1)

## (a)
fit2=lm(odor~temp+gas+pack+I(temp^2)+I(gas^2)+I(pack^2),data=odor)
anova(fit2,fit1)
