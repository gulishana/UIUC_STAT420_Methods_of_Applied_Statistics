x <- c(7.5,8.2,8.1,7.8,7.6,8.0,7.8,7.9,8.2)
mean(x)
sd(x)
t.test(x,mu=8,alternative=c("less"),conf.level=0.9)