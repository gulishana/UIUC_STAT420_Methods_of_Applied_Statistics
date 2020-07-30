# Problem 1 d) 

GPA = c(1.8,0.9,1.5,2.4,2.4,3.3,3.9,3.6,2.1,2.4,3.0,3.9);GPA
Club = c(rep(1,4),rep(2,4),rep(3,4)); Club
College= c(rep(1:4,3)); College
results = glm(GPA ~ factor(Club) + factor(College))
summary(aov(results))

qf(0.95,2,6)
qf(0.95,3,6)

# Problem 2 e)

GPA = c(4.0,3.4,3.2,2.5,3.2,2.3,2.5,3.3,1.5,2.4,2.8,3.7,
        3.2,3.7,2.2,3.0,3.2,2.7,1.9,2.8,1.5,2.1,3.3,2.8);GPA
College= c(rep(1,6),rep(2,6),rep(3,6),rep(4,6)); College
Club = c( rep( c(1,1,2,2,3,3), 4) ); Club

results = glm(GPA ~ factor(Club) * factor(College))
summary(aov(results))

qf(0.95,6,12)
qf(0.95,2,12)
qf(0.95,3,12)
