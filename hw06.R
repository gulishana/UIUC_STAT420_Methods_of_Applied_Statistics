# Problem b) 

Drama = c(1.8,0.9,1.5,2.4)
Writing = c(2.4,3.3,3.9,3.6)
Statistics= c(2.1,2.4,3.0,3.9)

Clubs = c(rep("Drama",4),rep("Writing",4),rep("Statistics",4))
GPA = c(Drama,Writing,Statistics)

results = glm(GPA ~ factor(Clubs))
summary(aov(results))

# Or another way
Drama <- c(rep(1,4),rep(0,8)); Drama
Writing <- c(rep(0,4),rep(1,4),rep(0,4)); Writing
Statistics <- c(rep(0,8),rep(1,4)); Statistics

results2 = lm(GPA ~ Drama + Writing + Statistics)
summary(results2)

anova(lm(GPA ~ 1), results2)

# Problem e)
Drama = c(1.8,0.9,1.5,2.4)
Writing = c(2.4,3.3,3.9,3.6)
Statistics= c(2.1,2.4,3.0,3.9)

Clubs = c(rep("Drama",4),rep("Writing",4),rep("Statistics",4))
GPA = c(Drama,Writing,Statistics)

results = glm(GPA ~factor(Clubs))
TukeyHSD(aov(results))


# Problem g) 
qt(1-0.05/6, 9, lower.tail = TRUE)
qt(1-0.05/6, 9)
qt((1-0.05/6), 9)

# Problem j) 
Drama = c(1.8,0.9,1.5,2.4)
Writing = c(2.4,3.3,3.9,3.6)
Statistics= c(2.1,2.4,3.0,3.9)

Clubs = c(rep("Drama",4),rep("Writing",4),rep("Statistics",4))
GPA = c(Drama,Writing,Statistics)
kruskal.test(GPA ~ factor(Clubs))
