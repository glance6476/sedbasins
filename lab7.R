# basics of visualizing distributions using R

#install.packages("Hmisc")
library(Hmisc)


# 
X = rnorm(1000, 3, 0.25)

plot(X)

hist(X)

summary(X)

