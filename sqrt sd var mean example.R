install.packages("modeest")
library(modeest)
mfv(discoveries)
votes<- c(200, 300, 400)
(votes - mean(votes))^2
sum( (votes-mean(votes)) ^2)
sum( (votes - mean(votes)) ^2) / length(votes)


votes1<- c(200, 300, 400)
votes2<- c(299, 300, 301)


sqrt( sum((votes1- mean(votes1)) ^2) / length(votes1) )
sqrt( sum((votes2 - mean(votes2)) ^2) / length(votes2) )

sd(discoveries)
var(discoveries)
mean(discoveries)
range(discoveries)

hist(rnorm(n=125, mean=95, sd=5))
