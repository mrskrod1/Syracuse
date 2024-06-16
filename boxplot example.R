mtcars
mean(mtcars$mpg[mtcars$am ==0])     #Automatic transmissions
mean(mtcars$mpg[mtcars$am --1])      #Manual transmissions

#Standard Deviation
sd(mtcars$mpg[mtcars$am ==0])     #Automatic transmission
sd(mtcars$mpg[mtcars$am ==1])     #Manual Transmission

#Boxplot
boxplot(mpg ~ am, data=mtcars)  #Boxplot of mpg, grouped by am

mean(sample(mtcars$mpg[mtcars$am ==0], size=19,replace=TRUE))
mean(sample(mtcars$mpg[mtcars$am ==1], size=13,replace=TRUE))

mean(sample(mtcars$mpg[mtcars$am ==0], size=19, replace=TRUE))-
  mean(sample(mtcars$mpg[mtcars$am ==1], size=13, replace=TRUE))

meansDiffs<- replicate(100,mean(sample(mtcars$mpg[mtcars$am ==0],
size=19, replace=TRUE))- mean(sample(mtcars$mpg[mtcars$am ==1],
size=13, replace=TRUE)))

hist(meansDiffs)

t.test(mtcars$mpg[mtcars$vs==0],mtcars$mpg[mtcars$vs==1])

quantile(meansDiffs, c(0.025, 0.975))

t.test(mtcars$mpg[mtcars$am ==0], mtcars$mpg[mtcars$am ==1])
mean(sample(mtcars$mpg[mtcars$am == 0 ],size=19,replace=TRUE)
     

t.test(mtcars$mpg[mtcars$vs==0],mtcars$mpg[mtcars$vs==1])
 
carsBEST<-BESTmcmc(mtcars$mpg[mtcars$am==0],
    mtcars$mpg[mtcars$am==1])
plot(carsBEST)


set.seed(10)
pgrp1 <- sample(precip,20, replace=TRUE)
pgrp2 <- sample(precip,20, replace=TRUE)
pgrp3 <- sample(precip,20, replace=TRUE)
v1 <- var(precip)
v2 <-var(c(pgrp1,pgrp2,pgrp3))
pgrp3 <- pgrp3-5
v3 <- var(c(pgrp1,pgrp2,pgrp3))
barplot(c(v1,v2,v3),names.arg=c("Original Data","3 Samples","3rd Grp+5"))


set.seed(10)  
precipAmount <- sample(precip,60,replace=TRUE)
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20))
precipDF <- data.frame(precipAmount, precipGrp)
precipOut <- aov(precipAmount ~ precipGrp, data=precipDF)
summary(precipOut) 

# What does the next line do?
precipDF$precipAmount[precipDF$precipGrp==3] <- precipDF$precipAmount[precipDF$precipGrp==3] - 7
precipOut <- aov(precipAmount ~ precipGrp, data=precipDF)
summary(precipOut)

install.packages("BayesFactor")
library("BayesFactor")
set.seed(10)  
precipAmount <- sample(precip,60,replace=TRUE)
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20))
precipDF <- data.frame(precipAmount, precipGrp)
precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut,iterations=10000)
summary(mcmcOut)

precipDF$precipAmount[precipDF$precipGrp==3] <- precipDF$precipAmount[precipDF$precipGrp==3] - 7
precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut,iterations=10000)
summary(mcmcOut)





    