#####Correlations

##Run some correlations and report the results using the provided code.

x <- c(1,2,3)
y <- c(1,3,2)
plot(x,y)
cor(x,y)

x <- c(1,2,3)
y <- c(3,1,2)
plot(x,y)
cor(x,y)

round( cor(iris[,1:4]), 2 )
round( cor(iris[,1:3]), 2 )


wood <- rnorm(24)
heat <- rnorm(24)
cor.test(wood,heat)

install.packages("BayesFactor")
library("BayesFactor")

bfCorTest <- function (x,y) # Get r from BayesFactor
{
  zx <- scale(x) # Standardize X
  zy <- scale(y) # Standardize Y
  zData <- data.frame(x=zx,rhoNot0=zy) # Put in a data frame
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  print(summary(mcmcOut[,"rhoNot0"])) # Get the HDI for rho
  return(bfOut) # Return Bayes factor object
}

bfCorTest(iris[,"Sepal.Length"],iris[,"Sepal.Width"])
