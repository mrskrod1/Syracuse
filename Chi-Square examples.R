data(rock)
cor.test(rock$area, rock$perm)
install.packages("BayesFactor")
library("BayesFactor")
bfCorTest<- funtion (x, y)    #Get r from Bayes Factor
{
  zx <- scale(x) # Standardize X
  zy <- scale(y) # Standardize Y
  zData <- data.frame(x=zx,rhoNot0=zy) # Put in a data frame
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  print(summary(mcmcOut[,"rhoNot0"])) # Get the HDI for r
  return(bfOut) # Return Bayes factor object
}
bfCorTest(rock$area, rock$perm)

UCBAdmissions[, , 1]
calcChiSquared<- function(actual, expected)
{
  diffs <- actual - expected        	# Take the raw difference for each cell
  diffsSq <- diffs ^ 2              	# Square each cell
  diffsSqNorm <- diffsSq / expected # Normalize with expected cells
 
  make2x2table <- function(ul) # The user supplies the count for the upper left cell
  {
    ll <- 313- ul # Calculate the lower left cell
    ur <- 512- ul # Calculate the upper right cell
    lr <- 19 - ur # Calculate the lower right cell
    
    # Put all of the cells into a 2x2 matrix
    matrix(c(ul,ur,ll,lr), nrow=2, ncol=2, byrow=TRUE)
  }
  
   
  sum(diffsSqNorm)                 	 # Return the sum of the cells
}
expectedValues <- matrix(c(512,89,313,19), nrow=2, ncol=2, byrow=TRUE)

calcChiSquared(make2x2table(512),expectedValues)
calcChiSquared(make2x2table(0),expectedValues)
calcChiSquared(make2x2table(825),expectedValues)

chiDist<-replicate(100000, calcChiSquared(make2x2table(rbinom(n=1,
size=825, prob=0.5)),expectedValues))

hist(chiDist)
calcChiSquared(make2x2table(512), expectedValues)
calcChiSquared(make2x2table(0), expectedValues)
chisq.test(make2x2table(512), correct=FALSE)

UCBAdmissionsMF <- ftable(UCBAdmissions, row.vars=2, col.vars="Admitted")
UCBAdmissionsMF
chisq.test(UCBAdmissionsMF, correct=FALSE)


ctBFout<- contingencyTableBF(make2x2table(512), sampleType = "poisson",
      posterior = FALSE)
ctBFout

ctMCMCout<- contingencyTableBF(make2x2table(512), 
    sampleType = "poisson", posterior = TRUE, iteration=10000)
summary(ctMCMCout)


set.seed(314)
badBoatMF <- ftable(Titanic, row.vars=2, col.vars="Survived")
badBoatMF
ctBFout <- contingencyTableBF(badBoatMF,sampleType="poisson",posterior=FALSE)
ctBFout

ctMCMCout <- contingencyTableBF(badBoatMF,sampleType="poisson",posterior=TRUE,iterations=10000)
summary(ctMCMCout)
maleProp <- ctMCMCout[,"lambda[1,1]"]/ctMCMCout[,"lambda[1,2]"]
mean(maleProp)
femaleProp <- ctMCMCout[,"lambda[2,1]"]/ctMCMCout[,"lambda[2,2]"]
mean(femaleProp)
diffProp <- maleProp - femaleProp
hist(diffProp)
mean(diffProp)
abline(v=quantile(diffProp,c(0.025)), col="black")
abline(v=quantile(diffProp,c(0.975)), col="black")
