?USJudgeRatings

#2
newJudge= USJudgeRatings
#newJudge <- USJudgeRatings

#3
newJudge$retain <- newJudge$RTEN > median(newJudge$RTEN)
#1=Retain, 0=not retain
newJudge$retainB <- ifelse(newJudge$RTEN > median(newJudge$RTEN),1, 0)
#4
#Run summary() on retain , CONT, and INTG
summary(data.frame(newJudge$retain, newJudge$INTG, newJudge$retainB))
#newJudge.retain newJudge.INTG   newJudge.retainB
#Mode :logical   Min.   :5.900   Min.   :0.0000  
#FALSE:22        1st Qu.:7.550   1st Qu.:0.0000  
#TRUE :21        Median :8.100   Median :0.0000  
#                Mean   :8.021   Mean   :0.4884  
#                3rd Qu.:8.550   3rd Qu.:1.0000  
#                Max.   :9.200   Max.   :1.0000 

#5
glm1<- glm(retain~CONT + INTG, family ="binomial", data = newJudge)

summary(glm1)

#Call:
#  glm(formula = retain ~ CONT + INTG, family = "binomial", data = newJudge)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-2.09169  -0.26140  -0.00009   0.21278   2.33087  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -88.816     30.359  -2.926  0.00344 **
#  CONT           2.993      1.347   2.223  0.02625 * 
#  INTG           8.236      2.728   3.019  0.00254 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 59.587  on 42  degrees of freedom
#Residual deviance: 18.772  on 40  degrees of freedom
#AIC: 24.772

#Number of Fisher Scoring iterations: 7


#Two things to observe in logistic regression model output
#! The sign (+/-) of the coefficient.
#2 The statistical significance of the coefficent.

#7
#Provide a write up of your results,
#with an interpretation of the research question, "Can the number of contacts between a lawyer
# and a judge along with the lawyers rating of judicial intergrity