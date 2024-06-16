universalBank<- universal_bank
logit<- glm(PersonalLoan ~ Income + Family + CCAvg + Mortgage +SecuritiesAccount +
      CDAccount + Online + CreditCard + Age + Experience + Education, data=universalBank, binomial)
summary(logit)

logit2<- glm(PersonalLoan ~ Income + Family + CCAvg + CDAccount +
            Online + CreditCard + Education, data=universalBank, binomial)
summary(logit2)

logit3<- glm(PersonalLoan ~ Income + Family + CDAccount + Online + 
            CreditCard, data=universalBank, binomial)
summary(logit3)

probit<- glm(PersonalLoan ~ Income + Family + CCAvg + Mortgage + SecuritiesAccount +
        CDAccount + Online + CreditCard + Age + Experience + Education, data=universalBank, family=binomial(link="probit"))
summary(probit)

probit2<- glm(PersonalLoan ~ Income + Family + CCAvg + SecuritiesAccount +
            CDAccount + Online + CreditCard + Education, data=universalBank, family=binomial(link="probit"))
summary(probit2)

CCAvgSecuritiesAccount<- universalBank$CCAvg*universalBank$SecuritiesAccount

logitM<- glm(PersonalLoan ~ CCAvg + SecuritiesAccount + CCAvgSecuritiesAccount,
            data=universalBank, family=binomial(link= "logit"))
summary(logitM)

SecuritiesAccountCreditCard<- universalBank$SecuritiesAccount*universalBank$CreditCard

logitM2<- glm(PersonalLoan ~ SecuritiesAccount + CreditCard + SecuritiesAccountCreditCard,
             data=universalBank, family=binomial(link= "logit"))
summary(logitM2)

IncomeEducation<-universalBank$Income*universalBank$Education
logitM3<- glm(PersonalLoan ~ Income + Education + IncomeEducation,
             data=universalBank, family=binomial(link= "logit"))
summary(logitM3)

IncomeCCAvg<-universalBank$Income*universalBank$CCAvg

logitM4<-
glm(PersonalLoan ~ Income + CCAvg + IncomeCCAvg, data=universalBank, family=binomial(link = "logit"))
summary(logitM4)


probitM<- glm(PersonalLoan ~ Income + Education + IncomeEducation, data= universalBank,family = binomial(link = "probit"))
summary(probitM)

probitM2<- glm(PersonalLoan ~ Income + CCAvg + IncomeCCAvg, data= universalBank,family = binomial(link = "probit"))
summary(probitM2)

