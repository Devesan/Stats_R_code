needed_packages <- c("Epi",  "generalhoslem", "regclass", "DescTools")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 

#Logistical Model

library(Epi)#ROC Curve
library(generalhoslem)#Needed to test assumption of linearity
library("regclass")#For confusion matrix
library(DescTools)

#Make sure categorical data is used as factors

logmodel1 <- glm(as.factor(higher.m) ~ as.factor(sex), data = s_perform, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel1)

#Chi-square plus significance
lmtest::lrtest(logmodel1)


#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=as.factor(s_perform$higher.m) ~as.factor(s_perform$sex), plot="ROC")

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel1, which="CoxSnell")
DescTools::PseudoR2(logmodel1, which="Nagelkerke")

#Summary of the model with co-efficients
stargazer(logmodel1, type="text")


#Exponentiate the co-efficients
exp(coefficients(logmodel1))
## odds ratios and 95% CI 
cbind(Estimate=round(coef(logmodel1),4),
      OR=round(exp(coef(logmodel1)),4))

#Probability of answering yes when female 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0)#YES this is the same as just having the 1st co-efficient
#Probability of answering yes when male 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1)

generalhoslem::logitgof(as.factor(s_perform$higher.m), fitted(logmodel1))


####### Model 2
#Make sure categorical data is used as factors

logmodel2 <- glm(as.factor(higher.m) ~ as.factor(sex)+as.factor(romantic.m), data = s_perform, na.action = na.exclude, family = binomial(link=logit))


#Full summary of the model
summary(logmodel2)

#Chi-square plus significance
lmtest::lrtest(logmodel2)

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel2, which="CoxSnell")
DescTools::PseudoR2(logmodel2, which="Nagelkerke")

#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=as.factor(s_perform$higher.m) ~as.factor(s_perform$sex)+as.factor(s_perform$romantic.m), plot="ROC")

#Summary of the model with co-efficients
stargazer(logmodel2, type="text")

#Exponentiate the co-efficients
exp(coefficients(logmodel2))
## odds ratios 
cbind(Estimate=round(coef(logmodel2),4),
      OR=round(exp(coef(logmodel2)),4))



#romantic.m 1 has a romantic relationship, 2 not having a romantic relationship
#1. Probability of answering yes when male and  having a relationship
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0)
#2. Probability of answering yes when female and having a relationship
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1)

#3.Probability of answering yes when male when not having a relationship
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*0)
#4. Probability of answering yes when female when not having a relationship
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*0)

#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok

generalhoslem::logitgof(as.factor(s_perform$higher.m), fitted(logmodel2))


#Collinearity
vifmodel<-car::vif(logmodel2)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
#Tolerance
1/vifmodel

