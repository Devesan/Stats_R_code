rm(list = ls())
setwd('Desktop/Data Science Classes/Probablity and statistics/Portfolio/')
setwd('../../Probablity and statistics/Portfolio/Portfolio site')
needed_packages <- c("tidyverse",  "semTools", "pastecs", "psych","FSA","car","effectsize","userfriendlyscience",
                     "sjstats","gmodels","lm.beta","stargazer","ggplot2")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 
library('tidyverse')
library(semTools)
library(pastecs)
library(psych)
library(FSA) #For percentage
library(car) # For Levene's test for homogeneity of variance 
library(effectsize) #To calculate effect size for t-test
library(userfriendlyscience)
library('sjstats')
library(gmodels)
library(lm.beta) #Will allow us to isolate the beta co-efficients
library(stargazer)#For formatting outputs/tables
library(ggplot2)
s_perform<-read_csv('https://raw.githubusercontent.com/Devesan/Stats_R_code/main/Datasets/sperformance-dataset.csv')
summary(s_perform)
str(s_perform)

cdat <- s_perform %>% summarise_at(vars(mG1), funs(mean(., na.rm=TRUE)))
cdat
ggplot(s_perform,aes(x=mG1))+geom_histogram(binwidth = 0.5, alpha=1, position="identity") +
  geom_vline(data=cdat, aes(xintercept= mG1),
             linetype="dashed", size=1)
var_of_interest <- c('Pstatus','schoolsup.m','internet','higher.m','age','mG1','mG2','mG3')
summary(s_perform[var_of_interest])
unique(s_perform$Pstatus)
sd(s_perform$mG1)

# Histogram of MG2

gg <- ggplot(s_perform, aes(x=s_perform$mG2))

#Change the label of the x axis
gg <- gg + labs(x="maths grade 2")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of tpcoiss
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(s_perform$mG2, na.rm=TRUE), sd=sd(s_perform$mG2, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg


qqnorm(s_perform$mG2)
qqline(s_perform$mG2, col=2)


pastecs::stat.desc(s_perform$mG2, basic=F)

#We can make our decision based on the value of the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will indicate if we have a problem
mG2skew<-semTools::skew(s_perform$mG2)
mG2kurt<-semTools::kurtosis(s_perform$mG2)
mG2skew[1]/mG2skew[2]

mG2kurt[1]/mG2kurt[2]



#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#This will tell us how big a problem we have
# Calculate the percentage of standardised scores that are greated than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores, abs gets absolute value

zmG2<- abs(scale(s_perform$mG2))

FSA::perc(as.numeric(zmG2), 1.96, "gt")
FSA::perc(as.numeric(zmG2), 3.29, "gt")


#mG3

gg <- ggplot(s_perform, aes(x=s_perform$mG3))

#Change the label of the x axis
gg <- gg + labs(x="maths grade 3")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of tpcoiss
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(s_perform$mG3, na.rm=TRUE), sd=sd(s_perform$mG3, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg


qqnorm(s_perform$mG3)
qqline(s_perform$mG3, col=2)


pastecs::stat.desc(s_perform$mG3, basic=F)

#We can make our decision based on the value of the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will indicate if we have a problem
mG3skew<-semTools::skew(s_perform$mG3)
mG3kurt<-semTools::kurtosis(s_perform$mG3)
mG3skew[1]/mG3skew[2]

mG3kurt[1]/mG3kurt[2]



#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#This will tell us how big a problem we have
# Calculate the percentage of standardised scores that are greated than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores, abs gets absolute value

zmG3<- abs(scale(s_perform$mG3))
FSA::perc(as.numeric(zmG3), 1.96, "gt")
FSA::perc(as.numeric(zmG3), 3.29, "gt")



### CORRELATION
#### Scatterplot

#Simple scatterplot of feeling of control and perceived stress
#aes(x,y)
scatter <- ggplot(s_perform, aes(s_perform$mG2, s_perform$mG3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "mG2", y = "mG3") 

#### Conducting Correlation Tests - Pearson, Spearman, Kendall


#Pearson Correlation
stats::cor.test(s_perform$mG2, s_perform$mG3, method='pearson')

#If our data does not fit a normal distribution we would report either Spearman or Kurtosis
#All tests are included here for completeness of explanation - in reality you would only include the most appropriate test.
#Spearman Correlation
#Change the method to be spearman.
#This test will give an error since this method uses ranking but cannot handle ties but that is ok with us - we consider this to be missing data
cor.test(s_perform$mG2, s_perform$mG3, method = "spearman")

#We can also use kendall's tau which does handle ties
cor.test(s_perform$mG2, s_perform$mG3, method = "kendall")



#Research question 2

#Get descriptive stastitics by group - output as a matrix
psych::describeBy(s_perform$mG3, s_perform$higher.m, mat=TRUE)

#Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we woudl expect probaility to not be statistically significant.
car::leveneTest(mG3 ~ higher.m, data=s_perform)
#Pr(>F) is your probability - in this case it is not statistically significant so we can assume homogeneity


#Conduct the t-test from package stats
#In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(mG3 ~ higher.m,var.equal=TRUE,data=s_perform)
#No statistically significant difference was found

res <- stats::t.test(mG3 ~ higher.m,var.equal=TRUE,data=s_perform)
#Calculate Cohen's d
#artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

#Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)


#Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes


#Question 3

#Get descriptive stastitics by group - output as a matrix
psych::describeBy(s_perform$mG3, s_perform$studytime.m, mat=TRUE)

#Conduct Bartlett's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(mG3~ studytime.m, data=s_perform)
#p value is > 0.05 so the result is not statistically significant so we can assume homogeneity


#Conduct ANOVA using the userfriendlyscience test oneway
#In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
#If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(s_perform$studytime.m),y=s_perform$mG3,posthoc='Tukey')
#No statistically significant difference was found

#use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(mG3~ studytime.m, data=s_perform)
#Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
#Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
#Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta
#Research Question 4
#Use the Crosstable function
#CrossTable(predictor, outcome, fisher = TRUE, chisq = TRUE, expected = TRUE)
gmodels::CrossTable(s_perform$famsize, s_perform$famsup.m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#more simplistic way of doing Chi-Square

#Create your contingency table
mytable<-xtabs(~famsup.m+famsize, data=s_perform)

ctest<-stats::chisq.test(mytable, correct=TRUE)#chi square test
#correct=TRUE to get Yates correction needed for 2x2 table

ctest#will give you the details of the test statistic and p-value
ctest$expected#expected frequencies
ctest$observed#observed frequencies
ctest$p.value
#Calculate effect size
sjstats::phi(mytable)
sjstats::cramer(mytable)


#Model
#Model1
scatter <- ggplot(s_perform, aes(s_perform$mG2, s_perform$mG3))
#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "mG2", y = "mG3") 


#Pearson Correlation
stats::cor.test(s_perform$mG2, s_perform$mG3, method='pearson')

#Building linear model using mG1,mG2 as predictors and mG3 as the dependent variable
s_perform$mG3<-na_if(s_perform$mG3,0)
model1<-lm(s_perform$mG3~s_perform$mG2)
anova(model1)
summary(model1)
lm.beta::lm.beta(model1)
stargazer(model1, type="text") #Tidy output of all the required stats
#as.factor(s_perform$sex)

#Assess how model meets key assumptions of linear regression

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model1))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#finding rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
influential
stem(influential)
head(s_perform[influential, ])  # influential observations.
head(s_perform[influential, ]$mG2)  # influential observations - look at the values of mG1
head(s_perform[influential, ]$mG3)  # influential observations - look at the values of mG3

car::outlierTest(model1) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model1) # leverage plots

#Assess homocedasticity 
plot(model1,1)
plot(model1, 3)
#The first plot is the chart of residuals vs fitted values, in the second plot the standardised residuals are on the Y axis. If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line. We reall want to see that there is no pattern in the residuals and that they are equally spread around the y = 0 line - the dashed line.
#n our case, as you can notice the red line is slightly distorted in the middle on plot 1 but is not a big problem. Looking at the second plot we can see that while it is a problem it is not huge. Only a concern if there are definite patterns.

#Create histogram and  density plot of the residuals
plot(density(resid(model1))) 

#Create a QQ plotqqPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(model1, main="QQ Plot") #qq plot for studentized resid

#Calculate Collinearity
vifmodel<-car::vif(model1)
vifmodel
#Calculate tolerance
1/vifmodel


#Model 2
model2<-lm(s_perform$mG3~s_perform$mG2+s_perform$higher.m)
anova(model2)
summary(model2)
stargazer(model2, type="text") #Tidy output of all the required stats
lm.beta(model2)
stargazer(model1, model2, type="text") 

#Check assumptions

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model2))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


#find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)
head(s_perform[influential, ])  # influential observations.
head(s_perform[influential, ]$mG2)  # influential observations - look at the values of mG2
head(s_perform[influential, ]$mG3)  # influential observations - look at the values of mG3
head(s_perform[influential, ]$higher.m)  # influential observations -look at the values of higher


car::outlierTest(model2) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model2) # leverage plots

#Assess homocedasticity 
plot(model2,1)
plot(model2, 3)
#This is a much better result than model 1

#A density plot of the residuals
plot(density(resid(model2))) 

#Create a QQ plot qPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(model2, main="QQ Plot Model 2") #qq plot for studentized resid

#Collinearity
vifmodel<-car::vif(model2)
vifmodel
#Tolerance
1/vifmodel

 
#Model 3
#interaction effect
#create interaction term - adding a new variable to the dataset inthighmG2
s_perform$inthigher.m <- ifelse(s_perform$higher.m=='yes',1,0)
s_perform$inthigher.m
#s_perform$mG2<-na_if(s_perform$mG2,0)
#s_perform$mG2
s_perform$inthighmG2<-as.numeric(s_perform$inthigher.m)*s_perform$mG2
s_perform$inthighmG2
model3 <-lm(s_perform$mG3~s_perform$mG2+s_perform$higher.m+s_perform$inthighmG2)
anova(model3)
summary(model3)
stargazer(model3, type="text") #Tidy output of all the required stats
lm.beta(model3)
stargazer(model2, model3, type="text") 

#Check assumptions

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model3))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


#find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)
head(s_perform[influential, ])  # influential observations.
head(s_perform[influential, ]$mG2)  # influential observations - look at the values of mG2
head(s_perform[influential, ]$mG3)  # influential observations - look at the values of mG3
head(s_perform[influential, ]$higher.m)  # influential observations -look at the values of higher
head(s_perform[influential, ]$inthighmG2)  # influential observations -look at the values of interaction var


car::outlierTest(model3) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model3) # leverage plots

#Assess homocedasticity 
plot(model3,1)
plot(model3, 3)

#A density plot of the residuals
plot(density(resid(model3))) 

#Create a QQ plot qPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(model3, main="QQ Plot Model 3") #qq plot for studentized resid

#Collinearity
vifmodel<-car::vif(model3)
vifmodel
#Tolerance
1/vifmodel

#==============================================================================

#Logistical Model
install.packages('Epi')
install.packages('generalhoslem')
install.packages('regclass')
install.packages('DescTools')
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






#===================================================
#Dimension Reduction
needed_packages <- c("psych",  "REdaS", "Hmisc", "corrplot", "ggcorrplot", "factoextra",  "nFactors")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 
library(psych)
library(REdaS)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(factoextra)#Used for principal component analysis to get a different view of eigenvalues
library(nFactors)

dim_data <- read_csv('https://raw.githubusercontent.com/Devesan/Stats_R_code/main/Datasets/studentpIusepersonality.csv')
str(dim_data)
d_data<-dim_data %>% select(matches("^D[0-9]+$"))

e_data<- dim_data %>% select(matches("^E[0-9]+$"))
e_data
corr_df <- cbind(d_data,e_data)
corr_df
#create a correlation matrix (these are just some methods)
raqMatrix<-cor(corr_df)
round(raqMatrix, 2)
Hmisc::rcorr(as.matrix(corr_df))

#Using ggcorrplot. Note these are examples you need to choose a style for yourself, you do not need to create multiple correlation matrices
p.mat <- ggcorrplot::cor_pmat(corr_df)
#Showing lower diagonal
ggcorrplot::ggcorrplot(raqMatrix, title = "Correlation matrix for  data", p.mat = p.mat, sig.level = .05, type="lower")


#Overlay plot with a white grid to space things out.
#t1.cex is the text size, pch is controlling what is shown for non-significant correlations
ggcorrplot::ggcorrplot(raqMatrix, sig.level=0.05, lab_size = 4.5, p.mat = NULL,
           insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 10) +
  theme(axis.text.x = element_text(margin=margin(-2,0,0,0)),
        axis.text.y = element_text(margin=margin(0,-2,0,0)),
        panel.grid.minor = element_line(size=10)) + 
  geom_tile(fill="white") +
  geom_tile(height=0.8, width=0.8)


#Showing the co-coefficients (this will be messy given the number of variables)
ggcorrplot::ggcorrplot(raqMatrix, lab=TRUE, title = "Correlation matrix for RAQ data",  type="lower")

##Step 2: Check if data is suitable - look at the relevant Statistics
###Bartlett's test
psych::cortest.bartlett(raqMatrix, n=nrow(corr_df))

###KMO :
psych::KMO(corr_df)

###Determinant 
det(cor(corr_df))

##Step 3: Do the Dimension Reduction  (PRINCIPAL COMPONENTS ANALYSIS)
pc1 <-  principal(corr_df, nfactors = 19, rotate = "none")
pc1 <-  principal(corr_df, nfactors = length(corr_df), rotate = "none")
pc1#output all details of the PCA


##Step 4: Decide which components to retain (PRINCIPAL COMPONENTS ANALYSIS)
#Create the scree plot
plot(pc1$values, type = "b") 
#Print the variance explained by each component
pc1$Vaccounted 
#Print the Eigenvalues
pc1$values

#Another way to look at eigen values plus variance explained (need to use princomp function of PCA to get right class for use with factoextra functions)
pcf=princomp(corr_df)
factoextra::get_eigenvalue(pcf)
factoextra::fviz_eig(pcf, addlabels = TRUE, ylim = c(0, 50))#Visualize the Eigenvalues
factoextra::fviz_pca_var(pcf, col.var = "black")
factoextra::fviz_pca_var(pcf, col.var = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         repel = TRUE # Avoid text overlapping
)
#Print the loadings above the level of 0.3
psych::print.psych(pc1, cut = 0.3, sort = TRUE)
#create a diagram showing the components and how the manifest variables load
fa.diagram(pc1) 
#Show the loadings of variables on to components
fa.sort(pc1$loading)
#Output the communalities of variables across components (will be one for PCA since all the variance is used)
pc1$communality 
#Visualize contribution of variables to each component
var <- factoextra::get_pca_var(pcf)
corrplot::corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
factoextra::fviz_contrib(pcf, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
factoextra::fviz_contrib(pcf, choice = "var", axes = 2, top = 10)


#Step 5: Rotation
#Apply rotation to try to refine the component structure
pc2 <-  principal(corr_df, nfactors = 4, rotate = "varimax")#Extracting 4 factors
#output the components
psych::print.psych(pc2, cut = 0.3, sort = TRUE)
#output the communalities
pc2$communality

#Step3,4 
#Factor Analysis - the default here is principal axis factoring fm=pa
#If we know our data going in is normally distributed we use maximum likelihood
facsol <- psych::fa(raqMatrix, nfactors=4, obs=NA, n.iter=1, rotate="varimax", fm="pa")

#Create your scree plot
plot(facsol$values, type = "b") #scree plot

#Print the Variance accounted for by each factor/component
facsol$Vaccounted
#Output the Eigenvalues
facsol$values 

#Print the components with loadings
psych::print.psych(facsol,cut=0.3, sort=TRUE)

#Print sorted list of loadings
fa.sort(facsol$loading)

#create a diagram showing the factors and how the manifest variables load
fa.diagram(facsol)

#Step 6
corr_df
d_data
e_data

#Output our d Alpha values
psych::alpha(d_data)
psych::alpha(e_data)

#If some items are to be reversed keyed, then either recode or get alpha to reverse code as needed by setting check.keys=TRUE (be careful with this - make sure you know it makes sense)
psych::alpha(e_data, check.keys=TRUE)

