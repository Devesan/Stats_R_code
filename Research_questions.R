needed_packages <- c("tidyverse",  "semTools", "pastecs", "psych","FSA","car","effectsize","userfriendlyscience",
                     "sjstats","gmodels","lm.beta","stargazer","ggplot2")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 



#Research Question 1
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

