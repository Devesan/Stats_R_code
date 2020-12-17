needed_packages <- c("tidyverse",  "semTools", "pastecs", "psych","FSA","car","effectsize","userfriendlyscience",
                     "sjstats","gmodels","lm.beta","stargazer","ggplot2")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 

s_perform<-read_csv('https://raw.githubusercontent.com/Devesan/Stats_R_code/main/Datasets/sperformance-dataset.csv')

str(s_perform)

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
