rm(list = ls());
getwd();
setwd('Desktop/Data Science Classes/Probablity and statistics/Project')
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
library(xlsx)
library(readxl)
excel_sheets('data_academic_performance.xlsx')
s_perform<-read_excel('data_academic_performance.xlsx',sheet = 'SABER11_SABERPRO')
head(s_perform)
summary(s_perform)
str(s_perform)

colnames(s_perform) <- tolower(colnames(s_perform))

#-============================SECTION 1==================================
#========SUMMARY STATISTICS OF VAR OF INTEREST
var_of_interest <- c('gender','revenue','school_nat','cr_pro','qr_pro','g_sc')
summary(s_perform[var_of_interest])

#========VISUALISATION 

#gender
ggp <- ggplot(s_perform, aes(x=gender))
ggp + geom_histogram(stat = 'count')

#revenue
ggp <- ggplot(s_perform, aes(x=revenue))
ggp + geom_histogram(stat = 'count')

#school_nat
ggp <- ggplot(s_perform, aes(x=school_nat))
ggp + geom_histogram(stat = 'count')

#school_type
ggp <- ggplot(s_perform, aes(x=school_type))
ggp + geom_histogram(stat = 'count')

#=================================SECTION 2=========================
#========Assessing Normality

#global score
gg <- ggplot(s_perform, aes(x=s_perform$g_sc))
gg <- gg + labs(x="Global ScoreL")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(s_perform$g_sc, na.rm=TRUE), sd=sd(s_perform$g_sc, na.rm=TRUE)))
gg

qqnorm(s_perform$g_sc)
qqline(s_perform$g_sc, col=2)

pastecs::stat.desc(s_perform$g_sc, basic=F)

g_scskew<-semTools::skew(s_perform$g_sc)
g_sckurt<-semTools::kurtosis(s_perform$g_sc)
g_scskew[1]/g_scskew[2]

g_sckurt[1]/g_sckurt[2]

g_sc<- abs(scale(s_perform$g_sc))

FSA::perc(as.numeric(g_sc), 1.96, "gt")
FSA::perc(as.numeric(g_sc), 3.29, "gt")


#cr_pro
gg <- ggplot(s_perform, aes(x=s_perform$cr_pro))
gg <- gg + labs(x="CRITICAL REASONING IN HIGH SCHOOL")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(s_perform$cr_pro, na.rm=TRUE), sd=sd(s_perform$cr_pro, na.rm=TRUE)))
gg

qqnorm(s_perform$cr_pro)
qqline(s_perform$cr_pro, col=2)

pastecs::stat.desc(s_perform$cr_pro, basic=F)

cr_proskew<-semTools::skew(s_perform$cr_pro)
cr_prokurt<-semTools::kurtosis(s_perform$cr_pro)
cr_proskew[1]/cr_proskew[2]

cr_prokurt[1]/cr_prokurt[2]

cr_pro<- abs(scale(s_perform$cr_pro))

FSA::perc(as.numeric(cr_pro), 1.96, "gt")
FSA::perc(as.numeric(cr_pro), 3.29, "gt")

#quantitative reasoning
gg <- ggplot(s_perform, aes(x=s_perform$qr_pro))
gg <- gg + labs(x="Quanttitative Reasoning")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(s_perform$qr_pro, na.rm=TRUE), sd=sd(s_perform$qr_pro, na.rm=TRUE)))
gg

qqnorm(s_perform$qr_pro)
qqline(s_perform$qr_pro, col=2)

pastecs::stat.desc(s_perform$qr_pro, basic=F)

qr_proskew<-semTools::skew(s_perform$qr_pro)
qr_prokurt<-semTools::kurtosis(s_perform$qr_pro)
qr_proskew[1]/qr_proskew[2]

qr_prokurt[1]/qr_prokurt[2]

qr_pro<- abs(scale(s_perform$qr_pro))

FSA::perc(as.numeric(qr_pro), 1.96, "gt")
FSA::perc(as.numeric(qr_pro), 3.29, "gt")



#=========== Missing Values

#Finding Null Values (% of null values)
na_count <-lapply(s_perform, function(y) round((sum(length(which(is.na(y)))))/length(y)*100))
na_count <- data.frame(na_count)
na_count


View(filter(s_perform, s_perform$school_type =='Not apply'))
s_perform <- filter(s_perform, s_perform$school_type !='Not apply')
s_perform

#=================================SECTION 3==========================
#=============SECTION 3.1
#=============correlation and t-test
#====================== Global score and quantitative reasoning


scatter <- ggplot(s_perform, aes(s_perform$g_sc, s_perform$qr_pro))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "G_Score", y = "QR_Pro") 
stats::cor.test(s_perform$g_sc, s_perform$qr_pro, method='pearson')


#====================== Global score and Critical reading


scatter <- ggplot(s_perform, aes(s_perform$g_sc, s_perform$cr_pro))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "G_score", y = "QR_Pro") 
stats::cor.test(s_perform$g_sc, s_perform$cr_pro, method='pearson')


#======================= T-test for differential variable

psych::describeBy(s_perform$g_sc, s_perform$school_nat, mat=TRUE)

car::leveneTest(g_sc ~ school_nat, data=s_perform)
stats::t.test(g_sc ~ school_nat,var.equal=FALSE,data=s_perform)

res <- stats::t.test(g_sc ~ school_nat,var.equal=FALSE,data=s_perform)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes

#============SECTION 3.2(MODEL 1)
#Model 1
model1<-lm(s_perform$g_sc~s_perform$qr_pro+s_perform$cr_pro)
anova(model1)
summary(model1)
stargazer(model1, type="text") #Tidy output of all the required stats
lm.beta(model1)
stargazer(model1, model2, type="text") 

#=====ASSUMPTIONS
#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model1))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

car::outlierTest(model1) 
car::leveragePlots(model1) 

#Assess homocedasticity 
plot(model1,1)
plot(model1, 3)

plot(density(resid(model1))) 

car::qqPlot(model1, main="QQ Plot Model 2") 

#Collinearity
vifmodel<-car::vif(model1)
vifmodel
#Tolerance
1/vifmodel


#======SECTION 3.3(Model 2)

model2<-lm(s_perform$g_sc~s_perform$qr_pro+s_perform$cr_pro+s_perform$school_nat)
anova(model2)
summary(model2)
stargazer(model2, type="text") #Tidy output of all the required stats
lm.beta(model2)


#=====ASSUMPTIONS

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model2))
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

car::outlierTest(model2)
car::leveragePlots(model2) 

#Assess homocedasticity 
plot(model2,1)
plot(model2, 3)

plot(density(resid(model2))) 

car::qqPlot(model2, main="QQ Plot Model 2") #qq plot for studentized resid

#Collinearity
vifmodel<-car::vif(model2)
vifmodel
#Tolerance
1/vifmodel

#========SECTION 3.4(COMPARING MODELS)
stargazer(model1, model2, type="text") 