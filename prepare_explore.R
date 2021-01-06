needed_packages <- c("tidyverse",  "semTools", "pastecs", "psych","FSA","car","effectsize","userfriendlyscience",
                     "sjstats","gmodels","lm.beta","stargazer","ggplot2")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 




s_perform<-read_csv('https://raw.githubusercontent.com/Devesan/Stats_R_code/main/Datasets/sperformance-dataset.csv')
summary(s_perform)
str(s_perform)

na_count <-lapply(s_perform, function(y) round((sum(length(which(y==0))))/length(y)*100))
na_count <- data.frame(na_count)
na_count

factor(s_perform$Medu)
library(ggplot2)
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

