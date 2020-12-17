#Dimension Reduction
needed_packages <- c("psych",  "REdaS", "Hmisc", "corrplot", "ggcorrplot", "factoextra",  "nFactors","tidyverse")                      
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

