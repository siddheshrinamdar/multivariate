#Reading the dataset
diabetes <- read.csv("C:/Users/siddh/Desktop/Rutgers Spring 19/MULTI/diabetes.csv")


#Lets have 0/1 as No/Yes

diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("No","Yes")

colnames(diabetes)[colnames(diabetes)=="Outcome"] <- "Diabetic"

library(PerformanceAnalytics)
library(ggplot2)
library(GGally)
library(Amelia)
library(ggcorrplot)
library(magrittr)
library(dplyr)
#library(plotly)
library(ggcorrplot)
library(ggplot2)
library(reshape2)

dim(diabetes)
attach(diabetes)
head(diabetes)

#Get the Correlations between the measurements
cor(diabetes[-9])
#Use of prcomp to compute the principal components(eigenvalues & eigenvectors). 
#With scale=TRUE, variable means are set to zero, and variances set to one
diabetes_pca = prcomp(diabetes[-9],scale=TRUE)
diabetes_pca
summary(diabetes_pca)
plot(diabetes_pca,type='l')   #VISUALIZE INDIVIDUAL COMPONENTS elbow curve


# sample scores stored in diabetes_pca$x
# singular values (square roots of eigenvalues) stored in diabetes_pca$sdev
# loadings (eigenvectors) are stored in diabetes_pca$rotation
# variable means stored in diabetes_pca$center
# variable standard deviations stored in diabetes_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2

attributes(diabetes_pca)     
diabetes_pca$rotation       #checking how individual components contribute to the percentage
diabetes_pca$sdev

(eigen_diabetes = diabetes_pca$sdev^2)
names(eigen_diabetes) = paste("PC",1:3,sep="")
eigen_diabetes
sumlambdas = sum(eigen_diabetes)
sumlambdas
propvar = eigen_diabetes/sumlambdas #explains the variance or %
propvar
cumvar_diabetes = cumsum(propvar)
cumvar_diabetes
matlambdas = rbind(eigen_diabetes,propvar,cumvar_diabetes)  #merge the datasets vertically

rownames(matlambdas) = c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(diabetes_pca)
diabetes_pca$rotation
print(diabetes_pca)


#scree plot
plot(eigen_diabetes, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
eigen_diabetes
