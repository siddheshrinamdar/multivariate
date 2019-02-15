diabetes <- read.csv("C:/Users/siddh/Desktop/Rutgers Spring 19/MULTI/diabetes.csv")

#columns
#Pregnancies: Number of times pregnant
#GlucosePlasma glucose concentration a 2 hours in an oral glucose tolerance test
#BloodPressureDiastolic blood pressure (mm Hg)
#SkinThicknessTriceps skin fold thickness (mm)
#Insulin2-Hour serum insulin (mu U/ml)
#BMIBody mass index (weight in kg/(height in m)^2)
#DiabetesPedigreeFunctionDiabetes pedigree function
#AgeAge (years)
#OutcomeClass variable (0 or 1) 268 of 768 are 1, the others are 0
head(diabetes)
structure(diabetes)

library(Amelia) #This library is used to plot missmap 
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(PerformanceAnalytics)
library(gridExtra)

# Correlation matrix
#This plot shows us correlation coeeficents of all the varaibles. 

data(diabetes)
corr <- round(cor(diabetes), 1)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower",  
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram", 
           ggtheme=theme_bw)
# From the plot, we can say that the variable Glucose has a higher impact on the Outcome variable. They are highly Co-rrelated. 
#Pregnancies and Age are strongly correlated with coeeficient value 0.54. 
#SkinThickness , BMI and Skinthickness and Insulin are positively correlated with coeeficient values 0.4. 


#Changing outcome from numerical to categorical varibale.
diabetes$Outcome<- is.factor(diabetes$Outome)
levels(diabetes$Outcome) <- c("No","Yes")


#correlation plot
#This plot shows the relationship between the variables. 
ggpairs(diabetes, aes(color=Outcome, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Correlation Plot of Variance(diabetes)")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
#From the box plots in the last segment, we see the variables Insulin, BloodPressure, and the DiabetesPedigreefunction 
#contain many outliers.

#stripchart is like scatter plots (or dot plots) of the given data. 
#It's like an alternative to boxplots when sample sizes are small and are also used to check outliers present in each variables
stripchart(diabetes$BloodPressure,
           main="Blood pressure levels",
           xlab="Pressure levels",
           ylab="",
           method="jitter",
           col="orange",
           pch=1)

#Densityplots 
plot1 = qplot(diabetes$Pregnancies, data = diabetes, geom = "density", fill = "red") 
plot2 = qplot(diabetes$Age, data = diabetes, geom = "density", fill = "red")
plot3 = qplot(diabetes$Glucose, data = diabetes, geom = "density", fill = "red")
plot4 = qplot(diabetes$BloodPressure, data = diabetes, geom = "density", fill = "red")
grid.arrange(plot1, plot2, plot3, ncol = 3)
#The density plot here shows the distribution of the data and if they are positively or negatively skewed. 

#Plots a missingness map showing where missingness occurs in the dataset

missmap(diabetes, main ="Missing values vs observed")

#No missing Values occured in our dataset.