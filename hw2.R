summary(diabetes)
head(diabetes)
structure(diabetes)

#Description : The diabetes dataset that has been taken from kaggle consists of different variable such as BMI, AGE,INSULIN, 
#SKIN THICKNESS, GLUCOSE,DIABETES PEDIGREE FUNCTION. We clean this data and apply different analysis types and visualizations
#to find out which factors of the variabes lead to Diabettes in a person.

#Lets have 0/1 as No/Yes

diabetes$Outcome <- as.factor(diabetes$Outcome)

levels(diabetes$Outcome) <- c("No","Yes")


#correlation plot
ggpairs(diabetes, aes(color=Outcome, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Correlation Plot of Variance(diabetes)")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram", 
           ggtheme=theme_bw)

#missmap
missmap(diabetes, main ="Missing values vs observed")



library(PerformanceAnalytics)  #importing the performance analytics library after installing it
chart.Correlation(diabetes[,-9], histogram=TRUE, col="grey10", pch=1, main="Chart Correlation of Variance") #plotting the correlation matrix

library(ggplot2)
library(GGally)

ggpairs(diabetes, aes(color=diabetes, alpha=1), lower=list(continuous="smooth"))+ theme_bw()+labs(title="Correlation Plot of Variance(diabetes)")+theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#scatter plot
attach(diabetes)
plot(DiabetesPedigreeFunction, Outcome, main="Scatterplot For Diabetes", 
     xlab="DegreeFunction ", ylab="Outcome", pch=19)