library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
#STEP 1: LOAD THE DATA INTO R
#STEP 2: MAKE SURE THE DATA MEETS THE ASSUMPTIONS
#SIMPLE LINEAR REGRESSION
# tells us the minimum, median, mean and max values of the
# independent variable(income) and dependent variable(happiness)
summary(income.data)
# Assumption: 1. Independence of observations(aka no autocorrelation)
# -We only have one independent variable and one dependent variable, we dont need to test for any hidden relationships among variables

#Assumption: 2.Normality: to check whether variable follows a normal distribution, use the hist() function
hist(income.data$happiness) #roughly bell-shaped(more observations in the middle of the distribution, fewer on the tails), can proceed with the linear regression

# Assumption 3: linearity: the relationship between the independent and dependent variable must be linear.test the visually with a scatter plot to see if the distribution of data points could be described with a straight line
plot(happiness ~ income, data = income.data)

#Assumption 4: homoscedasticity (aka homogeneity of variance)
#test this assumption later, after fitting the linear model
# MULTIPLE LINEAR REGRESSION
#Assumption 1: Independence of observations (aka no autocorrelation)
#use cor() to test the relationship between independent variables and make sure it's not too highly correlated

cor(heart.data$biking, heart.data$smoking) #the output is 0.015 means the CORRELATION between biking and smoking is small (0.015 is only a 1.5% correlation), so we can include both parameters in model

#Assumption 2: Normality
hist(heart.data$heart.disease)

#Assumption 3: Linearity
#biking and heart disease
plot(heart.disease ~ biking, data=heart.data)
#smoking and heart disease
plot(heart.disease ~ smoking, data=heart.data)

#Assumption 4: Homosdesticity
#check this after we make the model

#STEP 3: PERFORM THE LINEAR REGRESSION ANALYSIS
#Simple regression: income and happiness


income.happiness.lm <- lm(happiness ~ income, data = income.data)#makes the linear model

summary(income.happiness.lm)#prints out the summary of the model

#Multiple regression: biking, smoking, and heart disease
heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart.data)

summary(heart.disease.lm)
#This means that for every 1% increase in biking to work, there is a correlated 0.2% decrease in the incidence of heart disease. Meanwhile, for every 1% increase in smoking, there is a 0.178% increase in the rate of heart disease.

#Step 4: Check for homoscedasticity

#Simple regression
par(mfrow=c(2,2))#divide the plots window into the number of rows and columns
plot(income.happiness.lm)
par(mfrow=c(1,1))

#Multiple regression
par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))#to go back to plotting one graph in the entire window, set the parameters again replace the 2,2 to 1,1

#STEP 5: VISUALIZE THE RESULTS WITH A GRAPH

#Simple regression

dev.off()
#plot the data points on a graph
income.graph<-ggplot(income.data, aes(x=income, y=happiness))+ geom_point()
income.graph
#add the linear regression line to the plotted data
income.graph <- income.graph + geom_smooth(method="lm", col="black")
income.graph
#add the equations for the regression line

income.graph <- income.graph + stat_regline_equation(label.x = 3, label.y = 7)

income.graph
#make the graph ready for publication

income.graph + theme_bw() +
labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")



#MULTIPLE REGRESSION- there are 7 steps need to follow
#Step 1:Create a new dataframe with the information needed to plot the model

plotting.data<-expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out=30),
  smoking=c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking)))

#Step2: Predict the values of heart disease based on your linear model
plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata=plotting.data)
#Step 3: Round the smoking numbers to two decimals
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)
#Step4: Change the ‘smoking’ variable into a factor
plotting.data$smoking <- as.factor(plotting.data$smoking)
#Step5:Plot the original data
heart.plot <- ggplot(heart.data, aes(x=biking, y=heart.disease)) + geom_point()

heart.plot

#Step 6: Add the regression lines
heart.plot <- heart.plot + geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)

heart.plot

#Step6: Make the graph ready for publication
heart.plot <-
  heart.plot +
  theme_bw() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)")

heart.plot


heart.plot + annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")

#References: https://www.scribbr.com/statistics/linear-regression-in-r/