rm(list = ls())

#library installation

install.packages("moments")
library(ggplot2)
library(moments)
library(dplyr)
library("MASS")

#read csv

cars <- read.csv("cars.csv", sep=";")
View(cars)

#check first rows
head(cars)

# Number of column
ncol(cars)

# Number of row
nrow(cars)

# DATA CLEANING

# Check null value
sum(is.na(cars))

cars


#Check data type for each column

# Display the structure of the dataset
str(cars)

# Summary statistics of the dataset
summary(cars)

# Check for missing values
missing_values<- colSums(is.na(cars))
missing_values

#check unique values for each colum
checkUniqueValues <- function(data) {
  for (col in names(data)) {
    unique_values <- unique(data[[col]])
    print(paste("Unique values in column", col, ":"))
    print(unique_values)
    print("-----------------------------")
  }
}
checkUniqueValues(cars)

#Data visualization

#bar Plot
origin_bar <- cars%>%count(Origin)
origin_bar
barplot(origin_bar$n, col = "darkblue",xlab = "Origin",ylab="Frequency",main="Number of Origin", names.arg = c("Europe","Japan","US"))

model_bar <- cars%>%count(Model)
model_bar
barplot(model_bar$n, col = "darkblue",xlab = "Models",ylab="Frequency",main="Number of Model", names.arg = c("70's","71's","72's","73's","74's","75's","76's","77's","78's","79's","80's","81's","82's"))

cylinder_bar <- cars%>%count(Cylinders)
cylinder_bar
barplot(cylinder_bar$n, col = "darkblue",xlab = "Cylinder",ylab="Frequency",main="Number of Cylinder", names.arg = c("3", "4","5", "6","8"))

# Define the variables we want to use
variables <- c("MPG", "Displacement", "Horsepower", "Weight", "Acceleration")

#Boxplot

# Set up the plot layout
par(mfrow = c(3, 2))

# Create boxplots for each variable
for (variable in variables) {
  boxplot(cars[[variable]], col = "blue", xlab = variable, main = variable)
}

#Histogram

# Set up the plot layout
par(mfrow = c(3, 2))

# Create Histogram for each variable
for (variable in variables) {
  hist(cars[[variable]], col = "yellow", xlab = variable, main = variable, freq = TRUE)
}

#Density Plot

# Set up the plot layout
par(mfrow = c(3, 2))

# Create Density Plot for each variable
for (variable in variables) {
  hist(cars[[variable]], col = "yellow", xlab = variable, main = variable,freq = FALSE)
  lines(density(cars[[variable]]))
}



#check discriptive statistics of the data

cars_summary<-cars[variables]
summary(cars_summary)

#Check Skewness & Kurtosis
skewness(cars_summary)
kurtosis(cars_summary)

#Normality check 
#Q-Q plot for normality

# Set up the plot layout
par(mfrow = c(3, 2))

# Create Density Plot for each variable
for (variable in variables) {
  qqnorm(cars[[variable]], main = paste("Q-Q plot for", variable), xlab = "Theoretical Dist", ylab = variable, col = "steelblue")
  qqline(cars[[variable]], col = "red", lwd = 2, lty = 2)
}

#use Jarque.test for normalitty check
for (variable in variables) {
  result <- jarque.test(cars[[variable]])
  cat("Variable:", variable, "\n")
  print(result)
  cat("\n")
  print("------------------")
}

#using Kolmogorov-Smirnov
for (variable in variables) {
  result <- ks.test(cars[[variable]], "pnorm")
  cat("Variable:", variable, "\n")
  print(result)
  cat("\n")
  print("------------------")
}

#using shapiro normality test
for (variable in variables) {
  result <- shapiro.test(cars[[variable]])
  cat("Variable:", variable, "\n")
  print(result)
  cat("\n")
  print("------------------")
}

#hypothesis testing

#Hypothesis testing for "MPG" column

#mean
mean(cars$MPG)

#t-test
t.test(x=cars$MPG, mu=23)

#hypothesis testing for "Displacement"

#mean
mean(cars$Displacement)

#t-test
t.test(x=cars$Displacement, mu=194)

#hypothesis testing for "Horsepower"

#mean
mean(cars$Horsepower)

#t-test
t.test(x=cars$Displacement, mu=103)

#hypothesis testing for "Weight"

#mean
mean(cars$Weight)

#t-test
t.test(x=cars$Weight, mu=2979)

#hypothesis testing for "Acceleration"

#mean
mean(cars$Acceleration)

#t-test
t.test(x=cars$Acceleration, mu=15)



#Goodness of fit test
#Goodness of fit test for origin 
cars%>%count(Origin)

Origins<-c(73,79,254)
origin_probability <- c(0.33,0.33,0.34)
chisq.test(Origins, p= origin_probability)

#Goodness of fit test for Models
cars%>%count(Model)

Models<-c(35,29,28,40,27,30, 34,28,36,29,29,30,31)
models_probability <- c(0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.076)
chisq.test(Models, p= models_probability)

#Goodness of fit test for Models
cars%>%count(Model)

Models<-c(35,29,28,40,27,30, 34,28,36,29,29,30,31)
models_probability <- c(0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.077,0.076)
chisq.test(Models, p= models_probability)

#Goodness of fit test for Cylinders
cars%>%count(Cylinders)

Cylinder<-c(4,207,3,84,108)
Cylinder_probability <- c(0.2,0.2,0.2,0.2,0.2)
chisq.test(Cylinder, p= Cylinder_probability)


#Chi-square test for dependence

#Dependence Origin vs Cylinders
cars.data<-data.frame(cars$Origin, cars$Cylinders)
head(cars.data)
cars.data= table(cars$Origin,cars$Cylinders)
cars.data
test<-chisq.test((cars.data))
test

#Dependence models vs Cylinders
cars.data<-data.frame(cars$Model, cars$Cylinders)
head(cars.data)
cars.data= table(cars$Model,cars$Cylinders)
cars.data
test<-chisq.test((cars.data))
test

#Dependence Origin vs models
cars.data<-data.frame(cars$Origin, cars$Model)
head(cars.data)
cars.data= table(cars$Origin,cars$Model)
cars.data
test<-chisq.test((cars.data))
test





#Regression
#clear environment
rm(list=ls())

#Data Cleaning
df<- read.csv("cars.csv",header = T,sep = ";")
df = df[-1,]
View(df)

head(df)
dim(df) #406 rows and 9 cols


#Changing horsepower into quantitative variable
df$Horsepower <- as.numeric(df$Horsepower)


# Run the linear model and save it as 'mod'
mod<-lm(Acceleration~Horsepower,data=df)

# Let's view the output:
mod

#get more results
summary(mod)

# combine the regression with the scatter plot
plot(df$Acceleration~df$Horsepower,
     data=df,
     pch=16, 
     xlab="Horsepower", 
     ylab="Acceleration",
     col="skyblue")

abline(mod,lwd=2,col="red")


#Extract the regression table
library('broom')
myresult<-tidy(mod)
myresult






#ANOVA 
#Clear environment
rm(list=ls())

df<- read.csv("cars.csv",header = T,sep = ";")
View(df)
df = df[-1,]

head(df)

library(tidyverse)

dim(df)
head(df)

#Only focus on origin and horsepower
dat<-df %>% select(Origin,Horsepower)
head(dat)

#Check the distinct values of Origin
unique(df$Origin) #[US, Japan, Europe]


library(ggplot2)

#ANOVA

#The null and alternative hypothesis of an ANOVA are:


#H0: The 3 Car's Origin [US, Japan, Europe] are equal in terms of Horsepower

#H1: At least one mean is different (at least one origin is different 
#   from the other 2 origin in terms of horsepower)


library("RColorBrewer")

#1. visualise the data
boxplot(dat$Horsepower ~ dat$Origin, 
        data = dat, 
        ylab = "Horsepower",
        xlab = "Car's Origin",
        col = brewer.pal(n = 3, name = "RdBu"))



#2 descriptive statistics
aggregate(dat$Horsepower~dat$Origin,data=dat, function(x)round(c(mean(x),sd=sd(x)),2))


#3 : oneway.test()
result<-oneway.test(Horsepower~dat$Origin, data=dat, var.equal=TRUE) #assume equal var
result
#reject null hypothesis -not equal




# Chi-square with independence

# Check Origin with Cylinders
table_data <- table(cars$Origin, cars$Cylinders)
table_data

# Create a new data frame
# Unique values of Origin column as the rows
csData <- data.frame(Origin = unique(cars$Origin))

# Create a table of counts for each combination of Origin and Cylinders
# Check and add the table data to the new data frame
if ("3" %in% colnames(table_data))
  csData$`3-4` <- table_data[, "3"] + table_data[, "4"]
if ("5" %in% colnames(table_data))
  csData$`5-6` <- table_data[, "5"] + table_data[, "6"]
if ("8" %in% colnames(table_data))
  csData$`7-8` <- table_data[, "8"]

# Print the new data frame
print(csData)
cylinders <- data.frame(csData)
cylinders$Origin <- NULL

# Perform chi-square test for independence
chi_square <- chisq.test(cylinders)
print(chi_square)
# results: X-squared = 156.38, df = 4, p-value < 2.2e-16
# X-squared (4,0.05) = 9.488, so reject null hypothesis

# Plot the chi-square curve
curve(dchisq(x, df = 4), from = 0, to = 20,
      main = 'Chi-Square Distribution (df = 4)',
      ylab = 'Density',
      lwd = 2, frame=FALSE)

# create vector of x values
# X-squared (4,0.05) = 9.488
x_vector <- seq(9.488, 20)

# create vector of chi-square density values
p_vector <- dchisq(x_vector, df = 4)

polygon(c(x_vector, rev(x_vector)), c(p_vector, rep(0, length(p_vector))),
        col = adjustcolor('red', alpha=0.3), border = NA)


# Correlation

# Check if any value = 0
print(sum(cars$Horsepower == 0))
print(sum(cars$Displacement == 0))

# Copy the original data frame
corData <- data.frame(cars)

# Remove rows with 0 horsepower
corData <- subset(corData, corData$Horsepower != 0)

# Convert to numeric
corData$Displacement <- as.numeric(corData$Displacement)
corData$Horsepower <- as.numeric(corData$Horsepower)

# Perform correlation test
correlation_result <- cor.test(corData$Displacement, corData$Horsepower)
print(correlation_result)
# results: t = 40.793, df = 398, p-value < 2.2e-16, cor = 0.8983263

# Plot the correlation
plot(corData$Displacement, corData$Horsepower, main="Horsepower vs Displacement", xlab="Displacement", ylab="Horsepower", 
     pch=19, cex=1.25, col=c("dodgerblue3"), las=1)

abline(lm(corData$Horsepower~corData$Displacement), col=c("red"))


