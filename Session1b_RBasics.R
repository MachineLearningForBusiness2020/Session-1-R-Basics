#Highlight a code and press "Control+r" in Windows or "command+enter" in MacOS to run this code.

#get the current working directory
getwd()

# Basic Calculations
8*6
2^16
2^
8*6
8*10

# Functions
sqrt(2)
abs(-65)
log(10)

#modulo function
5%%3

#Help document
?sqrt
example(abs)


# Variables
SquareRoot2 = sqrt(2)
SquareRoot2
HoursYear <- 365*24
HoursYear
print(SquareRoot2)
cat('SquareRoot2 =',SquareRoot2,'\n','HoursYear =',HoursYear)
ls()

# Vectors
c(2,3,5,8,13)
Country = c("Brazil", "China", "India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country
LifeExpectancy
Country[1]
LifeExpectancy[3]
Sequence = seq(0,100,2)
Sequence

# Matrices

#Entry values
entries = 1:20
#Row Names
rnames = c('R1','R2','R3','R4','R5')
#Column Names
cnames = c('C1','C2','C3','C4')
#Create a matrix
M = matrix(entries,nrow=5,ncol=4,byrow=TRUE,dimnames=list(rnames,cnames))

entries
rnames
cnames
M

# Data Frames
CountryData = data.frame(Country, LifeExpectancy)
CountryData
CountryData$Population = c(199000,1390000,1240000,7997,318000)
CountryData
Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)
NewCountryData
AllCountryData = rbind(CountryData, NewCountryData)
AllCountryData

#Calculate the mean and standard deviation of life expectancy
mean(LifeExpectancy)

sd(LifeExpectancy)

#save data into .csv file

write.csv(AllCountryData,"AllCountryData.csv")


# if...else... statement

x <- -5
if(x >= 0){
  print("Non-negative number")
} else {
  print("Negative number")
}

# if...else... ladder

x <- 0
if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else
print("Zero")


#For loop
#Count the number of odd numbers

x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 1)  count = count+1
}
print(count)

#While loop

i <- 1
while (i < 6) {
  print(i)
  i = i+1
}

#Define your own functions
pow <- function(x, y) {
  # function to print x raised to the power y
  result <- x^y
  print(paste(x,"raised to the power of", y, "is", result))
  return(result) 
}

a = 2
b = 10
r = pow(a,b)
print(r)

# Read the csv file
  USDA = read.csv("USDA.csv")
# Structure of the data set
  str(USDA)
# Statistical summary
  summary(USDA)


# Vector notation
  USDA$Sodium
  
# Finding the index of the food with highest sodium levels
  which.max(USDA$Sodium)
  
# Get names of variables in the data set
  names(USDA)
  
# Get the name of the food with highest sodium levels
  USDA$Description[which.max(USDA$Sodium)]
  
# Create a subset of the foods with sodium content above 10,000mg
  HighSodium = subset(USDA, Sodium>10000)
  
# Count the number of rows, or observations
  nrow(HighSodium)

# Output names of the foods with high sodium content
  HighSodium$Description
  
# Finding the index of CAVIAR in the data set
  match("CAVIAR", USDA$Description)
  
# Find amount of sodium in caviar
  USDA$Sodium[match("CAVIAR", USDA$Description)]

# Summary function over Sodium vector
  summary(USDA$Sodium)
  
# Standard deviation
  sd(USDA$Sodium, na.rm = TRUE)
  
# Scatter Plots
  plot(USDA$Protein, USDA$TotalFat)
  
# Add xlabel, ylabel and title
  plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")

# Creating a histogram
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C")

# Add limits to x-axis
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100))

# Specify breaks of histogram
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100), breaks=100)
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100), breaks=2000)

# Boxplots
  boxplot(USDA$Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar")


# Creating a variable that takes value 1 if the food has higher sodium than average, 0 otherwise
  HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
  str(HighSodium)

# Adding the variable to the data set
  USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))

# Similarly for HighProtein, HigCarbs, HighFat
  USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
  USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
  USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))

# How many foods have higher sodium level than average?
  table(USDA$HighSodium)

# How many foods have both high sodium and high fat?
  table(USDA$HighSodium, USDA$HighFat)

# Average amount of iron sorted by high and low protein?
  tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)

# Maximum level of Vitamin C in foods with high and low carbs?
  tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)

# Using summary function with tapply
  tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)