#Session 1b In-Class Exercises Solutions

#1. Which variable names are recommended in R: SquareRoot2; Square Root2; Square.Root2; 2SquareRoot?

#Solution: SquareRoot2 and Square.Root2

#2. You deposit $10,000 in the bank. The annual interest rate is 4.1%. How much will you get if you withdraw your money in 10 years. How many years does it take for the total amount money to grow to $20,000?  

#Solution:
# The money after 10 years:
10000*(1+0.041)^10

# The years taken to reach 20,000: (1+0.041)^t>=20000/10000=2, so t>= log(2)/(log(1+0.041))
log(2)/(log(1+0.041))

# 3. Find the two roots of 𝑋^2−4𝑋−3=0
#Solution:
a = 1
b = -4
c= -3
d = b^2 - 4*a*c
x1 = (-b-sqrt(d))/(2*a)
x2 = (-b+sqrt(d))/(2*a)
cat('x1=',x1," x2=",x2)

# 4. A wise man could make a right (resp. wrong) judgement with prob. 0.99 (resp. 0.01). What is the probability such that a wiseman will make at least one mistake if he judges for 1,000 times?
p = 0.99
1 - p^1000

# 5. Write a function to calculate n factorial, where n is a positive integer number. 
# Use your function to compute 10 factorial.

fact <- function(n) {
  # function to print n factorial
  seq = c(1:n)
  result = 1
  for (i in seq) {
    result = result * i
  }
  
  print(paste(n,"factorial is",result))
  return(result)
}

r = fact(10)
print(r)

# 6. Write a function to determine the tax one needs to pay given the total annual income x. 
# Use your function to compute the tax with annual income x=1,000,000

tax <- function(x){
  # function to print the individual income tax
  result = 0
  if(x<=36000){
    result = x*0.03
  }
  else if(x<=144000){
    result = 36000*0.03+(x-36000)*0.1 
  }
  else if(x<=300000){
    result = 36000*0.03+(144000-36000)*0.1+(x-144000)*0.2 
    } 
  else if(x<=420000){
    result = 36000*0.03+(144000-36000)*0.1+(300000-144000)*0.2+(x-300000)*0.25 
    } 
  else if(x<=660000){
    result = 36000*0.03+(144000-36000)*0.1+(300000-144000)*0.2+(420000-300000)*0.25+(x-420000)*0.3
    } 
  else if(x<=960000){
    result = 36000*0.03+(144000-36000)*0.1+(300000-144000)*0.2+(420000-300000)*0.25+(660000-420000)*0.3+(x-660000)*0.35
  }
  else{
    result = 36000*0.03+(144000-36000)*0.1+(300000-144000)*0.2+(420000-300000)*0.25+(660000-420000)*0.3+(960000-660000)*0.35+(x-960000)*0.45
    }
  print(paste("The total payable tax is",result))
  return(result)
}

x = 1000000
r = tax(x)
print(r)

#7. What is the amount of Calories in fish broth (“FISH BROTH”)?

USDA = read.csv("USDA.csv")
USDA$Calories[match("FISH BROTH", USDA$Description)]

#8. Find the standard deviation of Calories for all food.
sd(USDA$Calories,na.rm=TRUE)

#9. Illustrate the relationship between the amount of sugar and carbohydrate using scatter plot (Label the x-axis, y-axis, title and set the color to red).
plot(USDA$Sugar, USDA$Carbohydrate, xlab="Sugar", ylab = "Carbohydrate", main = "Sugar vs Carbohydrate", col = "blue")


#10. What is the average amount of fat for high carb food with protein less than the 75-percentile (of high card foods) amount? 
HighCarbs = subset(USDA,Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
summary(HighCarbs$Protein)
HighCarbs$p75 = as.numeric(HighCarbs$Protein < 10.0)
tapply(HighCarbs$TotalFat,HighCarbs$p75,mean,na.rm=TRUE)
