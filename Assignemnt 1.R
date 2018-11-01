library(readr)

# 3.
#1.
college <- read.csv(file.choose())
Auto <- read.csv(file.choose())
salary_class <- read.csv(file.choose())
View(college)
rownames(college) <- college[, 1]
college$X <- NULL

#2. 
str(college)
stats<-psych::describe(college[,2:dim(college)[2]])

#3.

pairs(college[,1:10])

#4.
dev.off()
boxplot(college$Outstate~college$Private, main="Distribution of Outstate by Private/Public", col=c("green", "orange")
        , xlab = "Private (Yes | No)", ylab = "Outstate")

#5.

# i.
# Create Elite variable with repeatation of NO for all rows.
# Assign Yes to Elite variable for those rows having Top10Perc from college > 50
# Convert character Elite variable to factor with 2 levels "Yes" and "No"
# Update college data frame by addding elite variable.

View(college)

college$Elite[college$Top10perc > 50] <- "Yes"
college$Elite[college$Top10perc <= 50] <- "No"
college$Elite <- as.factor(college$Elite)

# ii.
summary(college$Elite)

boxplot(college$Outstate~college$Elite, main="Distribution of Outstate by Elite (Yes | No)", col=c("green", "orange")
        , xlab = "Elite (Yes | No)", ylab = "Outstate")

#6.
dev.off()
par(mfrow = c(2,2))
hist(college$Outstate, main = "Distribution of Outstate", xlab = "Outstate", col = "steelblue", las = 2, freq = F, breaks = 10)
hist(college$Books, main = "Distribution of Books", xlab = "Cost of Books", col = "green", las = 2, freq = F, breaks = 12)
hist(college$Apps, main = "Distribution of Apps", xlab = "Applications", col = "orange", las = 2, freq = F, breaks = 14)
hist(college$Top25perc, main = "Distribution of Top25 Percent of high school class", breaks = 8, xlab = "Top25Percent", col = "grey", las = 2, freq = F)

#################################################################
# 4.

library(readr)
Auto <- read.csv("Auto.csv")
View(Auto)
str(Auto)

# a. 
# Checking for NA's
for (i in names(Auto)) {
  count <- sum(is.na(Auto[, i]))
  if(count > 0) {
    print(i)
  } 
}

# No missing values.

# b.
View(Auto)
str(Auto)
# $ mpg         : num  
# $ cylinders   : int  
# $ displacement: num  
# $ horsepower  : Factor w/ 94 levels 
# $ weight      : int  
# $ acceleration: num  
# $ year        : int  
# $ origin      : int  
# $ name        : Factor w/ 304 Levels.

# str command gives is the structure and the data type. However, we still need to prob further to handle the data.
length(levels(as.factor(Auto$mpg))) # 129 
length(levels(as.factor(Auto$cylinders))) # 5 -> Can be converted to a factor
length(levels(as.factor(Auto$displacement))) # 82
length(levels(as.factor(Auto$horsepower))) # 94
length(levels(as.factor(Auto$weight))) # 350
length(levels(as.factor(Auto$acceleration))) # 95
length(levels(as.factor(Auto$year))) # 13 -> Can be converted to a factor
length(levels(as.factor(Auto$origin))) # 3 -> Can be converted to a factor
length(levels(as.factor(Auto$name))) # 304


# Quantitative:-> mpg, displacement, horsepower, weight, acceleration
# Qualitative:-> cylinders, year, origin

# Note: name does not gives is anything therefore it should be converted to character.

# c.

Auto$mpg <- as.numeric(Auto$mpg)
Auto$displacement <- as.numeric(Auto$displacement)
Auto$horsepower <- as.numeric(Auto$horsepower)
Auto$weight <- as.numeric(Auto$weight)
Auto$acceleration <- as.numeric(Auto$acceleration)

Auto$cylinders <- as.factor(Auto$cylinders)
Auto$year <- as.factor(Auto$year)
Auto$origin <- as.factor(Auto$origin)

Auto$name <- as.character(Auto$name)

predictorQuantitativeColumns <- c('mpg', 'displacement', 'horsepower', 'weight', 'acceleration')
quantitativeAutoColumns <- Auto[ , predictorQuantitativeColumns]
sapply(quantitativeAutoColumns, FUN = range)
# Range of quantitative columns
#         mpg      displacement horsepower   weight    acceleration
# [1,]    9.0           68          1        1613          8.0  <- Minimum
# [2,]   46.6          455         94        5140         24.8  <- Maximum

# d.
sapply(quantitativeAutoColumns, FUN = sd)
# Standard Deviation of quantitative columns
#     mpg    displacement   horsepower       weight      acceleration 
#  7.825804   104.379583    29.862697      847.904119     2.749995 

# e.
AutoSubset <- Auto
AutoSubset <- AutoSubset[-c(10:85), ] # Removing 10th to 85th observation from AutoSubset

sapply(AutoSubset[,predictorQuantitativeColumns], FUN = sd)
# SD
#    mpg     displacement   horsepower       weight      acceleration 
# 7.908184    99.635385     30.076725       810.642938     2.680514 

sapply(AutoSubset[,predictorQuantitativeColumns], FUN = mean)
# Mean
#    mpg     displacement   horsepower       weight    acceleration 
# 24.43863    187.04984     50.99688      2933.96262     15.72305 

sapply(AutoSubset[,predictorQuantitativeColumns], FUN = range)
# Range      
#        mpg      displacement horsepower weight    acceleration
# [1,]  11.0           68          1      1649          8.5
# [2,]  46.6          455         94      4997         24.8

# f.
# Step 1: Cheking distribution of every variable
# Numeric or Quantitative variables
dev.off()
par(mfrow = c(5,2))
# mpg
hist(Auto$mpg, main = "Distribution of MPG", xlab = "MPG", col = "steelblue", las = 2, freq = F)
rug(jitter(Auto$mpg), col = "red")
lines(density(Auto$mpg), col = "green", lwd = 2)
boxplot(Auto$mpg, col=c("orange"), xlab = "MPG")

# displacement
hist(Auto$displacement, main = "Distribution of displacement", xlab = "displacement", col = "steelblue", las = 2, freq = F)
rug(jitter(Auto$displacement), col = "red")
lines(density(Auto$displacement), col = "green", lwd = 2)
boxplot(Auto$displacement, col=c("orange"), xlab = "displacement")

# horsepower
hist(Auto$horsepower, main = "Distribution of horsepower", xlab = "horsepower", col = "steelblue", las = 2, freq = F)
rug(jitter(Auto$horsepower), col = "red")
lines(density(Auto$horsepower), col = "green", lwd = 2)
boxplot(Auto$horsepower, col=c("orange"), xlab = "horsepower")

# weight
hist(Auto$weight, main = "Distribution of weight", xlab = "weight", col = "steelblue", las = 2, freq = F)
rug(jitter(Auto$weight), col = "red")
lines(density(Auto$weight), col = "green", lwd = 2)
boxplot(Auto$weight, col=c("orange"), xlab = "weight")

# acceleration
hist(Auto$acceleration, main = "Distribution of acceleration", xlab = "acceleration", col = "steelblue", las = 2, freq = F)
rug(jitter(Auto$acceleration), col = "red")
lines(density(Auto$acceleration), col = "green", lwd = 2)
boxplot(Auto$acceleration, col=c("orange"), xlab = "acceleration")

# Qualitative variables

dev.off()
par(mfrow = c(3,1))
# Cylinder
barplot(prop.table(table(Auto$cylinders)) * 100, main = "Cylinder Categories", xlab = "Cylinder Category (Levels)", las = 1, col = "orange")
# Year
barplot(prop.table(table(Auto$year)) * 100, main = "Distribution by Year", xlab = "Year Category (Levels)", las = 1, col = "lightblue")
# Origin
barplot(prop.table(table(Auto$origin)) * 100, main = "Distribution by origin", xlab = "Origin Category (Levels)", las = 1, col = "green")

# Steps 2: Relationship between predictors/indepedent variables

# Numeric- Numeric

dev.off()
par(mfrow = c(3,2))
# mpg - displacement
plot(Auto$mpg~Auto$displacement, col="red", main="Mileage to Displacement", xlab="mpg", ylab="displacement", pch=16) 
abline(lm(Auto$mpg~Auto$displacement), col="darkgreen", lwd=2.5)

# mpg - horsepower
plot(Auto$mpg~Auto$horsepower, col="red", main="mpg to horsepower", xlab="mpg", ylab="horsepower", pch=16) 
abline(lm(Auto$mpg~Auto$horsepower), col="darkgreen", lwd=2.5)

# horsepower - accelaration
plot(Auto$horsepower~Auto$acceleration, col="red", main="horsepower to Accelaration", xlab="horsepower", ylab="acceleration", pch=16) 
abline(lm(Auto$horsepower~Auto$acceleration), col="darkgreen", lwd=2.5)

# mpg - weight
plot(Auto$mpg~Auto$weight, col="red", main="Mileage to weight", xlab="mpg", ylab="weight", pch=16) 
abline(lm(Auto$mpg~Auto$weight), col="darkgreen", lwd=2.5)

# horsepower - weight
plot(Auto$horsepower~Auto$weight, col="red", main="horsepower to weight", xlab="horsepower", ylab="weight", pch=16) 
abline(lm(Auto$horsepower~Auto$weight), col="darkgreen", lwd=2.5)

# mpg - acceleration
plot(Auto$mpg~Auto$acceleration, col="red", main="mpg to acceleration", xlab="mpg", ylab="acceleration", pch=16) 
abline(lm(Auto$mpg~Auto$acceleration), col="darkgreen", lwd=2.5)

# Factor- Numeric
dev.off()
par(mfrow = c(2, 2))
# Mpg distibution by Cylinders
boxplot(mpg ~ cylinders, data = Auto, main="Distribution of mpg for cylinders", 
        xlab = "cylinders Categories (levels)", ylab = "mpg", col=c("orange", "lightblue4"))

# Acceleration distribution by Cylinders
boxplot(acceleration ~ cylinders, data = Auto, main="Distribution of acceleration for cylinders", 
        xlab = "cylinders Categories (levels)", ylab = "acceleration", col=c("orange", "lightblue4"))

# Mpg distibution by Origin
boxplot(mpg ~ origin, data = Auto, main="Distribution of mpg for origin", 
        xlab = "origin Categories (levels)", ylab = "mpg", col=c("orange", "lightblue4"))

# Mpg distibution by Year
boxplot(mpg ~ year, data = Auto, main="Distribution of mpg for year", 
        xlab = "year Categories (levels)", ylab = "mpg", col=c("orange", "lightblue4"))

# Comments: 
# 1. Acceleration is normally distributed.
# 2. MPG, Displacement and Weight has little positive skewness.
# 3. Horsepower has little negative skewness.
# 4. Most automobiles has 4 cylinders and origin 1.
# 5. With the increase in mileage displacement decreases.
# 6. With the inrease in weight mileage decreases.
# 7. Automobiles with Origin 3 has highest mileage followed by 2 & 1.

# g.
# Comments: Variables such a Displacement, Weight and Origin will be useful in predicting mileage.
dev.off()
plot(density(Auto$mpg)) # Normally Distributed. Little skew. Lets remove outliers.
outliers<-boxplot(Auto$mpg)$out
mpg1<-ifelse(Auto$mpg %in% outliers, mean(Auto$mpg), Auto$mpg)

# mpg and displacement
# Negatively correlated -> -0.804
corSummary <- cor(Auto$mpg, Auto$displacement, use = "complete.obs", method = "pearson")
corSummary

# mpg and Weight
# Negatively correlated -> -0.8317
corSummary <- cor(Auto$mpg, Auto$weight, use = "complete.obs", method = "pearson")
corSummary

# mpg and origin
# H0 Null rejected (pValue < 5%). That mean is different for all the categories.
testAOV <- aov(mpg~origin, data = Auto)
summary(testAOV) 

# R-Square value 0.703. Displacement, Weight and Origin are 3 key parameters to predict mileage.
options(scipen = 999)
linearModel <- lm (mpg1 ~ displacement+weight+origin, data = Auto)
summary(linearModel)


#################################################################
# 5.

library(readr)
salary_class <- read.csv("salary-class.csv")
View(salary_class)
str(salary_class)

sampleSize<-round(nrow(salary_class) * 0.6)
sampIndices <- sample(nrow(salary_class), sampleSize)
trainingData <- salary_class[sampIndices,]
testData <- salary_class[-sampIndices,]

# 2. 
# Classification and Regression (C&R)
# Classification tree analysis is when the predicted outcome is the class to which the data belongs.
# Regression tree analysis is when the predicted outcome can be considered a real number
dev.off()
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
colnames(salary_class)

index = sample(2, nrow(salary_class), replace = T, prob = c(0.6, 0.4))
trainingData = salary_class[index == 1, ]
testData = salary_class[index == 2, ]

trainData_tree1 <- rpart(INCOME~., data = trainingData, method = "class")
trainData_tree1
rpart.plot(trainData_tree1)
plot(trainData_tree1)

trainData_tree1_result <- table(predict(trainData_tree1, type = "class"), trainingData$INCOME, dnn = c("Predicted", "Actual"))
trainData_tree1_result
trainData_error_tree1 <- (931 + 1994) / nrow(trainingData) * 100; trainData_error_tree1

testData_tree1 <- rpart(INCOME~., data = testData, method = "class")
testData_tree1
rpart.plot(testData_tree1)
plot(testData_tree1)

testData_tree1_result <- table(predict(testData_tree1, type = "class"), testData$INCOME, dnn = c("Predicted", "Actual"))
testData_tree1_result
testData_error_tree1 <- (497 + 1374) / nrow(testData) * 100; testData_error_tree1

diffInError_tree1 <- (trainData_error_tree1 - testData_error_tree1); diffInError_tree1


# 5 leaves in a decision tree.

# 3. 
str(salary_class)
summary(tree1)
tree1
# Major predictors of income as seen from decision tree are :- 
# a. MSSTATUS
# b. C.GAIN
# C. DEGREE

# We can get this information from the splitting nodes having greater support.

# 4.
levels(salary_class$MSTATUS)
levels(salary_class$DEGREE)
tree1

noOfTuples <- nrow(trainingData) # 19537

# Rule 1:
# If Marital Status is Divorced | Married-spouse-absent | Never-married | Separated | 
# Widowed and C.Gain is less than 7140 then income is less 
# than equal to 50 K.

# Confidence : (10321 - 518) / 10321 = 94.98%
# Support: 10321 / noOfTuples = 52.82%

# Rest no other rules meet the criteria given in the question. Giving the other best rules.

# Rule 2:
# If Marital Status is Married-AF-spouse |  Married-civ-spouse and Degree is Bachelors 
# | Doctorate | Masters | Prof-school then income is greater than 50 K.

# Confidence : (2706 - 771) / 2706 = 71.50%
# Support: 2076 / noOfTuples = 10.62%

# Rule 3:
# If Marital Status is Married-AF-spouse |  Married-civ-spouse and Degree is 10th |
# 11th | 12th | 1st-4th | 5th-6th | 7th-8th | 9th | Assoc-acdm | Assoc-voc | HS-grad |
# Preschool | Some-collegeand C.Gain greater than equal to 5096 then then income 
# is less than equal 50 K.

# Confidence : (6030 - 1785) / 6030 = 70.39%
# Support: 6030 / noOfTuples = 30.86%

# 5.


# Tree 2: Tree without pruning,
trainData_tree2 <- rpart(INCOME~., data = trainingData, method = "class", 
                         control = rpart.control(minsplit = 0, cp = 0.0))
trainData_tree2
#rpart.plot(tree2)
#plot(trainData_tree2)

trainData_tree2_result <- table(predict(trainData_tree2, type = "class"), 
                                trainingData$INCOME, dnn = c("Predicted", "Actual"))
trainData_tree2_result
trainData_error_tree2 <- (180 + 276) / nrow(trainingData) * 100; trainData_error_tree2

testData_tree2 <- rpart(INCOME~., data = testData, method = "class", 
                        control = rpart.control(minsplit = 0, cp = 0.0))
testData_tree2
#rpart.plot(tree2)
#plot(testData_tree2)

testData_tree2_result <- table(predict(testData_tree2, type = "class"), 
                               testData$INCOME, dnn = c("Predicted", "Actual"))
testData_tree2_result
testData_error_tree2 <- (102 + 139) / nrow(testData) * 100; testData_error_tree2

diffInError_tree2 <- (trainData_error_tree2 - testData_error_tree2); diffInError_tree2

# Tree 3: Tree with 500 observations for parent & 100 for children and cp = 0.02,
trainData_tree3 <- rpart(INCOME~., data = trainingData, method = "class", 
                         control = rpart.control(minsplit = 500, minbucket = 100, cp = 0.02))
trainData_tree3
#rpart.plot(tree3)
plot(trainData_tree3)

trainData_tree3_result <- table(predict(trainData_tree3, type = "class"), 
                                trainingData$INCOME, dnn = c("Predicted", "Actual"))
trainData_tree3_result
trainData_error_tree3 <- (769 + 2255) / nrow(trainingData) * 100; trainData_error_tree3

testData_tree3 <- rpart(INCOME~., data = testData, method = "class", 
                        control = rpart.control(minsplit = 500, minbucket = 100, cp = 0.02))
testData_tree3
#rpart.plot(tree3)
plot(testData_tree3)

testData_tree3_result <- table(predict(testData_tree3, type = "class"), 
                               testData$INCOME, dnn = c("Predicted", "Actual"))
testData_tree3_result
testData_error_tree3 <- (506 + 1554) / nrow(testData) * 100; testData_error_tree3

diffInError_tree3 <- (trainData_error_tree3 - testData_error_tree3); diffInError_tree3

