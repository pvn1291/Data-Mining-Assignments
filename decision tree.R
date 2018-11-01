## We are going to use the Carseats dataser from ILRS library

library(ISLR) # book's library
library(rpart) # to fit decision tree

attach(Carseats) # dataset to use

head(Carseats)

## Start data manipulation
range(Sales) # Sales range from 0 to 16

# creat a categorical variables based on sales
High <-  ifelse(Sales >= 8, "YES", "No")
length(High)
dim(Carseats)

# Appends High to Carseat dataset, and now our dataset is ready!
Carseats <- data.frame(Carseats,High)
Carseats <- Carseats[,-1]
# You could also use: Carseats$Sales <- NULL

## Split data into testing and training sets
set.seed(2)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
test <- -train
train_data <- Carseats[train,]
test_data <- Carseats[test,]

dim(train_data)

# fit the tree model using training data
tree_model <- rpart(High~., train_data)

summary(tree_model)


library(rpart.plot)
prp(tree_model)


# Check how the model is doing using the test data
tree_pred <- predict(tree_model, test_data, type = "class")
mean(tree_pred != test_data$High)


# Prune the tree
CPtable <- tree_model$cptable
plot(CPtable[,1],CPtable[,3], type = "b")

opt <- which.min(CPtable[,"xerror"])
opt
cp <- CPtable[opt, "CP"]
cp

# We can now prune the model based on the best value of cp
tree_prune <- prune(tree_model, cp = cp)
tree_prune


# Check how it is doing
tree_pred <- predict(tree_prune, test_data, type = "class")
mean(tree_pred != test_data$High)

