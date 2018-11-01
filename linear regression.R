# We are going to use the Boston dataset in the MASS package.
# The Boston dataset is a collection of data about housing values 
# in the suburbs of Boston. 
# Our goal is to predict the median value of owner-occupied homes (medv) 
# using all the other continuous variables available.

# Lets access the data first
#install.packages("MASS")
library(MASS)
?Boston
data <- Boston

# Check if there is any missing value in data. 
# We need to impute missing values before using a neural net model
apply(data,2,function(x) sum(is.na(x)))

# Split data into training and testing data
set.seed(500)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

# First fit a linear regression model 
lm.fit <- lm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

# We need to normalize data before applying neural network model
# Lets use min-max transformation to normalize data
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_scaled <- scaled[index,]
test_scaled <- scaled[-index,]

# Construct a neural network model using "neuralnet" package
# install.packages("neuralnet")
library(neuralnet)
n <- names(train_scaled)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f, data=train_scaled, hidden = 4, linear.output=T)

# Plot the constructed neural network model
plot(nn)

# Get the predicted value on test data
pr.nn <- compute(nn,test_scaled[,1:13])
pr.nn$net.result

# Remember that the net will output a normalized prediction, 
# so we need to scale it back in order to make a meaningful comparison
pr.nn_origin <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

test.r <- (test_scaled$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

# Let's compute MSE
MSE.nn <- sum((test.r - pr.nn_origin)^2)/nrow(test_scaled)

print(paste(MSE.lm,MSE.nn))
# It seems the neural network model is doing a better work than the linear model 
# at predicting medv. A better comparison is looking at the error rates through the cross-validation.

# visual approach to the performance of the network and the linear model on the test set
par(mfrow=c(1,2))

plot(test$medv,pr.nn_origin,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)