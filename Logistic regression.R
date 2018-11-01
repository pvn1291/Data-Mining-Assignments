mydata <- binary
head(mydata)

summary(mydata)

# convert the integer variale rank to factor
mydata$rank <- factor(mydata$rank)
mydata$admit <- factor(mydata$admit)

# or we can use lapply(data, FUN) function to apply the function FUN on "data" 
#****
var <- data.frame(mydata$rank, mydata$admit)
var <- lapply(var,factor)
mydata$rank <- var[[1]]
mydata$admit <- var$mydata.admit
#*****

summary(mydata)

# Run a logistic regression model
mylogit = glm(admit~.,data= mydata, family= "binomial")

summary(mylogit)

exp(coef(mylogit))

#  Lets apply the step function for variable selection
null = glm(admit~1, data= mydata, family = "binomial") # Includes only the intercept
full = glm(admit~., data= mydata, family = "binomial")
step(null, scope=list(lower=null, upper=full), direction="forward")

