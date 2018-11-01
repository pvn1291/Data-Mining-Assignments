library(ISLR)
data("Carseats")
Data<-Carseats[sample(nrow(Carseats)),] 
attach(Data)
High <- ifelse(Sales >= 8, "YES", "No")
Data <- data.frame(Data,High)
Data <- Data[,-1]
colnames(Data)[11] <- "Target"

set.seed(2)
indx <- sample(2, nrow(Data), replace = TRUE, prob = c(0.8,0.2))
train_data <- Data[indx == 1,]
test_data <- Data[indx == 2 ,]

#************************************
# Naive Bayes Model

library(e1071)
naive_e1071 <- naiveBayes(Target ~ ., data = train_data)
naive_e1071
predict(naive_e1071, train_data, type = "raw")


#************************************
# Random Forest Model

library(randomForest)
rf = randomForest(Target ~ ., data = Data, ntree = 300, proximity = T, replace= T, importance = T, mtry = sqrt(ncol(Data)-1))
rf

plot(rf)
legend("topright", legend = colnames(rf$err.rate), cex = 0.5, lty = c(1,2,3), col = c(1,2,3), horiz = T)

importance(rf, type = 2) 

rf$proximity

rf$predicted

rf$votes

#******************************
# Confusion matrix
library(caret)
confusionMatrix(rf$predicted,Data$Target)
confusionMatrix(predict(rf, type = "class", newdata = test_data), test_data$Target, positive = "YES", dnn = c("Predictions", "Actual Values"))

#*****************************
# Charts
library(ROCR)
score <- rf$votes[,2]
pred <- prediction(score, Data$Target) 
pred

perf <- performance(pred,"tpr","fpr")
perf
plot(perf)
plot(perf, col = "red", lty = 3, lwd = 3)

auc <- performance(pred, "auc")
auc
auc <- unlist(slot(auc, "y.values"))
auc

opt.cut <- function(perf, pred){
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2
  ind<- which(d==min(d))
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},perf@x.values, perf@y.values,perf@alpha.values)
}

print(opt.cut(perf,pred))

#***************************************
# Cross-validation

k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(Data)),breaks=k,labels=FALSE) 
models.err <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rf")))

for(i in 1:k)
{ 
  testIndexes <- which(folds==i, arr.ind=TRUE) 
  Test <- Data[testIndexes, ] 
  Train <- Data[-testIndexes, ] 
  
  library(randomForest)
  rf <- randomForest(Target~., data = Train, ntree = 10, mtry = sqrt(ncol(Train)))
  predicted <- predict(rf, newdata = Test, type = "class")
  models.err[i] <- mean(Test$Target != predicted)
  }

models.err

mean(models.err)
