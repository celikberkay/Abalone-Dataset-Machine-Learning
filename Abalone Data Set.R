# Decision Tree Tutorial on Abalone Data Set 

library(tree) # Contains the "tree" function

# construct a DT with tree function using entire "Abalone" data set
mydata <- read.table("C:/Users/7/Desktop/abalone.data", header=TRUE, 
                     sep=",") # reading txt file as a table
ir.tr <- tree(Sex ~ .,data = mydata, split = "deviance")
ir.tr
# display the results
summary(ir.tr)
misclass.tree(ir.tr)

# visualize DT 
plot(ir.tr,  type = "uniform")
text(ir.tr)

set.seed(579642)  #Set the seed for reproducibility
# Use 70% of samples for traning, the rest for testing
# the indices (row ids) are saved in the "sub" vector
sub <- sample(1:nrow(mydata), size=nrow(mydata)*0.7)

ir.tr2 <- tree(Sex ~ ., data = mydata, subset = sub)
summary(ir.tr2)
misclass.tree(ir.tr2)

# plot final DT
plot(ir.tr2,  type = "uniform")
text(ir.tr2)

# Compute training performance of the DT by using only training samples (their indices were saved in the "sub" vector)
train_predict <- table(predict(ir.tr2, mydata[sub, ], type = "class"), mydata[sub, "Sex"])
rownames(train_predict) <- paste("Actual", rownames(train_predict), sep = ",")
colnames(train_predict) <- paste("Predicted", colnames(train_predict), sep = ",")
print(train_predict)

# Compute test performance of the DT  by using only test samples
test_predict <- table(predict(ir.tr2, mydata[-sub, ], type = "class"), mydata[-sub, "Sex"])
rownames(test_predict) <- paste("Actual", rownames(test_predict), sep = ",")
colnames(test_predict) <- paste("Predicted", colnames(test_predict), sep = ",")
print(test_predict)


#Cross-validation version - Construct a new DT for different partitions of the samples - 100 times

dt_acc <- numeric()
set.seed(1815850)

for(i in 1:100){
  sub <- sample(1:nrow(mydata), size=nrow(mydata)*0.7)
  fit2 <- tree(Sex ~ ., data = mydata, subset = sub)
  test_predict <- table(predict(fit2, mydata[-sub, ], type = "class"), mydata[-sub, "Sex"])
  dt_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
}

mean(dt_acc)

plot(1-dt_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Abalone With Different Subsets of Data")





#=========================================================================
#=========================================================================
#=========================================================================

# kNN Tutorial on Abalone Data Set 

library(class) # Contains the "knn" function
set.seed(498593) #Set the seed for reproducibility

#Create partitions in the mydata data set (30% for training, 70% for testing/evaluation)
ir_sample <- sample(1:nrow(mydata), size=nrow(mydata)*0.7)
ir_train <- mydata[ir_sample, ] #Select the 70% of rows
ir_test <- mydata[-ir_sample, ] #Select the 30% of rows

#First try to determine the right K-value 
abalone_acc <- numeric() #holding variable
for(i in 1:20){
  #Apply knn with k = i
  predict <- knn(train=ir_train[,-1], test=ir_test[,-1], cl=ir_train$Sex, k=i)
  abalone_acc <- c(abalone_acc, mean(predict==ir_test$Sex))
}

#Plot error rates for k=1 to 20
plot(1-abalone_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Abalone with varying K")

# Average accuracy of 20 k-values
mean(abalone_acc)


#Try different samples of the Abalone data set to validate K value 

trial_sum <- numeric(20)
trial_n <- numeric(20)
set.seed(13845)

for(i in 1:10){
  ir_sample <- sample(1:nrow(mydata), size=nrow(mydata)*0.7)
  ir_train <- mydata[ir_sample,]
  ir_test <- mydata[-ir_sample,]
  test_size <- nrow(ir_test)
  
  for(j in 1:20){
    predict <- knn(ir_train[,-1], ir_test[,-1], ir_train$Sex, k=j)
    trial_sum[j] <- trial_sum[j] + sum(predict==ir_test$Sex)
    trial_n[j] <- trial_n[j] + test_size
  }
}

plot(1-trial_sum / trial_n, type="l", ylab="Error Rate",xlab="K",main="Error Rate for Abalone With Varying K (100 Samples)")




