library(lattice)
library(ggplot2)
library(corrplot)
library(ROCR)
library(randomForest)

# Implementing random forest

train_rf = train
test_rf  = test

train_rf$success = ifelse(train_rf$success == 1, "S", "F")
train_rf$success = as.factor(train_rf$success)
test_rf$success  = ifelse(test_rf$success == 1, "S", "F")
test_rf$success  = as.factor(test_rf$success)

head(train_rf)
model1 = randomForest(success ~ ., data = train_rf, importance = TRUE)
model1

model2 = randomForest(success ~ ., data = train_rf, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain = predict(model2, train_rf, type = "class")

# Checking classification accuracy
table(predTrain, train_rf$success)
confusionMatrix(table(predTrain, train_rf$success), positive="S")

# Predicting on test set
predTest = predict(model2, test_rf, type = "class")

table(predTest, test_rf$success)
confusionMatrix(table(predTest, test_rf$success),positive="S")

mean(predTest == test_rf$success)

# To check important variables in model2
importance(model2)        
varImpPlot(model2) 

######## MODEL 3 is the used model!!!! ###########
model3 = randomForest(success ~ ., data = train_rf, ntree = 500, mtry = 5, importance = TRUE)
model3

# Predicting on train set
predTest_3 = predict(model3, test_rf, type = "class")

table(predTest_3, test_rf$success)
confusionMatrix(table(predTest_3, test_rf$success),positive = "S")

pred_rf_3 = ifelse(predTest_3 == "S", 1, 0)
rf_label  = ifelse(test_rf$success == "S", 1, 0)
# Printing area under the curve for RF
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_rf_3 = prediction(pred_rf_3, rf_label) 
perf_obj_rf_3 = performance(pred_obj_rf_3, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_rf_3      = performance(pred_obj_rf_3, "auc")@y.values[[1]]
plot(perf_obj_rf_3)
abline(a = 0, b = 1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_rf_3,3))))


importance(model3)        
varImpPlot(model3) 

model4 = randomForest(success ~ ., data = train_rf, ntree = 500, mtry = 4, importance = TRUE)
model4

importance(model4)
write.csv(importance(model4), file= "RF_VarImp.csv")

# Predicting on train set
predTest_4 <- predict(model4, test_rf, type = "class")

table(predTest_4, test_rf$success)
confusionMatrix(table(predTest_4, test_rf$success),positive="S")

pred_rf_4 <- ifelse(predTest_4 == "S", 1, 0)
# Printing area under the curve for RF
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_rf_4 = prediction(pred_rf_4, rf_label) 
perf_obj_rf_4 = performance(pred_obj_rf_4, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_rf_4      = performance(pred_obj_rf_4, "auc")@y.values[[1]]
plot(perf_obj_rf_4)
abline(a = 0, b = 1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_rf_4,3))))