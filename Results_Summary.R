
# Results of Full Logistic regression
accuracy.full = confusionMatrix(table(test_long$score, test_long$success), positive = "1")$overall["Accuracy"]
sens.full     = confusionMatrix(table(test_long$score, test_long$success), positive = "1")$byClass["Sensitivity"]
spe.full      = confusionMatrix(table(test_long$score, test_long$success), positive = "1")$byClass["Specificity"]
type1.full    = confusionMatrix(table(test_long$score, test_long$success), positive = "1")$table[2,1]
type2.full    = confusionMatrix(table(test_long$score, test_long$success), positive = "1")$table[1,2]

# Results of Reduced logistic regression
accuracy.reduced = confusionMatrix(table(test_v2$score, test_v2$success), positive = "1")$overall["Accuracy"]
sens.reduced     = confusionMatrix(table(test_v2$score, test_v2$success), positive = "1")$byClass["Sensitivity"]
spe.reduced      = confusionMatrix(table(test_v2$score, test_v2$success), positive = "1")$byClass["Specificity"]
type1.reduced    = confusionMatrix(table(test_v2$score, test_v2$success), positive = "1")$table[2,1]
type2.reduced    = confusionMatrix(table(test_v2$score, test_v2$success), positive = "1")$table[1,2]

# Results of Rpart
accuracy.rpart = confusionMatrix(table(pred.rp, test.tree$success), positive="1")$overall["Accuracy"]
sens.rpart     = confusionMatrix(table(pred.rp, test.tree$success), positive="1")$byClass["Sensitivity"]
spe.rpart      = confusionMatrix(table(pred.rp, test.tree$success), positive="1")$byClass["Specificity"]
type1.rpart    = confusionMatrix(table(pred.rp, test.tree$success), positive="1")$table[2,1]
type2.rpart    = confusionMatrix(table(pred.rp, test.tree$success), positive="1")$table[1,2]

# Results of CTree
accuracy.inf = confusionMatrix(table(ctree.predict, new_test$success), positive ="1")$overall["Accuracy"]
sens.inf     = confusionMatrix(table(ctree.predict, new_test$success), positive ="1")$byClass["Sensitivity"]
spe.inf      = confusionMatrix(table(ctree.predict, new_test$success), positive ="1")$byClass["Specificity"]
type1.inf    = confusionMatrix(table(ctree.predict, new_test$success), positive ="1")$table[2,1]
type2.inf    = confusionMatrix(table(ctree.predict, new_test$success), positive ="1")$table[1,2]

# Results of Random Forest
accuracy.rf = confusionMatrix(table(predTest_3, test_rf$success),positive="S")$overall["Accuracy"]
sens.rf     = confusionMatrix(table(predTest_3, test_rf$success),positive="S")$byClass["Sensitivity"]
spe.rf      = confusionMatrix(table(predTest_3, test_rf$success),positive="S")$byClass["Specificity"]
type1.rf    = confusionMatrix(table(predTest_3, test_rf$success),positive="S")$table[2,1]
type2.rf    = confusionMatrix(table(predTest_3, test_rf$success),positive="S")$table[1,2]

# Results of XGBoost
accuracy.xgb = confusionMatrix(table(xgbpred, ts_label), positive="1")$overall["Accuracy"]
sens.xgb     = confusionMatrix(table(xgbpred, ts_label), positive="1")$byClass["Sensitivity"]
spe.xgb      = confusionMatrix(table(xgbpred, ts_label), positive="1")$byClass["Specificity"]
type1.xgb    = confusionMatrix(table(xgbpred, ts_label), positive="1")$table[2,1]
type2.xgb    = confusionMatrix(table(xgbpred, ts_label), positive="1")$table[1,2]


# Results data frame
modelnames  = c("Full logistic regression", "Reduced logistic regression", "Rpart", "Ctree", "Random forest", "XGBoost")
accuracy    = c(accuracy.full, accuracy.reduced, accuracy.rpart, accuracy.inf, accuracy.rf, accuracy.xgb)
sensitivity = c(sens.full, sens.reduced, sens.rpart, sens.inf, sens.rf, sens.xgb)
specificity = c(spe.full, spe.reduced, spe.rpart, spe.inf, spe.rf, spe.xgb)
Type1       = c(type1.full, type1.reduced, type1.rpart, type1.inf, type1.rf, type1.xgb)
Type2       = c(type2.full, type2.reduced, type2.rpart, type2.inf, type2.rf, type2.xgb)
result.df   = data.frame(modelnames, accuracy, sensitivity, specificity, Type1, Type2)

result.df

