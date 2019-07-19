library(lattice)
library(ggplot2)
library(ROCR)
library(rpart)
library(party)


# Building a conditional inference tree
new_train         = train
new_train$success = ifelse(new_train$success == 1, "S", "B")
new_train$success = as.factor(new_train$success)

ctree.model =  ctree(success ~ . , data = new_train)
ctree.model
new_test    = test


#plot(ctree.model)
ctree.predict = predict(ctree.model, new_test)
table(ctree.predict, new_test$success)
ctree.predict = ifelse(ctree.predict == "S", 1, 0)
confusionMatrix(table(ctree.predict, new_test$success), positive = "1")

options(repr.plot.width = 5, repr.plot.height = 5) # setting initial plot area dimensions
pred_obj_inf = prediction(ctree.predict, new_test$success) 
perf_obj_inf = performance(pred_obj_inf, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_inf = performance(pred_obj_inf, "auc")@y.values[[1]]
plot(perf_obj_inf)
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_inf,3))))



