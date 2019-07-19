library(lattice)
library(ggplot2)
library(corrplot)
library(caret)
library(ROCR)
library(rpart)
library(party)
library(rpart.plot)

### Rpart IMPLEMENTATION ###


train.tree = train
test.tree  = test

sf.rp      = rpart(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + 
                  social.Both + social.Facebook + social.Twitter+ continent.Americas +continent.Asia+ 
                  sector.cons_disc + sector.cons_stap+sector.health , data = train.tree, 
                  control=rpart.control(minsplit=2, minbucket = 2, cp=0.001), method = "class")


sf.rp$variable.importance

pred.rp = predict(sf.rp, test.tree, type="class")

table(test.tree$success, pred.rp)
confusionMatrix(table(pred.rp, test.tree$success), positive="1")

options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_rpart = prediction(as.numeric(pred.rp), as.numeric(test.tree$success)) 
perf_obj_rpart = performance(pred_obj_rpart, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_rpart      = performance(pred_obj_rpart, "auc")@y.values[[1]]
plot(perf_obj_rpart, main = "Rpart AUC")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_rpart,3))))

rpart.plot(sf.rp, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Pruning the recursice partioning tree - doesnt change the initial split
#Finding the min cross-validation error of the reg. tree model
min(sf.rp$cptable[, "xerror"]) # 0.141085
# locating the record with the min cross-validation errors
which.min(sf.rp$cptable[, "xerror"]) # 25
# getting the cost complexity parameter
sf.cp <- sf.rp$cptable[25, "CP"]
sf.cp # 0.001

prune.tree =  prune(sf.rp, cp= sf.cp)
plot(prune.tree, margin = 0.0001)
text(prune.tree, all = TRUE, use.n = TRUE)
pred.rpp  = predict(prune.tree, test.tree, type= "class")
pred.rpp  = ifelse(pred.rpp > 0.7, 1, 0)
table(test.tree$success, pred.rpp)
confusionMatrix(table(pred.rpp, test.tree$success))



