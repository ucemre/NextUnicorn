library(lattice)
library(ggplot2)
library(corrplot)
library(xgboost)
library(randomForest)

# XGB Application

labels   = train$success
ts_label = test$success

new_tr = model.matrix(~.+0,data = train[,-1 ]) 
new_ts = model.matrix(~.+0,data = test[,-1 ])

dtrain = xgb.DMatrix(data = new_tr,label = labels) 
dtest  = xgb.DMatrix(data = new_ts,label = ts_label)

params = list(booster = "gbtree", objective = "binary:logistic", eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1, 
               subsample = 1, colsample_bytree = 1)


# finding the optimal number of iteration rounds ( stops if the error doen not decrease in 20 consecutive iterations)
xgbcv = xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stopping_rounds = 20, maximize = F)

# best iteration 72

min(xgbcv$evaluation_log$test_error_mean) # 0.047908
# therefore CV accuracy is 1-0.047908 = 0.952092

xgb1 = xgb.train (params = params, data = dtrain, nrounds = 129, watchlist = list(val = dtest,train = dtrain), 
                   print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")
#model prediction
xgbpred = predict (xgb1,dtest)
summary(xgbpred)
xgbpred = ifelse (xgbpred > 0.6,1,0) 

xgbpred_df  =  as.data.frame(xgbpred)
ts_label_df = as.data.frame(ts_label)

confusionMatrix(table(xgbpred, ts_label), positive = "1")

# Printing area under the curve for XGB
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_xgb = prediction(xgbpred, ts_label) 
perf_obj_xgb = performance(pred_obj_xgb, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_xgb = performance(pred_obj_xgb, "auc")@y.values[[1]]
plot(perf_obj_xgb)
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_xgb,3))))

mat = xgb.importance (feature_names = colnames(new_tr), model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])