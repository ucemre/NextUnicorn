# HELPER FUNCTIONS # 

# Combining social media appearance
has_social  = function(df){
  df$social = ifelse(!is.na(df$twitter_url) & !is.na(df$facebook_url), "Both",
                      ifelse(!is.na(df$twitter_url), "Twitter",
                             ifelse(!is.na(df$facebook_url), "Facebook", "None")))
}

# Print metrics
print_metrics = function(lr, df, score, label){
  resids      = df [, label] - score
  resids2     =  resids**2
  N           = length(score)
  r2          =  as.character(round(summary(lr)$r.squared))
  adj_r2      = as.character(round(summary(lr)$adj.r.squared, 4))
  cat(paste('Mean Square Error      =', as.character(round(sum(resids2)/N, 4)), " \n"))
  cat(paste('Root Mean Square Error =', as.character(round(sqrt(sum(resids2)/N), 4)), " \n"))
  cat(paste('Mean Absolute Error    =', as.character(round(sum(abs(resids2)/N), 4)), " \n"))
  cat(paste('Median Absolute Error  =', as.character(round(sum(median(resids)/N), 4)), " \n"))
  cat(paste('R^2                    =', r2, ' \n'))
  cat(paste('Adjusted R^2           =', adj_r2, " \n"))
}

# Histogram of Residuals

hist_resids = function(df, score, label, bins = 10) {
  par(bg = NA)
  options(repr.plot.width =4, repr.plot.height = 3) # setting initial plot area dimensions
  df$resids = df[, label] - score
  bw        = (max(df$resids) - min(df$resids))/(bins + 1)
  ggplot(df, aes(resids))+
    geom_histogram(binwidth = bw, aes(y =..density..), alpha = 0.5) +
    geom_density(aes(y =..density..), color ="blue") +
    xlab("Residual Value") + ggtitle("Histogram of residuals")
}


# Q-Q Plot of Residuals
resids_qq   = function(df, score, label) {
  options(repr.plot.width = 4, repr.plot.height = 3) # setting initial plot area dimensions
  df$resids = df[, label] - score
  ggplot() +
    geom_qq(data = df, aes(sample = resids)) +
    ylab("Quantiles of residuals") + xlab("Quantiles of standard normal") +
    ggtitle("QQ plot of residual values")
}

# Score model
score_model = function(df, threshold){
  df$score  = ifelse(df$probs > threshold, 1, 0)
  df
}

# Logistic evaluation
logistic.eval =  function (df){
  df$conf     = ifelse(df$success == 1 & df$score == 1, "TP",
                ifelse(df$success == 0 & df$score == 1, "FP",
                ifelse(df$success == 0 & df$score == 0, "TN", "FN")))
  
  TP = length(df[df$conf == "TP", "conf"])
  FP = length(df[df$conf == "FP", "conf"])
  TN = length(df[df$conf == "TN", "conf"])
  FN = length(df[df$conf == "FN", "conf"])
  
  # Confusion matrix as data frame
  out             = data.frame(Negative = c(TN, FN), Positive = c(FP, TP))
  row.names(out)  =  c("TrueNeg", "TruePos")
  print(out)
  
  # Compute and print metrics
  P   = TP / (TP + FP)
  R   = TP / (TP + FN)
  F1  = 2*P*R / (P+R)
  cat("\n")
  cat(paste("Accuracy               =", as.character(round((TP + TN) / (TP + TN + FP + FN), 3)), "\n"))
  cat(paste("Precision              =", as.character(round(P, 3)), "\n"))
  cat(paste("Recall/Sensitivity     =", as.character(round(R, 3)), "\n"))
  cat(paste("F1                     =", as.character(round(F1, 3)), "\n"))
  cat(paste("Error rate             =", as.character(round(1 - (TP + TN) / (TP + TN + FP + FN), 3)), "\n"))
  
}

# ROC and AUC
ROC_AUC     = function(df){
  options(repr.plot.width = 5, repr.plot.height = 5) # setting initial plot area dimensions
  pred_obj  = prediction(df$probs, df$success) 
  perf_obj  = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
  AUC       = performance(pred_obj, "auc")@y.values[[1]]
  plot(perf_obj)
  abline(a = 0, b = 1, col = "red")
  text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))
}


