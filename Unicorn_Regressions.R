library(ggplot2)
library(corrplot)
library(ROCR)
library(pscl)
# Logistic regression with fullmodel
train_long = train
test_long  = test

names(train)
long_col = c("funding_rounds", "company_age", "first_funding_lag", "lastFundingtoDate", "funding_total_usd", "last_funding_lag", 
              "social.Both", "social.Facebook", "social.Twitter", "social.None", "continent.Americas", 
              "continent.Asia", "continent.Europe", "sector.comm_serv", "sector.cons_disc", "sector.cons_stap", "sector.financials", 
              "sector.health", "sector.it")


lr_all = glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                last_funding_lag + social.Both + social.Facebook + social.Twitter + social.None + continent.Americas +  
                continent.Asia + continent.Europe + sector.comm_serv + sector.cons_disc + sector.cons_stap + sector.financials 
              + sector.health + sector.it  ,data = train_long, family = "binomial")

summary(lr_all)
lr_all_coef = summary(lr_all)$coefficients

test_long$probs = predict(lr_all, newdata = test_long, type = "response")
test_long[1:20, c("success", "probs")]
summary(test_long$probs)

hist_resids(test_long, test_long$probs, label = "success")

resids_qq(test_long, test_long$probs, label = "success")

logistic.eval(test_long)
ROC_AUC(test_long)
confusionMatrix(table(test_long$score, test_long$success), positive = "1")

#removing NA variables - indicates  linear dependency
train_v2  = train
test_v2   = test
lr_all_v2 = glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                   social.Both + social.Facebook + social.Twitter + continent.Americas +  
                   continent.Asia  + sector.comm_serv + sector.cons_disc + sector.cons_stap + sector.financials 
                 + sector.health   ,data = train_v2, family = "binomial")
table(train_v2$social.Both)
summary(lr_all_v2)

### REDUCED LOGISTIC REGRESSION ####

#taking the significant indep variables from v2
lr_all_v3 = glm(success ~ funding_rounds + company_age +  lastFundingtoDate  + funding_total_usd +
                   social.Both + social.Facebook + social.Twitter + continent.Americas +  
                   sector.comm_serv + sector.cons_disc  + sector.cons_stap + sector.health,
                 data = train_v2, family = "binomial")

#McFaddens R-squared

pR2(lr_all_v3)
pR2(lr_all)
lr_all_v3_coeff = summary(lr_all_v3)$coefficients

summary(lr_all_v3)

test_v2$probs = predict(lr_all_v3, newdata = test_v2, type = "response")

hist_resids(test_v2, test_v2$probs, label = "success")

resids_qq(test_v2, test_v2$probs, label = "success")

confusionMatrix(table(test_v2$score, test_v2$success), positive = "1")

logistic.eval(test_v2)
par(mfrow = c(1,1))
ROC_AUC(test_v2) 



