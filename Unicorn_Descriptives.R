library(countrycode)
library(dplyr)
library(tidyr)
library(lattice)
library(ggplot2)
library(caret)

# Input data frame cdf is the output of "Unicorn_DataCleaning"

cdf %>% group_by(success) %>% summarise(avg = mean(company_age))
cdf %>% group_by(success) %>% summarise(avg = mean(funding_total_usd))
cdf %>% group_by(success) %>% summarise(avg = mean(funding_rounds))
cdf %>% group_by(success) %>% summarise(avg = mean(first_funding_lag))
cdf %>% group_by(success) %>% summarise(avg = mean(lastFundingtoDate))
cdf %>% group_by(success) %>% summarise(avg = mean(last_funding_lag))

cdf %>% group_by(success) %>% summarise(avg = median(company_age))
cdf %>% group_by(success) %>% summarise(avg = median(funding_total_usd))
cdf %>% group_by(success) %>% summarise(avg = median(funding_rounds))
cdf %>% group_by(success) %>% summarise(avg = median(first_funding_lag))
cdf %>% group_by(success) %>% summarise(avg = median(lastFundingtoDate))
cdf %>% group_by(success) %>% summarise(avg = median(last_funding_lag))

names(cdf)
mean(cdf$company_age)
mean(cdf$funding_total_usd)
mean(cdf$first_funding_lag)
mean(cdf$funding_rounds)
mean(cdf$lastFundingtoDate)
length(unique(cats$cat1))

table(ncdf$success, ncdf$social.None)
table(ncdf$success, ncdf$continent.Europe)
table(ncdf$success, ncdf$sector.)

cdf_desc <- cdf
cdf_desc$success <- ifelse(cdf_desc$success ==1, "Successful", "Failure")

cdf_desc <- cdf_desc[cdf_desc$first_funding_lag >=0, ]
cdf_desc <- cdf_desc[cdf_desc$last_funding_lag <=20, ]
cdf_desc <- cdf_desc[cdf_desc$funding_rounds <=10, ]

class(ncdf)

ncdf_desc <- sapply(ncdf, as.numeric)

cor_values <- cor(ncdf_desc)


pdf("boxplot_vertical.pdf", width = 6, height = 9)
par(mar = c(5, 5, 5, 5), mfrow = c(3,2))
boxplot(company_age~success,data = cdf_desc,     main = "Company Age", 
   xlab = "Company Status", ylab = "Company Age")
boxplot(funding_total_usd~success, data=cdf_desc, main = "Total Funding (USD)", 
   xlab = "Company Status", ylab="Total Funding (USD)")
boxplot(funding_rounds~success, data=cdf_desc,    main = "Funding Rounds", 
   xlab = "Company Status", ylab="Funding Rounds")
boxplot(first_funding_lag~success, data=cdf_desc, main = "First funding lag", 
   xlab = "Company Status", ylab="First funding lag")
boxplot(lastFundingtoDate~success, data=cdf_desc, main = "Last funding to date", 
   xlab = "Company Status", ylab="Last funding to date")
boxplot(last_funding_lag~success, data=cdf_desc,  main = "Last funding lag", 
   xlab = "Company Status", ylab="Last funding lag")
dev.off()


