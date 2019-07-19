library(countrycode)
library(dplyr)
library(tidyr)
library(lattice)
library(ggplot2)
library(corrplot)
library(caret)
library(ROCR)
library(rpart)
library(party)
library(imbalance)

# Importing helper functions for data cleaning
source("helperFunctions.R")

# Importing data
data    = read.csv("20181031_crunchbase_export_firms.csv", sep = ";", na.strings=c("","NA"))
# n=215729, 23 variables 
data_bu = data

# Deleting the columns with no useful information
data    = data[ , -which(names(data) %in% c("phone","email", "state_code", "region"))]

# Filtering out the companis with no funding information
data    = data[!is.na(data$funding_total_usd), ]

data    = data[!is.na(data$funding_rounds), ]

# Adjusting vartiable types
data$funding_rounds     = as.numeric(data$funding_rounds)
data$funding_total_usd  = as.numeric(data$funding_total_usd)

# Transforming year founded and funding information to continous variables and adjusting variable type
data = separate(data, founded_on,       sep = "-", into = c("year_founded", "month_founded"))
data = separate(data, first_funding_on, sep = "-", into = c("first_funding", "month_f_funding"))
data = separate(data, last_funding_on,  sep = "-", into = c("last_funding", 'month_l_funding'))
data = separate(data, closed_on,        sep = "-", into = c("closing_date", "closing_month"))

data$year_founded   = as.numeric(data$year_founded)
data$first_funding  = as.numeric(data$first_funding)
data$last_funding   = as.numeric(data$last_funding)
data$closing_date   = as.numeric(data$closing_date)

# We assume that companies <10 years of age are considered as start-up
data = data[data$year_founded >2009,] # only 61430 firms are founded after 2009

data = data[!is.na(data$year_founded), ] # 53287 firms have year founded information

data = data[!is.na(data$company_name), ] # 53287 firms have company name information

data = data[!is.na(data$domain), ] # 51606 of the companies stated domains

dta$company_age         = 2019 - data$year_founded
data$first_funding_lag  = data$first_funding - data$year_founded
data$last_funding_lag   = data$last_funding - data$first_funding 
data$lastFundingtoDate  = 2019 - data$last_funding
data$closing_age        = data$closing_date - data$year_founded + 1 # will be deleted after descriptives


# closed companies with missing closing date information are assumed to go out of business within the same year of founding. Therefore  
data$closing_age = ifelse(data$status == "closed" & is.na(data$closing_age), 1, data$closing_age)
# companies which are acquired, ipod or still operating are assigned closing age of 0
data$closing_age = ifelse(data$status != "closed" , 0, data$closing_age)

df = data

# Merging social media existance
data$twitter_url  = as.character(data$twitter_url)
data$facebook_url = as.character(data$facebook_url)

social  = has_social(data)
data    = cbind(data, social)

df = data

# Parsing industry categories

data = data[!is.na(data$category_group_list), ] # 50978 companies states business category / industry

cats = separate(data, category_group_list, sep = "\\|", into = c("cat1", 'cat2', 'cat3', 'cat4', 'cat5', 'cat6', 
                                                                'cat7', 'cat8', 'cat9', 'cat10', 'cat11', "cat12", "cat13", "cat14", 'cat15', "cat16"), remove = FALSE)

cats = cats[, which(names(cats) %in% c("cat1", 'cat2', 'cat3', 'cat4', 'cat5', 'cat6', 
                                        'cat7', 'cat8', 'cat9', 'cat10', 'cat11', "cat12", "cat13", "cat14", 'cat15', "cat16"))]

cats_count = sapply(cats, function(x) sum(!is.na(x)))

par(mar = c(5, 5, 5, 5), mfrow = c(1,1))
barplot(cats_count, main = 'Category Overview', xlab = 'Category number', ylab = 'Count', ylim = c(0, 50000))

cat1_counts = sort(table(cats$cat1), decreasing = TRUE)
cat1_counts = as.data.frame(cat1_counts)

top_cat1          = top_n(cat1_counts, 10) # commerce and shopping (5431), apps (4576) and financial services ( 3687) are top 3 most freq industries
top10_cat1        = top_cat1[, "Freq"]
names(top10_cat1) = top_cat1$Var1

par(mar = c(5, 11, 5, 1))
barplot(top10_cat1, main = 'Top 10 Primary category', xlab = 'Count', xlim =c(0, 6000), horiz = TRUE, las =2, bg = "transparent")

data = cbind(data, cats$cat1)

colnames(data)[colnames(data) == "cats$cat1"] <- "category"

data$sector = ifelse(data$category == "advertising"                       | data$category == "content and publishing" | # commercial services
                      data$category == "design"                           | data$category == "events" | 
                      data$category == "internet services"                | data$category == "media and entertainment" |  
                      data$category == "messaging and telecommunications" | data$category == "mobile" | 
                      data$category == "professional services", "comm_serv", 
               ifelse(data$category == "administrative services" | data$category == "clothing and apparel" | #consumer discretionary
                      data$category == "commerce and shopping"   | data$category == "consumer goods" | 
                      data$category == "gaming"                  | data$category == "travel and tourism" | data$category == "sales and marketing" , "cons_disc", 
               ifelse(data$category == "community and lifestyle" | data$category == "consumer electronics" | 
                      data$category == "education"               | data$category == "food and beverage" , "cons_stap", # consumer staples
               ifelse(data$category == "energy"                  | data$category == "sustainability" , "energy",  # energy
               ifelse(data$category == "financial services"      | data$category == "payments","financials", # financials
               ifelse(data$category == "agriculture and farming" | data$category == "biotechnology"|
                      data$category == "health care"             | data$category == "sports","health", # health
               ifelse(data$category == "government and military" | data$category == "manufacturing"|
                      data$category == "science and engineering" | data$category == "transportation","industrials", # industrials
               ifelse(data$category == "apps"                    | data$category == "artificial intelligence"| data$category == "data and analytics"|
                      data$category == "hardware"                | data$category == "information technology" |
                      data$category == "navigation and mapping"  | data$category == "platforms" |data$category == "privacy and security" |
                      data$category == "software" ,"it", # IT
               ifelse(data$category == "real estate","realestate", # real estate
               ifelse(data$category == "natural resources","utilities", "")))))))))) # utilities

data = data[ , -which(names(data) %in% c("founded_on","first_funding_on", "last_funding_on","closing_date", "cb_url", "twitter_url", "facebook_url", "category_list", 
                                          "category_group_list", "short_description", "category"))]
df = data


# Checking for duplicates and cleaning them
length(unique(data$domain)) # there are 56 repeating domains

table(duplicated(data$domain), data$sector)

dup_domains = data[duplicated(data$domain) == TRUE, "domain"]

dubs        = data[data$domain %in% dup_domains, "sector" ]

data$unique = paste(as.character(data$domain), as.character(data$sector), sep = "_")

length(unique(data$unique)) # 50962 -> 16 duplicates
table(duplicated(data$unique), data$sector)

data        = data[!duplicated(data$unique), ] # 16 duplicates are removed

# Transforming country codes to continents
ata$country_code = as.character(data$country_code)
data             = data[!is.na(data$country_code), ] # 1436 companies do not have location information

data$country_code[data$country_code == "ROM"] =  "ROU"
data$country_code[data$country_code == "BAH"] =  "BHS"
data$country_code[data$country_code == "TAN"] =  "TZA"

data$country    = countrycode(data$country_code, "iso3c", "country.name")
data$continent  = as.factor(countrycode(data$country_code, "iso3c", "continent"))

data$success = 1
data$success[as.character(data$status) == "closed"] = 0

cdf = data[ , -which(names(data) %in% c("company_name","domain", "country_code","city", "employee_count", 
                                         "unique", "country", "dub", "year_founded", "first_funding", "last_funding", 
                                         "status", "cats$cat1", "uuid", "closed_on", "closing_date", "month_founded",
                                         "month_f_funding", "month_l_funding", "closing_month" ))]
# Removing outliers
cdf = cdf[cdf$first_funding_lag >= 0,  ] 
cdf = cdf[cdf$last_funding_lag  <= 20, ]
cdf = cdf[cdf$funding_rounds    <= 10, ]

dummies <- dummyVars(success ~ ., data = cdf )
dummy_frame <- data.frame(predict(dummies, newdata = cdf))
success_dummies <- predict(dummies, newdata = cdf)
head(success_dummies)

# We want to find the weather the dummies have zero varince or not. Cut frequency - most frequently encountered value over most frequently encountered value
# We need 10 different appearances
near_zero = nearZeroVar(success_dummies, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
near_zero[near_zero$zeroVar == TRUE | (near_zero$nzv == TRUE), ] # top 3 are closing_age, continent.africa and continent oceania

cdf           = cdf[cdf$continent != "Oceania" & cdf$continent != "Africa", ]
cdf$continent = droplevels(cdf$continent)
table(cdf$continent)
cdf           = cdf[cdf$sector != "energy" & cdf$sector != "industrials" & cdf$sector != "utilities" & cdf$sector != "realestate", ]
cdf$sector    = droplevels(cdf$sector)

dummies         = dummyVars(success ~ ., data = cdf )
success_dummies = predict(dummies, newdata = cdf)

success_dummies = as.data.frame(success_dummies)

ncdf = as.data.frame(cbind(success = cdf$success, success_dummies))
ncdf = ncdf[, -which(names(ncdf) %in% c("closing_age"))]

# Splitting train and test data sets
train_inv = sample(seq_len(nrow(ncdf)), size = 0.7*nrow(ncdf))
train     = ncdf[train_inv,  ]
test      = ncdf[-train_inv, ]

#Oversampling the unsuccessful companies to deal with imbalanced data in train and test datasets
train = oversample(train, ratio=0.50, method = "ADASYN", filtering=FALSE, classAttr= "success", wrapper="KNN")
dim(train)
test = oversample(test, ratio=0.50, method = "ADASYN", filtering=FALSE, classAttr= "success", wrapper="KNN")
names(train)

# Define binary variables for train and set set
train$social.Both        = ifelse(train$social.Both >0.5, 1, 0)
train$social.Facebook    = ifelse(train$social.Facebook >0.5, 1, 0)
train$social.None        = ifelse(train$social.None >0.5, 1, 0)
train$social.Twitter     = ifelse(train$social.Twitter >0.5, 1, 0)
train$sector.comm_serv   = ifelse(train$sector.comm_serv >0.5, 1, 0)
train$sector.cons_disc   = ifelse(train$sector.cons_disc >0.5, 1, 0)
train$sector.cons_stap   = ifelse(train$sector.cons_stap >0.5, 1, 0)
train$sector.financials  = ifelse(train$sector.financials >0.5, 1, 0)
train$sector.health      = ifelse(train$sector.health >0.5, 1, 0)
train$sector.it          = ifelse(train$sector.it >0.5, 1, 0)
train$continent.Americas = ifelse(train$continent.Americas >0.5, 1, 0)
train$continent.Asia     = ifelse(train$continent.Asia >0.5, 1, 0)
train$continent.Europe   = ifelse(train$continent.Europe >0.5, 1, 0)

test$social.Both        = ifelse(test$social.Both >0.5, 1, 0)
test$social.Facebook    = ifelse(test$social.Facebook >0.5, 1, 0)
test$social.None        = ifelse(test$social.None >0.5, 1, 0)
test$social.Twitter     = ifelse(test$social.Twitter >0.5, 1, 0)
test$sector.comm_serv   = ifelse(test$sector.comm_serv >0.5, 1, 0)
test$sector.cons_disc   = ifelse(test$sector.cons_disc >0.5, 1, 0)
test$sector.cons_stap   = ifelse(test$sector.cons_stap >0.5, 1, 0)
test$sector.financials  = ifelse(test$sector.financials >0.5, 1, 0)
test$sector.health      = ifelse(test$sector.health >0.5, 1, 0)
test$sector.it          = ifelse(test$sector.it >0.5, 1, 0)
test$continent.Americas = ifelse(test$continent.Americas >0.5, 1, 0)
test$continent.Asia     = ifelse(test$continent.Asia >0.5, 1, 0)
test$continent.Europe   = ifelse(test$continent.Europe >0.5, 1, 0)











