library(dplyr)
library(tidyr)
library(plyr)
library(tavR)
library(sqldf)
library(RH2)
library(data.table)

old <- Sys.time() # get start time
# setwd("~/R/______________________")

##################################################################################
 
sql <- "______________________C"
df1 <- wh_query(sql)
df1$Transaction_ID <- as.character(df1$Transaction_ID)

# Reads .DAT file
df2 <- read.delim("~/R/______________________", header=TRUE, sep=",")
df2$Transaction_ID <- as.character(df2$Transaction_ID)
df2$Transaction_ID <- substring(df2$Transaction_ID, 4)
df <- merge(df1, df2, by="Transaction_ID")

# Takes the dataframe with a column that includes rules tripped/points and converts it
# to dataframe with itinerary and indiviual rule tripped on each row
s <- strsplit(as.character(df$scoring7_rules_tripped), split = ";")
df <- data.frame(V1 = rep(df$Transaction_ID, sapply(s, length)), V2 = rep(df$trans_is_fraud, sapply(s, length)), V3 = unlist(s))
# Seperates rules rule tripped and points for each row
df <- df%>%
          separate(V3, c("rule_tripped", "rule_points"), ":")
          
# Renames the first column and drops unneccessary columns
colnames(df)[1] <- "itinerary_id"
colnames(df)[2] <- "trans_is_fraud"
df <- df[,c(1:3)]
 
# Creates a list of unique rules tripped
unique_rules <- list(unique(df$rule_tripped))
# Loops through and adds empty columns for each unique rule tripped
for(i in unique_rules){
  df[,i] <- NA
}
 
# Splits Data to Fraud and Non Fraud
fraud <- df[df$trans_is_fraud == 1,]
non_fraud <- df[df$trans_is_fraud != 1,]
fraud$trans_is_fraud <- NULL
non_fraud$trans_is_fraud <- NULL
 
# Working function. Need to put into for loop
#df3[,4] <- ifelse(df3$rule_tripped %in% list(colnames(df3[4])), 1, 0)
# Loops through and adds a 1 to each row if the column header matches the rule tripped
for(i in 3:ncol(fraud)){
  fraud[,i] <- ifelse(fraud$rule_tripped %in% list(colnames(fraud[i])), 1, 0)
}
for(i in 3:ncol(non_fraud)){
  non_fraud[,i] <- ifelse(non_fraud$rule_tripped %in% list(colnames(non_fraud[i])), 1, 0)
}
fraud$rule_tripped <- NULL
non_fraud$rule_tripped <- NULL
fraud <- as.data.table(fraud)
non_fraud <- as.data.table(non_fraud)

# Groups all rules tripped by itinerary_id
groupColumns <- c("itinerary_id")
dataColumns <- c(colnames(fraud[,3:ncol(fraud)]))
fraud <- ddply(fraud, groupColumns, function(x)colSums(x[dataColumns]))
dataColumns <- c(colnames(non_fraud[,3:ncol(non_fraud)]))
non_fraud <- ddply(non_fraud, groupColumns, function(x)colSums(x[dataColumns]))
 
fraud <- as.data.frame(colSums(fraud[,-1]))
non_fraud <- as.data.frame(colSums(non_fraud[,-1]))
fraud <- as.data.frame(fraud)
non_fraud <- as.data.frame(non_fraud)
fraud <- cbind(rownames(fraud), fraud)
rownames(fraud) <- NULL
colnames(fraud) <- c("rule_no", "fraud")
non_fraud <- cbind(rownames(non_fraud), non_fraud)
rownames(non_fraud) <- NULL
colnames(non_fraud) <- c("rule_no", "non_fraud")
df_rules <- merge(fraud, non_fraud, by="rule_no")
df_rules$total_tripped  <- df_rules$fraud+df_rules$non_fraud
df_rules$fraud_rate <- df_rules$fraud/df_rules$total_tripped
fraud_rates_sum <- sum(df_rules$fraud_rate)
df_rules$weight <- df_rules$fraud_rate/fraud_rates_sum
df_rules$points <- as.integer(df_rules$weight*20000)
df_rules$rule_no <- as.character(df_rules$rule_no)
rm(fraud, non_fraud, df)
rules_lookup <- read.csv("______________________")
colnames(rules_lookup) <- c("rule_no", "rule_description")
rules_lookup$rule_no <- as.character(rules_lookup$rule_no)
df <- merge(df_rules, rules_lookup, by="rule_no")


##############################################################
write.csv(df_rules, file = "______________________")
# print elapsed time
new <- Sys.time() - old # calculate difference
print(new) # print in nice format
