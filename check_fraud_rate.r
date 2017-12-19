library(tavR)
library(dplyr)
library(sqldf)
library(data.table)
library(tidyr)
library(plyr)
setwd("~/R/_________________")
  
# sql <-"_________________"
# df <- tavR(sql)
df <- read.delim("~/R/_________________", header=TRUE, sep=",")

##################################################################################

#loops through dataframe from file
for(i in colnames(df)) {
  d = data.frame(df[,i]) #creates dataframe for each column name
  colnames(d) <- paste(i)
  d$fraud <- df$trans_is_fraud #adds a column for indicating if itinerary was fraud
  d$non_fraud <- df$trans_is_fraud #adds a column for indicating if itinerary was not fraud
  d <- within(d, {
    non_fraud <- ifelse(non_fraud == 1, 0, 1)
  })
  groupColumns <- c(i) # Groups colums by variable
  dataColumns <- c(colnames(d[,2:ncol(d)]))
  d <- ddply(d, groupColumns, function(x) colSums(x[dataColumns]))
  d$fraud_rate <- (d$fraud/(d$fraud+d$non_fraud)) # Adds a fraud rate column
  d <-  d[order(-d$fraud_rate),] # Sorts rows by fraud rate
  d <- d[!(d$fraud_rate < 0.2),] # Removes any rows with a fraud rate of less than 10%
  d <- d[!(d$fraud < 3),] # Removes any rows with less than 3 instances of fraud
  assign(paste("df_", i, sep = ""), d)
}

################################################################################

# Removes any dataframes that have 0 rows
isEmpty <- function(x) {
  is.data.frame(x) && nrow(x) == 0L
}
empty <- unlist(eapply(.GlobalEnv, isEmpty))
rm(list = names(empty)[empty])
