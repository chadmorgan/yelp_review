#-----------------------------------------------------------------------------------------------------------------------------------------
# Author: Chad Morgan
# Project: Predictive modeling class
# Purpose: Prepare Yelp review data for homework
#
#   Note: Another script already converted the data from JSON to CSV files.
#       This script is mainly for data cleaning and to split off a testing data set to predict (since the final testing data isn't available through kaggle)
#-----------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls()) # clear R workspace
memory.limit(size=8192)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------------------------------------------
# import reveiws data
yelp_reviews<-read.csv('P:/Innovation/predictive modeling training/yelp review exercise/yelp_training_set_review.csv',stringsAsFactors=FALSE)

# reorder columns
yelp_reviews<-yelp_reviews[,c('review_id','user_id','business_id','date','stars','votes_useful','votes_cool','votes_funny','text')]
# convert date from char format to date format
yelp_reviews$date<-as.Date(yelp_reviews$date)

# split data into testing and training set
yelp_reviews$random <- runif(dim(yelp_reviews)[1])
yelp_reviews$train <- ifelse(yelp_reviews$random>.2,1,0)

# remove carriage returns from text field
yelp_reviews$text<-gsub(pattern='\n', replacement=' ',yelp_reviews$text)

# training data 
train.full <- yelp_reviews[yelp_reviews$train==1,c('review_id','user_id','business_id','date','stars','votes_useful','votes_cool','votes_funny','text')]
# test data with answers
test.full <- yelp_reviews[yelp_reviews$train==0,c('review_id','user_id','business_id','date','stars','votes_useful','votes_cool','votes_funny','text')]
# test data without answers
test.partial <- yelp_reviews[yelp_reviews$train==0,c('review_id','user_id','business_id','date','stars','text')]

# write to csv
write.csv(train.full,'P:/Innovation/predictive modeling training/yelp review exercise/review_training.csv',row.names=FALSE)
write.csv(test.full,'P:/Innovation/predictive modeling training/yelp review exercise/review_testing_wAnswers.csv',row.names=FALSE)
write.csv(test.partial,'P:/Innovation/predictive modeling training/yelp review exercise/review_testing.csv',row.names=FALSE)

#-----------------------------------------------------------------------------------------------------------------------------------------
# import businesses data
yelp_business<-read.csv('P:/Innovation/predictive modeling training/yelp review exercise/yelp_training_set_business.csv',stringsAsFactors=FALSE)

# reorder the columns and drop those without any information
yelp_business<-yelp_business[,c('business_id','name','open','full_address','city','state','latitude','longitude','review_count','stars','categories')]

# remove carriage returns from address field
yelp_business$full_address<-gsub(pattern='\n', replacement=' ',yelp_business$full_address)

# write to csv
write.csv(yelp_business,'P:/Innovation/predictive modeling training/yelp review exercise/business.csv',row.names=FALSE)

#-----------------------------------------------------------------------------------------------------------------------------------------
# import user data
yelp_user<-read.csv('P:/Innovation/predictive modeling training/yelp review exercise/yelp_training_set_user.csv',stringsAsFactors=FALSE)

# reorder the columns and drop those without any information
yelp_user<-yelp_user[,c('user_id','name','review_count','average_stars','votes_cool','votes_funny','votes_useful')]

# write to csv
write.csv(yelp_user,'P:/Innovation/predictive modeling training/yelp review exercise/user.csv',row.names=FALSE)
