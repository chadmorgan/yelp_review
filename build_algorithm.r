
#-----------------------------------------------------------------------------------------------------------------------------------------
#   Name: build_algorithm.r
#   Author: Chad Morgan
#   Purpose: Build an algorithm to predict useful votes for a Yelp review
#
#-----------------------------------------------------------------------------------------------------------------------------------------


rm(list = ls()) # clear R workspace
memory.limit(size=8192)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------------------------------------------
# read in training data and do initial formatting
reviews<-read.csv('/home/chadm/yelp/review_training.csv',stringsAsFactors=FALSE)
reviews$text<-gsub("/N"," ",reviews$text)
reviews$date<-as.Date(reviews$date)

# split off another testing set
reviews$test<-ifelse(runif(dim(reviews)[1])>.8,1,0)
test_set <- unique(reviews[reviews$test==1,'review_id'])
train_set <- setdiff(unique(reviews$review_id),test_set)
traindat<-reviews[reviews$test==0,]

# initial variable creation
traindat$review_length<-nchar(traindat$text) # length of text
traindat$log_useful <- log(traindat$votes_useful+1)	# log useful votes, which will the response variabel we build an algorithm to predict

	
#-----------------------------------------------------------------------------------------------------------------------------------------
# Exploratory Data Analysis with data visualization
summary(traindat)
ggplot(data=traindat,aes(x=log_useful))+geom_density() 											# useful votes is very skewed
ggplot(data=traindat,aes(x=date,y=log_useful))+geom_point()+geom_smooth()						# usefulness increased initially, peaked around 2008, then tailed off
ggplot(data=traindat,aes(x=factor(stars),y=log_useful,color=factor(stars)))+geom_boxplot()		# 
aggregate(x=traindat$log_useful,by=list(traindat$stars),FUN=mean)								# negative reviews are slightly more useful
ggplot(data=traindat,aes(x=review_length,y=votes_useful))+geom_point()+geom_smooth()			# longer reviews more useful. Non-linear, monotonically increasing (try log transform)


#-----------------------------------------------------------------------------------------------------------------------------------------
# import and explore user data
users<-read.csv('/home/chadm/yelp/user.csv',stringsAsFactors=FALSE)

# add date of first review for each user
first_review_user <- aggregate(reviews$date,by=list(reviews$user_id),FUN=min)
colnames(first_review_user)<-c('user_id','first_review_user')
users<-merge(x=users,y=first_review_user,by='user_id',in.x=TRUE)

# more derived features by user
users$avg_useful_user <- users$votes_useful/users$review_count			# mean useful votes per review by user
users$avg_funny_user <- users$votes_funny/users$review_count            # mean funny votes per review by user
users$avg_cool_user <- users$votes_cool/users$review_count              # mean cool votes per review by user
users$votes_over_stars_user<-users$votes_useful/users$average_stars     # useful votes over average stars by user

# user names that end in e or a (proxy for female)
name_set<-unique(tolower(users$name))
users$ea_endname<-ifelse(tolower(substr(users$name,nchar(users$name),nchar(users$name)))=='e',1,ifelse(tolower(substr(users$name,nchar(users$name),nchar(users$name)))=='a',1,0))
aggregate(x=users$votes_useful,by=list(users$ea_endname),FUN=mean) # looks like users with names ending in e and a have lower average votes_useful

# cluster users
uc_var<-scale(data.matrix(users[,c('votes_useful','votes_cool','votes_funny','average_stars','review_count','first_review_user')]))
k_wss<-NULL
for (i in 2:75){
k_cluster<-kmeans(uc_var,centers=i)
k_wss[i]<-k_cluster$tot.withinss
}
plot(k_wss)
k_cluster<-kmeans(uc_var,centers=10)
users<-cbind(users,k_cluster$cluster)
colnames(users)<-c("user_id", "name", "review_count", "average_stars", "votes_cool", "votes_funny", "votes_useful", "first_review_user", "avg_useful_user", 
		"avg_funny_user", "avg_cool_user", "votes_over_stars_user", "ea_endname", "user.cluster")

# join users onto review data
traindat<-merge(x=traindat,y=users,by='user_id',in.x=TRUE)


#-----------------------------------------------------------------------------------------------------------------------------------------
# business data (I already did a few things with this in business_features.r)

# import business data
business<-read.csv('/home/chadm/yelp/business1.csv',stringsAsFactors=FALSE)
# drop out a few columns we won't be likely to want
business<-business[,c('business_id','open','city','latitude','longitude','review_count','stars','bcluster')]
colnames(business)<-c('business_id','business.open','city','latitude','longitude','business.review_count','business.stars','business.cluster')

# join to reviews
traindat<-merge(x=traindat,y=business,by='business_id',in.x=TRUE)

# first review for this business
first_review_business <- aggregate(reviews$date,by=list(reviews$business_id),FUN=min)
colnames(first_review_business) <- c('business_id','b.first_date')
traindat<-merge(x=traindat,y=first_review_business,by='business_id',in.x=TRUE)
traindat$b.first_review <- ifelse(traindat$date==traindat$b.first_date,1,0)
traindat$b.age_to_first <- as.numeric(traindat$date-traindat$b.first_date)


#-----------------------------------------------------------------------------------------------------------------------------------------
# analyze review text

library(tm)
library(reshape2)

text_reviews<-data.frame(traindat$text)
text_reviews[,1]<-as.character(text_reviews[,1])
slice <- seq(from=1,to=dim(text_reviews)[1]) %% 40

# loop over chunks of the data for text processing
for (i in 0:39){
	slice_reviews = data.frame(text_reviews[slice==i,])

	# make and clean a corpus for the review text
	corpus<-Corpus(DataframeSource(slice_reviews))
	corpus<-tm_map(corpus,tolower)
	corpus<-tm_map(corpus,stripWhitespace)
	corpus<-tm_map(corpus,removePunctuation)
	corpus <- tm_map(corpus, removeWords, stopwords('english'))

	# turn cleaned up corpus into a document-term matrix
	dtm<-DocumentTermMatrix(corpus)
	dtm<-removeSparseTerms(dtm,.99) 	# remove infrequent terms
	dtm_i<-data.frame(data.matrix(dtm))	# convert to data frame
	# dtm_i<-data.frame(data.matrix(weightTfIdf(weightTfIdf(dtm)))) # optional: tf-idf weighting
	
	# remove terms that aren't in the master and chunk dtms, then append to master
	if(i==0) { master_dtm <- dtm_i 	}
	if(i!=0){
		dtm_i<-dtm_i[,intersect(colnames(master_dtm),colnames(dtm_i))]
		master_dtm <- master_dtm[,intersect(colnames(master_dtm),colnames(dtm_i))]
		master_dtm <- rbind(master_dtm,dtm_i)
	}
}


# try lasso model on our extracted review words
y<-data.matrix(traindat$log_useful)
x<-data.matrix(master_dtm)
lasso_words <- glmnet(y=y,x=x,alpha=1,family='gaussian')

# try sparse PCA on the words
library(elasticnet)
cov<-cov(x)
par<-rep(.003,10)
spca<-spca(cov,10,type='Gram',sparse='penalty',para=par)
sum(spca$pev)
loadings<-data.matrix(spca$loadings)
text_pc_scores<-x%*%loadings

# note: I unfortunately ended up running out of time to use any of these text mining features in my algorithm


#-----------------------------------------------------------------------------------------------------------------------------------------
# make more derived features


# split off CV data
traindat$train<-ifelse(runif(dim(traindat)[1])>.8,1,0)

# compare review stars to average for business
traindat$b.stars_ratio<-traindat$stars/traindat$business.stars

#compare review stars to average for user
traindat$b.stars_ratio<-traindat$stars/traindat$average_stars


# split businesses up by geographical region (using decision trees and useful votes)
library(tree)
geo.tree<-tree(log_useful~latitude+longitude
 ,data=traindat[traindat$train==1,]
 ,control=tree.control(nobs=dim(traindat[traindat$train==1,])[1],mincut=10,,mindev=.0001))
 
 # tune tree
cvdat<-traindat[traindat$train==0,]
tune_k<-NULL
for (k in 1:40){
	k_i = exp(k/2)
	prune.k<-prune.tree(geo.tree,k=k_i) 
	prune_k.pred<-data.matrix(predict(prune.k,newdata=cvdat))
	prune_k.rmse <- sqrt(sum((prune_k.pred-cvdat$log_useful)^2)/dim(cvdat)[1])
	k_info = cbind(k_i,prune_k.rmse)
	tune_k<-rbind(tune_k,k_info)
}

 geo.tree<-prune.tree(geo.tree,k=7.389056)
 traindat$geo.pred<-predict(geo.tree,newdata=traindat)
 traindat$geo.region<-as.character(traindat$geo.pred)

 ggplot(data=traindat,aes(x=longitude,y=latitude,color=geo.pred))+geom_point()


# average useful votes per business
business.avg_useful<-aggregate(x=traindat$votes_useful,by=list(traindat$business_id),FUN=mean)
colnames(business.avg_useful)<-c('business_id','business.avg_useful')
traindat<-merge(x=traindat,y=business.avg_useful,by='business_id',in.x=TRUE)


#-----------------------------------------------------------------------------------------------------------------------------------------
# Algorithm training - build several machine learning algorithms to predict useful votes

# cv data
cvdat<-traindat[traindat$train==0,]


#-------- linear regression model ------#

# train regression model
simple.lm<-lm(log_useful~stars+avg_useful_user+review_count+review_length+b.first_review+b.age_to_first+business.review_count+date+first_review_user+geo.pred+business.avg_useful+factor(business.cluster)+factor(user.cluster)
	,data=traindat[traindat$train==1,])

summary(simple.lm)                                                          # examine coefficients
cvdat$lm.predict <- predict(simple.lm,newdata=cvdat)                        # predict useful votes for validation data
lm.rmse<- sqrt(sum((cvdat$lm.predict-cvdat$log_useful)^2)/dim(cvdat)[1])    # calculate CV RMSE for regression


#-------- lasso regression model -------#

library(glmnet)
library(dummies)
# make dummy variables for business and user clusters
b.cluster.dummy <- dummy(traindat$business.cluster)
u.cluster.dummy <- dummy(traindat$user.cluster)

# set up training and validation X and y matricies
xmat <- data.matrix(cbind(traindat[traindat$train==1,c('stars','avg_useful_user','review_count','review_length','b.first_review','b.age_to_first','business.review_count','date','first_review_user','geo.pred','business.avg_useful')]
	,b.cluster.dummy[traindat$train==1,],u.cluster.dummy[traindat$train==1,]))
ymat <- data.matrix(traindat[traindat$train==1,'log_useful'])

cv.xmat <- data.matrix(cbind(traindat[traindat$train==0,c('stars','avg_useful_user','review_count','review_length','b.first_review','b.age_to_first','business.review_count','date','first_review_user','geo.pred','business.avg_useful')]
	,b.cluster.dummy[traindat$train==0,],u.cluster.dummy[traindat$train==0,]))

# train lasso model
lasso <- glmnet(x=xmat,y=ymat,alpha=1,family='gaussian')                        # lasso path fitting wiht glmnet
cv.lasso <- cv.glmnet(x=xmat,y=ymat,alpha=1,family='gaussian',nfold=5)          # 5-fold cross-validation to find lambda that minimizes error
cvdat$lasso.predict<-predict(lasso,newx=cv.xmat,s=cv.lasso$lambda.min)          # predict useful votes for validation data
lasso.rmse<- sqrt(sum((cvdat$lasso.predict-cvdat$log_useful)^2)/dim(cvdat)[1])  # calculate CV RMSE for lasso

# pull out lasso coefficents
predict(lasso,s=cv.lasso$lambda.min,type='coefficients')

# LASSO model selected the following features:

# stars+avg_useful_user+review_count+review_length+b.first_review+b.age_to_first+business.review_count+date+first_review_user+geo.pred+business.avg_useful
# +business.cluster2+business.cluster3+business.cluster4+business.cluster5+business.cluster14+business.cluster15+business.cluster18+user.cluster1
# +user.cluster2+user.cluster3+user.cluster4+user.cluster5+user.cluster7+user.cluster8+user.cluster10


#-------  random forest ----------#

library(randomForest)

# fit random forest
rf.model <- randomForest(log_useful~stars+avg_useful_user+review_count+review_length+b.first_review+b.age_to_first
		+business.review_count+date+first_review_user+geo.pred+business.avg_useful
			+user.cluster+business.cluster+latitude+longitude
		,data=traindat[traindat$train==1,]
		,importance=TRUE	# collect variable importance
		,mtry=5				# m: number of split vars
		,ntree=150			# number of trees to learn
		,nodesize=5)		# mimimum cases in a leaf

cvdat$rf.predict <- predict(rf.model,newdata=cvdat)                         # predict useful votes for validation data
rf.rmse<- sqrt(sum((cvdat$rf.predict-cvdat$log_useful)^2)/dim(cvdat)[1])    # calculate CV RMSE for random forest


#-------- neural network ---------#
library(nnet)

# set up training data
traindat<-cbind(traindat,b.cluster.dummy,u.cluster.dummy)
cvdat<-traindat[traindat$train==0,]


# fit neural network (using features selected by lasso model)
nnet.model <- nnet(log_useful~stars+avg_useful_user+review_count+review_length+b.first_review+b.age_to_first+business.review_count+date+first_review_user+geo.pred+business.avg_useful+business.cluster2+business.cluster3+business.cluster4+business.cluster5+business.cluster14+business.cluster15+business.cluster18
		,data=traindat[traindat$train==1,]
        ,size=40            # 40 hidden nodes
        ,decay=.8           # penalty on sum of squared weights = .8
        ,linout=TRUE        # output activiation function = linear
        ,maxit=4000)        # train for up to 4000 iterations (will stop early on convergence)

cvdat$nnet.predict <- predict(nnet.model,newdata=cvdat)                     # predict useful votes for validation data
nnet.rmse<-sqrt(sum((cvdat$nnet.predict-cvdat$log_useful)^2)/dim(cvdat)[1]) # calculate CV RMSE for neural network

# note: random forest had the best CV performance, so I will use it for the final predictions of the testing data
# note: with more time, I would have wanted to further tune the random forest and neural network and I would have also tried SVR and boosting


#-----------------------------------------------------------------------------------------------------------------------------------------
# bring in testing data and make final predictions

# read in testing file
testdat <- read.csv('/home/chadm/yelp/review_testing.csv',stringsAsFactors=FALSE)

# add initial features
testdat$date <- as.Date(testdat$date)		# date format
testdat$review_length<-nchar(testdat$text) 	# length of text

# merge on user and business
testdat <- merge(x=testdat,y=users,by='user_id',in.x=TRUE)
testdat <- merge(x=testdat,y=business,by='business_id',in.x=TRUE)
testdat<-merge(x=testdat,y=business.avg_useful,by='business_id',in.x=TRUE)
testdat$geo.pred<-predict(geo.tree,newdata=testdat)
testdat$b.first_review <- ifelse(testdat$date==testdat$b.first_date,1,0)
testdat$b.age_to_first <- as.numeric(testdat$date-testdat$b.first_date)

# predict log_useful
testdat$log_useful <- predict(rf.model,newdata=testdat)
testdat$predicted_votes_useful <- exp(testdat$log_useful)-1

# make submission
write.csv(testdat[,c('review_id','predicted_votes_useful')],'/home/chadm/yelp/chadm_review_predict.csv',row.names=FALSE)

# save workspace
save.image('/home/chadm/yelp/review_modeling_3.7.2014.RData')
