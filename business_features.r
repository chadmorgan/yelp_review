

#-----------------------------------------------------------------------------------------------------------------------------------------
#   Name: business_features.r
#   Author: Chad Morgan
#   Purpose: create features from the business data, mostly around the business category
#
#-----------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls()) # clear R workspace
memory.limit(size=8192)
.libPaths('P://R//win-library//2.15')
library(ggplot2)

business<-read.csv('P:/Innovation/predictive modeling training/yelp review exercise/business.csv',stringsAsFactors=FALSE)

# turn categories field into an incidence matrix
library(tm)
cats<-data.frame(business$categories)
cats[,1]<-gsub(","," ",cats[,1])

cat_corpus<-Corpus(DataframeSource(cats))
cat_corpus<-tm_map(cat_corpus,tolower)
cat_corpus<-tm_map(cat_corpus,stripWhitespace)

dtm<-DocumentTermMatrix(cat_corpus)
findFreqTerms(dtm,lowfreq=200)

freq_cats<-data.matrix(removeSparseTerms(dtm,.993))

# try sparse PCA to reduce redundant categories to a few dimensions
library(elasticnet)
cov<-cov(freq_cats)
par<-rep(.003,20)
spca<-spca(cov,20,type='Gram',sparse='penalty',para=par)
sum(spca$pev)
loadings<-data.frame(spca$loadings)
names_list<-NULL
for (i in 1:20) {
	# reflect PCs with negative loading as the largest absolute value to ease interpretation
	if(abs(min(loadings[,i]))>abs(max(loadings[,i]))) {loadings[,i]<-loadings[,i]*-1}
	names_list[i]<-rownames(loadings[loadings[,i]==max(loadings[,i]),]) # get top cateogry for name
}
colnames(loadings)<-names_list
# score businesses on our 20 super categories
cat_scores<-scale(freq_cats)%*%data.matrix(loadings)
summary(cat_scores)
# cluster using categories to get discrete group option
k_wss<-NULL
for (i in 2:75){
k_cluster<-kmeans(freq_cats,centers=i)
k_wss[i]<-k_cluster$tot.withinss
}
plot(k_wss)
business_cluster<-kmeans(freq_cats,centers=31)
business$bcluster <- factor(business_cluster$cluster)
write.csv(business,'P:/Innovation/predictive modeling training/yelp review exercise/business1.csv',row.names=FALSE)

# Mapping the businesses
summary(business)
ggplot(data=business,aes(x=review_count))+geom_density()
ggplot(data=business,aes(x=stars))+geom_density()
ggplot(data=business,aes(x=stars,y=review_count))+geom_jitter()
library(maps)
states<-map_data("state")
arizona<-subset(states, region %in% c('arizona'))
map<-ggplot()
map<-map+geom_polygon( data=arizona, aes(x=long, y=lat, group = group),colour="grey10", fill="grey50" )
map<-map+geom_point( data=business, aes(x=longitude,y=latitude, color=city))
library(ggmap)
gmap<-get_googlemap('phoenix')
ggmap(gmap)+geom_point(data=business, aes(x=longitude,y=latitude, color=bcluster))