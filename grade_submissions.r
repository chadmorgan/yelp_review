
#------------------------------------------------------------------------------------------------------------------------------------------------
# Author: Chad Morgan
# Project: Modeling class - Yelp homework
# Purpose: Grade homework submissions and rank students on model quality
#------------------------------------------------------------------------------------------------------------------------------------------------

# import testing data with true values of useful votes
testdat <-read.csv('/home/chadm/yelp/review_testing_wAnswers.csv',stringsAsFactors=FALSE)


# folders for submissions

path1 =
path2 =
path3 =

# function to compute RMSLE for a given submission
grade_hw <- function(input_file,folder){
	full_path = paste(folder,'/',input_file,sep='')					# put path together
	submitdat <- read.csv(full_path,stringsAsFactors=FALSE)			# import submission data
	
	# merge submission and answers
	comparedat <- merge(x=testdat[,c('review_id','votes_useful')],y=submitdat,by='review_id',in.x=TRUE)
	comparedat$predicted_votes_useful <- ifelse(comparedat$predicted_votes_useful<0,0,comparedat$predicted_votes_useful) # if prediction is missing, fill in with a zero
	comparedat$predicted_votes_useful <- ifelse(is.na(comparedat$predicted_votes_useful),0,comparedat$predicted_votes_useful) # if prediction is missing, fill in with a zero
	
	
	# compute RMSLE
	log_predict = log(1+comparedat$predicted_votes_useful)
	log_actual = log(1+comparedat$votes_useful)
	n = dim(comparedat)[1]
	RMSLE = sqrt( sum( (log_predict - log_actual)^2 ) / n )
	
	# output grade
	grade_submission <- data.frame(cbind(input_file,RMSLE))
	return(grade_submission)
}

# test the function with my sample submission
sample_score <- grade_hw('chadm_review_predict.csv',path1)
sample_score <- grade_hw('jeffse_review_predict.csv',path2)
sample_score <- grade_hw('phila_review_predict.csv',path2)


# stack all of the submission and sort ascending on RMSLE
grades <- t(cbind(chad_RMSE,phil_RMSE,jeff_RMSE,mattpy_rmse,mattm_rmse,brianr_rmse))
names <- unlist(rownames(grades))
grades <- data.frame(cbind(grades,names))
colnames(grades)<-c('rmsle','names')
grades$rmsle <- as.numeric(as.character(grades$rmsle))
