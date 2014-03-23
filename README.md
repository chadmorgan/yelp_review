yelp_review
===========

Machine learning practice with the Kaggle Yelp contest


As part of the internal machine learning class that I taught, we used the Kaggle Yelp contest as a practice problem.

1. I pulled down the Yelp contest training data and converted it from
JSON to CSV (separate python script that I didn’t author)
2. prepare_yelp_data.r - split off a training and testing dataset for
the class to use and do some minor data cleaning
3. business_features.r - create some business category based features
for my own algorithm
4. build_algorithm.r - my code for building an algorithm to predict the
useful votes that a Yelp review will get
5. grade_submissions.r - script to grade all of the student submissions
