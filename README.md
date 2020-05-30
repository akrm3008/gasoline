# Introduction
Few days before landfall of Hurricane Irma in Florida and post its landafall (6th-15th Sept 2017), the following types of tweets were observed in FLorida, that reported people's inability to find gasoline:
* "The shelters are full, there is no gas. Tornados could happen, and storm surge is predicted. So what are people supposed to do? Irma ”
* "Insane..95 percent of Florida trying to leave at one time. Roads r slammed. No gas. No hotels available.42Scared to see my neighborhood after irma”
* "Gas  stations  out  of  gas,  water  shelves  empty,  stores  and  airports  closed.  Stocked  up  on  food  and  wine, waiting on irma”

The natural question that arises in such a scenario is that can Twitter posts be used to sense current47shortage and forecast future shortages? There are two challenges related to this question:

##  Challenge C1 - How to identify tweets about shortage 
Social media data, especially from twitter,  is  difficult  to  process  and  classify  as  it  is  unstructured,  noisy  and  contains  a plethora  of information (large number of tweets). Also, a single tweet contains a maximum of 140 characters, is informal and contains abbreviations and spelling mistakes. Interpreting the semantics of such a short message and classifying it is a hard problem.

##  Challenge C2 - Forecasting the spatio-temporal shortage from tweets
The spatio-temporal distribution of tweets about shortage are not equivalent to the spatio-temporal shortage distribution. Spatial and temporal  lag  between  the  origin  of  the  shortage  and  the  tweet  about  shortage  is  an uncertain quantity. This makes forecasting shortages using tweets a challenging problem.

To address challenge C1, I developed a tweet classifier that that uses unigrams and latent topics (identified by topic modeling techniques) as features to identify tweets about gasoline shortage. Analysis of the identified tweets shows us that the number of tweets about shortage (in a day, in a city) predict the number of stations out of gasoline. A detailed analysis of the spatio-temporal dynamics of the arrival of shortage tweets shows us that the arrival of tweets in a city follows a Poisson distribution. Using these two insights, we tackle challenge C2, as follows. I developed a Regression model with a unique Hybrid Loss Function (HLF, that combines the properties of Poisson Regression and time series based ARIMA models) to predict the number of future tweets in a city. A separate Poisson Regression Model is used to predict the amount of shortage from the predicted number of tweets

# Data Description

The data set had roughly one million tweets from Florida during the period 6-15 September 2017. The data covered a data frame in R with 1048575 rows and 41 columns that include TWEET ID, TWEET TEXT, USER ID, DATE, HASHTAG, LATITUDE, and LONGITUDE.

Apart from the Twitter data, we also collected ground truth data about gas shortage from Gasbuddy Application (https://business.gasbuddy.com/hurricane-irma-live-updates-fuel-availability-station-outages/7) and details and predictions of the Hurricane path from the National Hurricane Center Website. We collected this data for 8 cities namely Gainesville, Jacksonville, Miami, Orlando, Tallahassee, Tampa, Naples and West Palm Beach, for the period of 6-15 September 2017 (dates when shortage  was  observed).  For  each  date  and  city  we  determined  if  the  city  was  predicted  to  be  on  the hurricane path, if it was inside the hurricane 3-day or 5-day cone, the number of days to arrival of the hurricane, if there were any hurricane/thunderstorm warning and watches from National Hurricane Center in the city on that date, the maximum sustained wind speed, and population of the city, the number of gas stations in the city, proportion of gasoline stations without gasoline. The idea behind collection of  these  attributes  is  that  these  variables  also  drive  panic-buying  behavior  causing  shortage  and  also influence the tweeting behavior of the people. Therefore they are potential predictors of gasoline shortage and tweeting behavior of people in the models.


# Methodology and Code

## Stage 1: Filtering to generate gasoline-related tweets (tweet_filtering.R)

In  Stage  1, I filtered  out  “gasoline-related”  tweets  from  the  compendium  of  tweets  generated  in  the affected area. I do this by keyword search in both the content and hashtags of each tweet. In the case of gasoline any word which has the letters “gas” as part of the word is a possible keyword. I use regular expressions to identify these key words. Even after this filtering, I have many tweets that are very noisy and need to be cleaned so as to facilitate further processing. I achieved this cleaning by removing user names, links, punctuations, tabs, general whitespaces, stopwords, and numbers. I further changed words to stem words and to lower case.

## Stage 2: Classification to identify gasoline-shortage tweets (tweet_classification.R)

To classify  the  tweets  as  “gasoline-shortage”, i.e tweets that implicitly or explicitly give information about gasoline shortage,   they  are  manually  annotated.  Next,  a  Support  Vector 6Machine Classifier is trained that classifies tweets using two kinds of features  unigrams and latent topics identified using topic modeling (LDA, CTM, LDA using Gibbs sampling). Through the 


## Stage 3: Prediction of the number of future gasoline shortage tweets using a hybrid loss function (HLF) 

### tweets_spatio_temporal_visualisation.R

In this script we did a thorough spatial and temporal visulisation of tweets about gasoline shortage in Florida. We see how the frequency of tweets evolve as the hurricane nears Florida, as advsories are made every six hours by national hurricane center, as ground situation of gasoline shortage changes, as evacuations are ordered, and after landfall.

### tweets_visulalisation_citywise.R

In this script, for each city namely, Gainesville, Jacksonville, Miami, Orlando, Tallahassee, Tampa, Naples and West Palm Beach, we visulised the spatio-temporal arrival of each city.

### temporal_dist_gas_tweets_citywise.R

In this script, we studied the temporal distribution of shortage tweets in each city. First, for each city, I test if the arrival of tweets fit a Poisson Distributin using Chi-square testing. In the second part we do a time series analysis of tweet arrival for each city.

### forecasting_tweets.R

In temporal_dist_gas_tweets_citywise.R, we found that the arrival of tweets in the city hourly follwoed a Poisson Distribution, and ARIMA models also fit the temporal patten of hourly gasoline tweets in each city. So in this tweet, we try to forecast gasoline tweets using three methods :
* ARIMA 
* Poisson Regression with independdent variables as percentage of gas stations without gas, if the city was inside the hurricane 3-day or 5-day cone, the number of days to arrival of the hurricane, if there were any hurricane/thunderstorm warning and watches from National Hurricane Center in the city on that date, the maximum sustained wind speed, and population of the city.
* A Hybrid Loss Function (HLF) that combines ARIMA results with liklihood function of Poisson Regression and solve it using gradient descent


## Stage 4: Prediction of the gasoline shortage using the forecasted tweets (predicting_shortage_from_tweets.R)

In this script, we use the prediction in stage 3 to predict shortage on next day using Poisson Regression


# For further details refer to the published work in file Published.pdf.




