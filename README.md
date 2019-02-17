# Introduction
Few days before landfall of Hurricane Irma in Florida and post its landafall (6th-15th Sept 2017), the following types of tweets were observed in FLorida, that reported people's inability to find gasoline:
* "The shelters are full, there is no gas. Tornados could happen, and storm surge is predicted. So what are people supposed to do? Irma ”
* "Insane..95 percent of Florida trying to leave at one time. Roads r slammed. No gas. No hotels available.42Scared to see my neighborhood after irma”
* "Gas  stations  out  of  gas,  water  shelves  empty,  stores  and  airports  closed.  Stocked  up  on  food  and  wine, waiting on irma”

The natural question that arises in such a scenario is that can Twitter posts be used to sense current47shortage and forecast future shortages? There are two challenges related to this question:

##  Challenge C1 - How to identify tweets about shortage :
Social media data, especially from twitter,  is  difficult  to  process  and  classify  as  it  is  unstructured,  noisy  and  contains  a plethora  of information (large number of tweets). Also, a single tweet contains a maximum of 140 characters, is informal and contains abbreviations and spelling mistakes. Interpreting the semantics of such a short message and classifying it is a hard problem.

##  Challenge C2 - Forecasting the spatio-temporal shortage from tweets:
The spatio-temporal distribution of tweets about shortage are not equivalent to the spatio-temporal shortage distribution. Spatial and temporal  lag  between  the  origin  of  the  shortage  and  the  tweet  about  shortage  is  an uncertain quantity. This makes forecasting shortages using tweets a challenging problem.

To address challenge C1, I developed a tweet classifier that that uses unigrams and latent topics (identified by topic modeling techniques) as features to identify tweets about gasoline shortage. Analysis of the identified tweets shows us that the number of tweets about shortage (in a day, in a city) predict the number of stations out of gasoline. A detailed analysis of the spatio-temporal dynamics of the arrival of shortage tweets shows us that the arrival of tweets in a city follows a Poisson distribution. Using these two insights, we tackle challenge C2, as follows. I developed a Regression model with a unique Hybrid Loss Function (HLF, that combines the properties of Poisson Regression and time series based ARIMA models) to predict the number of future tweets in a city. A separate Poisson Regression Model is used to predict the amount of shortage from the predicted number of tweets

# Data Description

My data set had roughly one million tweets from Florida during the period 6-15 September 2017. The data covered a data frame in R with 1048575 rows and 41 columns that include TWEET ID, TWEET TEXT, USER ID, DATE, HASHTAG, LATITUDE, and LONGITUDE.

Apart from the Twitter data, we also collected ground truth data about gas shortage from Gasbuddy Application (https://business.gasbuddy.com/hurricane-irma-live-updates-fuel-availability-station-outages/7) and details and predictions of the Hurricane path from the National Hurricane Center Website. We collected this data for 8 cities namely Gainesville, Jacksonville, Miami, Orlando, Tallahassee, Tampa, Naples and West Palm Beach, for the period of 6-15 September 2017 (dates when shortage  was  observed).  For  each  date  and  city  we  determined  if  the  city  was  predicted  to  be  on  the hurricane path, if it was inside the hurricane 3-day or 5-day cone, the number of days to arrival of the hurricane, if there were any hurricane/thunderstorm warning and watches from National Hurricane Center in the city on that date, the maximum sustained wind speed, and population of the city, the number of gas stations in the city, proportion of gasoline stations without gasoline. The idea behind collection of  these  attributes  is  that  these  variables  also  drive  panic-buying  behavior  causing  shortage  and  also influence the tweeting behavior of the people. Therefore they are potential predictors of gasoline shortage and tweeting behavior of people in the models.
