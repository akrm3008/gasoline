# In this script, we analyse the temporal arrival of tweets about gasoline shortage at the city level 
# In the first part, for each city, I test if the arrival of tweets fit a Poisson Distributin using a 
# Chi-square test 
#  In the second part we do a time series analysis of tweet arrival for each city
library(gridExtra)
library(dplyr)
library(ggplot2)
library(standardize)


# getting bounding box for each city

bb_tampa <- attr(map_tampa,"bb")  # Tampa and surrounding suburbs and cities
bb_miami <- attr(map_miami,"bb")  # miami and surrounding sunburbs and cities
bb_florida <- attr(map_florida,"bb")
bb_florida <- attr(map_florida,"bb")
bb_tal <- attr(map_tal,"bb")
bb_jack <- attr(map_jack,"bb")
bb_orlando <- attr(map_orlando,"bb")
bb_gaine <- attr(map_gaine,"bb")
bb_palm <- attr(map_palm_beach,"bb")
bb_naples <- attr(map_naples,"bb")

# In dfgas_short changing getting exact Latitudes and longitudes for "poi's"
dfgas_short <- dfgas_short  %>% mutate(LATITUDE = ifelse(PLACE_TYPE== "poi" & is.na(LATITUDE),PLACE_BBOX_LB_LAT, LATITUDE),
                          LONGITUDE= ifelse(PLACE_TYPE== "poi" & is.na(LONGITUDE),PLACE_BBOX_LB_LON,LONGITUDE))


# Tweets For Tampa
df_tampa_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_tampa$ll.lat,
                                 PLACE_BBOX_LT_LAT < bb_tampa$ur.lat , PLACE_BBOX_LB_LON > bb_tampa$ll.lon,
                                 PLACE_BBOX_RT_LON < bb_tampa$ur.lon)
df_tampa_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_tampa$ll.lat,
                                      LATITUDE < bb_tampa$ur.lat , LONGITUDE > bb_tampa$ll.lon,
                                      LONGITUDE < bb_tampa$ur.lon) 

# Combining tweets in Tampa with and without exact location 
df_tampa <- rbind(df_tampa_bb, df_tampa_exactloc)

# Getting tampa tweets date-wise
df_tampa <- df_tampa %>% arrange(DATE,TIME)
df_tampa_6 <- df_tampa %>% filter(DATE=="9/6/2017")
df_tampa_7 <- df_tampa %>% filter(DATE=="9/7/2017")
df_tampa_8 <- df_tampa %>% filter(DATE=="9/8/2017")
df_tampa_9 <- df_tampa %>% filter(DATE=="9/9/2017")
df_tampa_6 <- df_tampa_6 %>%  mutate(hour= hour(hms(as.character(factor(df_tampa_6$TIME- df_tampa_6$TIME[1])))))
df_tampa_7 <- df_tampa_7 %>%  mutate(hour= hour(hms(as.character(factor(df_tampa_7$TIME- df_tampa_7$TIME[1])))))                                               
df_tampa_8 <- df_tampa_8 %>%  mutate(hour= hour(hms(as.character(factor(df_tampa_8$TIME- df_tampa_8$TIME[1])))))
df_tampa_9 <- df_tampa_9 %>%  mutate(hour= hour(hms(as.character(factor(df_tampa_9$TIME- df_tampa_9$TIME[1])))))


df_tampa_by_hour <- rbind(df_tampa_6,df_tampa_7,df_tampa_8,df_tampa_9)





# Getting number of tweets in time intervals now (1 day interval)
num_tweets_tampa_hourly <- df_tampa_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

tampa_hourly_tweet_dist <- num_tweets_tampa_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))

qplot(num_tweets_tampa_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Tampa")

mean_tampa <- sum(tampa_hourly_tweet_dist$num_of_tweets*tampa_hourly_tweet_dist$freq)

## Doing Chi square goodness of fit test to see if the "tampa_hourly_tweet_dist" follows a Poisson Distribution
probs = dpois(tampa_hourly_tweet_dist$num_of_tweets, lambda=mean_tampa) # generating probabilities for a Poisson Distribtion with lambda = mean_tampa
probs
comp = 1-sum(probs)
chisq.test(x=c(tampa_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(tampa_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)


# Same anlysis for Miami
df_miami_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_miami$ll.lat,
                                 PLACE_BBOX_LT_LAT < bb_miami$ur.lat , PLACE_BBOX_LB_LON > bb_miami$ll.lon,
                                 PLACE_BBOX_RT_LON < bb_miami$ur.lon)
df_miami_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_miami$ll.lat,
                                      LATITUDE < bb_miami$ur.lat , LONGITUDE > bb_miami$ll.lon,
                                      LONGITUDE < bb_miami$ur.lon) 

df_miami <- rbind(df_miami_bb, df_miami_exactloc)
df_miami <- df_miami %>% arrange(DATE,TIME)

df_miami_6 <- df_miami %>% filter(DATE=="9/6/2017")
df_miami_7 <- df_miami %>% filter(DATE=="9/7/2017")
df_miami_8 <- df_miami %>% filter(DATE=="9/8/2017")
df_miami_9 <- df_miami %>% filter(DATE=="9/9/2017")
df_miami_6 <- df_miami_6 %>%  mutate(hour= hour(hms(as.character(factor(df_miami_6$TIME- df_miami_6$TIME[1])))))
df_miami_7 <- df_miami_7 %>%  mutate(hour= hour(hms(as.character(factor(df_miami_7$TIME- df_miami_7$TIME[1])))))                                               
df_miami_8 <- df_miami_8 %>%  mutate(hour= hour(hms(as.character(factor(df_miami_8$TIME- df_miami_8$TIME[1])))))
df_miami_9 <- df_miami_9 %>%  mutate(hour= hour(hms(as.character(factor(df_miami_9$TIME- df_miami_9$TIME[1])))))



df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_miami_by_hour <- rbind(df_miami_6,df_miami_7,df_miami_8,df_miami_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_miami_hourly <- df_miami_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_miami_hourly <- right_join(num_tweets_miami_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 


miami_hourly_tweet_dist <- num_tweets_miami_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))

qplot(num_tweets_miami_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Miami")

mean_miami <- sum(miami_hourly_tweet_dist$num_of_tweets*miami_hourly_tweet_dist$freq)
probs = dpois(miami_hourly_tweet_dist$num_of_tweets, lambda=mean_miami)
probs
comp = 1-sum(probs)
chisq.test(x=c(miami_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(miami_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)




# For Orlando 

df_orlando_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_orlando$ll.lat,
                                   PLACE_BBOX_LT_LAT < bb_orlando$ur.lat , PLACE_BBOX_LB_LON > bb_orlando$ll.lon,
                                   PLACE_BBOX_RT_LON < bb_orlando$ur.lon)
df_orlando_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_orlando$ll.lat,
                                        LATITUDE < bb_orlando$ur.lat , LONGITUDE > bb_orlando$ll.lon,
                                        LONGITUDE < bb_orlando$ur.lon) 

df_orlando <- rbind(df_orlando_bb, df_orlando_exactloc)
df_orlando <- df_orlando %>% arrange(DATE,TIME)

df_orlando_6 <- df_orlando %>% filter(DATE=="9/6/2017")
df_orlando_7 <- df_orlando %>% filter(DATE=="9/7/2017")
df_orlando_8 <- df_orlando %>% filter(DATE=="9/8/2017")
df_orlando_9 <- df_orlando %>% filter(DATE=="9/9/2017")
df_orlando_6 <- df_orlando_6 %>%  mutate(hour= hour(hms(as.character(factor(df_orlando_6$TIME- df_orlando_6$TIME[1])))))
df_orlando_7 <- df_orlando_7 %>%  mutate(hour= hour(hms(as.character(factor(df_orlando_7$TIME- df_orlando_7$TIME[1])))))                                               
df_orlando_8 <- df_orlando_8 %>%  mutate(hour= hour(hms(as.character(factor(df_orlando_8$TIME- df_orlando_8$TIME[1])))))
df_orlando_9 <- df_orlando_9 %>%  mutate(hour= hour(hms(as.character(factor(df_orlando_9$TIME- df_orlando_9$TIME[1])))))



df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_orlando_by_hour <- rbind(df_orlando_6,df_orlando_7,df_orlando_8,df_orlando_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_orlando_hourly <- df_orlando_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_orlando_hourly <- right_join(num_tweets_orlando_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 


orlando_hourly_tweet_dist <- num_tweets_orlando_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))
qplot(num_tweets_orlando_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Orlando")

mean_orlando <- sum(orlando_hourly_tweet_dist$num_of_tweets*orlando_hourly_tweet_dist$freq)
probs = dpois(orlando_hourly_tweet_dist$num_of_tweets, lambda=mean_orlando)
probs
comp = 1-sum(probs)
chisq.test(x=c(orlando_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(orlando_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)


# For Tal

df_tal_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_tal$ll.lat,
                               PLACE_BBOX_LT_LAT < bb_tal$ur.lat , PLACE_BBOX_LB_LON > bb_tal$ll.lon,
                               PLACE_BBOX_RT_LON < bb_tal$ur.lon)
df_tal_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_tal$ll.lat,
                                    LATITUDE < bb_tal$ur.lat , LONGITUDE > bb_tal$ll.lon,
                                    LONGITUDE < bb_tal$ur.lon) 

df_tal <- rbind(df_tal_bb, df_tal_exactloc)
df_tal <- df_tal %>% arrange(DATE,TIME)

df_tal_6 <- df_tal %>% filter(DATE=="9/6/2017")
df_tal_7 <- df_tal %>% filter(DATE=="9/7/2017")
df_tal_8 <- df_tal %>% filter(DATE=="9/8/2017")
df_tal_9 <- df_tal %>% filter(DATE=="9/9/2017")
df_tal_6 <- df_tal_6 %>%  mutate(hour= hour(hms(as.character(factor(df_tal_6$TIME- df_tal_6$TIME[1])))))
df_tal_7 <- df_tal_7 %>%  mutate(hour= hour(hms(as.character(factor(df_tal_7$TIME- df_tal_7$TIME[1])))))                                               
df_tal_8 <- df_tal_8 %>%  mutate(hour= hour(hms(as.character(factor(df_tal_8$TIME- df_tal_8$TIME[1])))))
df_tal_9 <- df_tal_9 %>%  mutate(hour= hour(hms(as.character(factor(df_tal_9$TIME- df_tal_9$TIME[1])))))



df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_tal_by_hour <- rbind(df_tal_6,df_tal_7,df_tal_8,df_tal_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_tal_hourly <- df_tal_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_tal_hourly <- right_join(num_tweets_tal_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 


tal_hourly_tweet_dist <- num_tweets_tal_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))

qplot(num_tweets_tal_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Tallahassee")

mean_tal <- sum(tal_hourly_tweet_dist$num_of_tweets*tal_hourly_tweet_dist$freq)

probs = dpois(tal_hourly_tweet_dist$num_of_tweets, lambda=mean_tal)
probs
comp = 1-sum(probs)
chisq.test(x=c(tal_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(tal_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)



# For Jacksonville

df_jack_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_jack$ll.lat,
                                PLACE_BBOX_LT_LAT < bb_jack$ur.lat , PLACE_BBOX_LB_LON > bb_jack$ll.lon,
                                PLACE_BBOX_RT_LON < bb_jack$ur.lon)
df_jack_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_jack$ll.lat,
                                     LATITUDE < bb_jack$ur.lat , LONGITUDE > bb_jack$ll.lon,
                                     LONGITUDE < bb_jack$ur.lon) 

df_jack <- rbind(df_jack_bb, df_jack_exactloc)
df_jack <- df_jack %>% arrange(DATE,TIME)

df_jack_6 <- df_jack %>% filter(DATE=="9/6/2017")
df_jack_7 <- df_jack %>% filter(DATE=="9/7/2017")
df_jack_8 <- df_jack %>% filter(DATE=="9/8/2017")
df_jack_9 <- df_jack %>% filter(DATE=="9/9/2017")
df_jack_6 <- df_jack_6 %>%  mutate(hour= hour(hms(as.character(factor(df_jack_6$TIME- df_jack_6$TIME[1])))))
df_jack_7 <- df_jack_7 %>%  mutate(hour= hour(hms(as.character(factor(df_jack_7$TIME- df_jack_7$TIME[1])))))                                               
df_jack_8 <- df_jack_8 %>%  mutate(hour= hour(hms(as.character(factor(df_jack_8$TIME- df_jack_8$TIME[1])))))
df_jack_9 <- df_jack_9 %>%  mutate(hour= hour(hms(as.character(factor(df_jack_9$TIME- df_jack_9$TIME[1])))))


df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9)  
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_jack_by_hour <- rbind(df_jack_6,df_jack_7,df_jack_8,df_jack_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_jack_hourly <- df_jack_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_jack_hourly <- right_join(num_tweets_jack_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 

num_tweets_jack_hourly <- num_tweets_jack_hourly %>% mutate(hour)

jack_hourly_tweet_dist <- num_tweets_jack_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))

qplot(num_tweets_jack_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Jacksonville")

mean_jack <- sum(jack_hourly_tweet_dist$num_of_tweets*jack_hourly_tweet_dist$freq)
probs = dpois(jack_hourly_tweet_dist$num_of_tweets, lambda=mean_jack)
probs
comp = 1-sum(probs)
chisq.test(x=c(jack_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(jack_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)




# For Gainesville
df_gaine_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_gaine$ll.lat,
                                 PLACE_BBOX_LT_LAT < bb_gaine$ur.lat , PLACE_BBOX_LB_LON > bb_gaine$ll.lon,
                                 PLACE_BBOX_RT_LON < bb_gaine$ur.lon)
df_gaine_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_gaine$ll.lat,
                                      LATITUDE < bb_gaine$ur.lat , LONGITUDE > bb_gaine$ll.lon,
                                      LONGITUDE < bb_gaine$ur.lon) 

df_gaine <- rbind(df_gaine_bb, df_gaine_exactloc)
df_gaine <- df_gaine %>% arrange(DATE,TIME)

df_gaine_6 <- df_gaine %>% filter(DATE=="9/6/2017")
df_gaine_7 <- df_gaine %>% filter(DATE=="9/7/2017")
df_gaine_8 <- df_gaine %>% filter(DATE=="9/8/2017")
df_gaine_9 <- df_gaine %>% filter(DATE=="9/9/2017")
df_gaine_6 <- df_gaine_6 %>%  mutate(hour= hour(hms(as.character(factor(df_gaine_6$TIME- df_gaine_6$TIME[1])))))
df_gaine_7 <- df_gaine_7 %>%  mutate(hour= hour(hms(as.character(factor(df_gaine_7$TIME- df_gaine_7$TIME[1])))))                                               
df_gaine_8 <- df_gaine_8 %>%  mutate(hour= hour(hms(as.character(factor(df_gaine_8$TIME- df_gaine_8$TIME[1])))))
df_gaine_9 <- df_gaine_9 %>%  mutate(hour= hour(hms(as.character(factor(df_gaine_9$TIME- df_gaine_9$TIME[1])))))


df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_gaine_by_hour <- rbind(df_gaine_6,df_gaine_7,df_gaine_8,df_gaine_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_gaine_hourly <- df_gaine_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_gaine_hourly <- right_join(num_tweets_gaine_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 

gaine_hourly_tweet_dist <- num_tweets_gaine_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))
qplot(num_tweets_gaine_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Gainesville")

mean_gaine <- sum(gaine_hourly_tweet_dist$num_of_tweets*gaine_hourly_tweet_dist$freq)
probs = dpois(gaine_hourly_tweet_dist$num_of_tweets, lambda=mean_gaine)
probs
comp = 1-sum(probs)
chisq.test(x=c(gaine_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(gaine_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)


# For entire florida
df_florida_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_florida$ll.lat,
                                   PLACE_BBOX_LT_LAT < bb_florida$ur.lat , PLACE_BBOX_LB_LON > bb_florida$ll.lon,
                                   PLACE_BBOX_RT_LON < bb_florida$ur.lon)
df_florida_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_florida$ll.lat,
                                        LATITUDE < bb_florida$ur.lat , LONGITUDE > bb_florida$ll.lon,
                                        LONGITUDE < bb_florida$ur.lon) 

df_florida <- rbind(df_florida_bb, df_florida_exactloc)
df_florida <- df_florida %>% arrange(DATE,TIME)

df_florida_6 <- df_florida %>% filter(DATE=="9/6/2017")
df_florida_7 <- df_florida %>% filter(DATE=="9/7/2017")
df_florida_8 <- df_florida %>% filter(DATE=="9/8/2017")
df_florida_9 <- df_florida %>% filter(DATE=="9/9/2017")
df_florida_6 <- df_florida_6 %>%  mutate(hour= hour(hms(as.character(factor(df_florida_6$TIME- df_florida_6$TIME[1])))))
df_florida_7 <- df_florida_7 %>%  mutate(hour= hour(hms(as.character(factor(df_florida_7$TIME- df_florida_7$TIME[1])))))                                               
df_florida_8 <- df_florida_8 %>%  mutate(hour= hour(hms(as.character(factor(df_florida_8$TIME- df_florida_8$TIME[1])))))
df_florida_9 <- df_florida_9 %>%  mutate(hour= hour(hms(as.character(factor(df_florida_9$TIME- df_florida_9$TIME[1])))))


df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_florida_by_hour <- rbind(df_florida_6,df_florida_7,df_florida_8,df_florida_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_florida_hourly <- df_florida_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_florida_hourly <- right_join(num_tweets_florida_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 

florida_hourly_tweet_dist <- num_tweets_florida_hourly %>% group_by(num_of_tweets) %>% summarise(num_of_instances=n()) %>%
  mutate(freq = num_of_instances / sum(num_of_instances))
qplot(num_tweets_florida_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
      ylab = "Frequency", main = "Hourly tweet distribution in Florida")

mean_florida <- sum(florida_hourly_tweet_dist$num_of_tweets*florida_hourly_tweet_dist$freq)

probs = dpois(florida_hourly_tweet_dist$num_of_tweets, lambda=mean_florida)
probs
comp = 1-sum(probs)
chisq.test(x=c(florida_hourly_tweet_dist$num_of_instances,0), p=c(probs,comp))
chisq.test(x=c(florida_hourly_tweet_dist$num_of_instances,0), simulate.p.value=TRUE)

# Visulise the arrival hourly arrival of tweets in each city
q1 <- qplot(num_tweets_tampa_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Tampa")

q2 <- qplot(num_tweets_miami_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Miami")

q3 <- qplot(num_tweets_orlando_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Orlando")

q4 <- qplot(num_tweets_jack_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Jacksonville")

q5 <- qplot(num_tweets_gaine_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Gainesville")


q7 <- qplot(num_tweets_tal_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Tallahassee")


q8 <- qplot(num_tweets_florida_hourly$num_of_tweets, geom="histogram", binwidth = 0.5,xlab = "Number of tweets in an hour",
            ylab = "Frequency", main = "Florida")

grid.arrange(q1, q2, q3, q4, q5, q7, nrow = 3)


# For palm
df_palm_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_palm$ll.lat,
                                PLACE_BBOX_LT_LAT < bb_palm$ur.lat , PLACE_BBOX_LB_LON > bb_palm$ll.lon,
                                PLACE_BBOX_RT_LON < bb_palm$ur.lon)
df_palm_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_palm$ll.lat,
                                     LATITUDE < bb_palm$ur.lat , LONGITUDE > bb_palm$ll.lon,
                                     LONGITUDE < bb_palm$ur.lon) 

df_palm <- rbind(df_palm_bb, df_palm_exactloc)
df_palm <- df_palm %>% arrange(DATE,TIME)

df_palm_6 <- df_palm %>% filter(DATE=="9/6/2017")
df_palm_7 <- df_palm %>% filter(DATE=="9/7/2017")
df_palm_8 <- df_palm %>% filter(DATE=="9/8/2017")
df_palm_9 <- df_palm %>% filter(DATE=="9/9/2017")
df_palm_6 <- df_palm_6 %>%  mutate(hour= hour(hms(as.character(factor(df_palm_6$TIME- df_palm_6$TIME[1])))))
df_palm_7 <- df_palm_7 %>%  mutate(hour= hour(hms(as.character(factor(df_palm_7$TIME- df_palm_7$TIME[1])))))                                               
df_palm_8 <- df_palm_8 %>%  mutate(hour= hour(hms(as.character(factor(df_palm_8$TIME- df_palm_8$TIME[1])))))
df_palm_9 <- df_palm_9 %>%  mutate(hour= hour(hms(as.character(factor(df_palm_9$TIME- df_palm_9$TIME[1])))))


df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_palm_by_hour <- rbind(df_palm_6,df_palm_7,df_palm_8,df_palm_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_palm_hourly <- df_palm_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_palm_hourly <- right_join(num_tweets_palm_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% select(DATE,hour,num_of_tweets) 


# For naples
df_naples_bb <-  dfgas_short %>% filter(is.na(LATITUDE), PLACE_BBOX_LB_LAT > bb_naples$ll.lat,
                                  PLACE_BBOX_LT_LAT < bb_naples$ur.lat , PLACE_BBOX_LB_LON > bb_naples$ll.lon,
                                  PLACE_BBOX_RT_LON < bb_naples$ur.lon)
df_naples_exactloc <- dfgas_short %>% filter(!is.na(LATITUDE), LATITUDE > bb_naples$ll.lat,
                                       LATITUDE < bb_naples$ur.lat , LONGITUDE > bb_naples$ll.lon,
                                       LONGITUDE < bb_naples$ur.lon) 

df_naples <- rbind(df_naples_bb, df_naples_exactloc)
df_naples <- df_naples %>% arrange(DATE,TIME)

df_naples_6 <- df_naples %>% filter(DATE=="9/6/2017")
df_naples_7 <- df_naples %>% filter(DATE=="9/7/2017")
df_naples_8 <- df_naples %>% filter(DATE=="9/8/2017")
df_naples_9 <- df_naples %>% filter(DATE=="9/9/2017")
df_naples_6 <- df_naples_6 %>%  mutate(hour= hour(hms(as.character(factor(df_naples_6$TIME- df_naples_6$TIME[1])))))
df_naples_7 <- df_naples_7 %>%  mutate(hour= hour(hms(as.character(factor(df_naples_7$TIME- df_naples_7$TIME[1])))))                                               
df_naples_8 <- df_naples_8 %>%  mutate(hour= hour(hms(as.character(factor(df_naples_8$TIME- df_naples_8$TIME[1])))))
df_naples_9 <- df_naples_9 %>%  mutate(hour= hour(hms(as.character(factor(df_naples_9$TIME- df_naples_9$TIME[1])))))

df6 <- data.frame(DATE=rep("9/6/2017",10),hour= 0:9) 
df7 <- data.frame(DATE=rep("9/7/2017",24),hour= 0:23) 
df8 <- data.frame(DATE=rep("9/8/2017",24),hour= 0:23) 
df9 <- data.frame(DATE=rep("9/9/2017",24),hour= 0:23)


df_naples_by_hour <- rbind(df_naples_6,df_naples_7,df_naples_8,df_naples_9)
df_by_hour <- rbind(df6,df7,df8, df9)
df_by_hour$DATE <- as.character(df_by_hour$DATE)
df_by_hour$num_of_tweets <- rep(0, nrow(df_by_hour))

num_tweets_naples_hourly <- df_naples_by_hour %>% group_by(DATE,hour) %>% summarise(num_of_tweets= n())

num_tweets_naples_hourly <- right_join(num_tweets_naples_hourly, df_by_hour, by = c("DATE","hour")) %>% 
  mutate(num_of_tweets=ifelse(is.na(num_of_tweets.x),0,num_of_tweets.x)) %>% 
  select(DATE,hour,num_of_tweets) 


# Doing time series analysis on tweets in each city

num_tweets_miami_hourly$Hour <-  seq(from = 1, to= nrow(num_tweets_miami_hourly) ) 
miami_gas_tweet_time_series <- ggplot(num_tweets_miami_hourly, aes(Hour,num_of_tweets)) + geom_line() +
  expand_limits(x=c(0,nrow(num_tweets_miami_hourly)), y=c(0, max(num_tweets_miami_hourly$num_of_tweets)))+  xlab("hour") + ylab("number of tweets in miami")
miami_gas_tweet_time_series 


# Modeling time series of miami tweets 
ts_miami <- ts(num_tweets_miami_hourly$num_of_tweets)
plot(ts_miami)

# Checking autocorrelation and partial autocorrelation functions for maimi time series 
acf(ts_miami,nrow(num_tweets_miami_hourly))
pacf(ts_miami,nrow(num_tweets_miami_hourly))
acf(diff(ts_miami),nrow(num_tweets_miami_hourly))
pacf(diff(ts_miami),nrow(num_tweets_miami_hourly))
acf(diff(diff(ts_miami)),nrow(num_tweets_miami_hourly))
pacf(diff(diff(ts_miami)),nrow(num_tweets_miami_hourly))

# Fitting different ARIMA and SARIMA models for miami time series
ts_miami_arima <-arima(ts_miami, order=c(0,1,1))
ts_miami_arima
(1-pnorm(abs(ts_miami_arima$coef)/sqrt(diag(ts_miami_arima$var.coef))))*2


ts_miami_seasonal <-arima(ts_miami, order=c(0,1,1),seasonal=list(order=c(1,0,0), period=14))
ts_miami_seasonal
(1-pnorm(abs(ts_miami_seasonal$coef)/sqrt(diag(ts_miami_seasonal$var.coef))))*2

ts_miami_seasonal2 <-arima((ts_miami), order=c(0,1,1),seasonal=list(order=c(0,0,1), period=15))
ts_miami_seasonal2
(1-pnorm(abs(ts_miami_seasonal2$coef)/sqrt(diag(ts_miami_seasonal2$var.coef))))*2

fore <- predict(ts_miami_seasonal2,n.ahead=24) 

ts.plot(ts_miami,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)

miami_pred <- sum(fore$pred)

ts_miami_seasonal3 <-arima(ts_miami, order=c(0,0,1),seasonal=list(order=c(0,1,1), period=14))
ts_miami_seasonal3
(1-pnorm(abs(ts_miami_seasonal3$coef)/sqrt(diag(ts_miami_seasonal3$var.coef))))*2

ts_miami_seasonal4 <-arima(ts_miami, order=c(0,0,11),seasonal=list(order=c(1,0,0), period=24))
ts_miami_seasonal4
(1-pnorm(abs(ts_miami_seasonal4$coef)/sqrt(diag(ts_miami_seasonal4$var.coef))))*2

ts_miami_seasonal5 <-arima(ts_miami, order=c(2,0,0),seasonal=list(order=c(1,0,0), period=24))
ts_miami_seasonal5
(1-pnorm(abs(ts_miami_seasonal5$coef)/sqrt(diag(ts_miami_seasonal5$var.coef))))*2


# TIme series analysis for all cities

ts_miami <- ts((num_tweets_miami_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_miami_plot <- plot(ts_miami)

acf_miami <- acf(ts_miami,nrow(num_tweets_miami_hourly))
pacf_miami <- pacf(ts_miami,nrow(num_tweets_miami_hourly))
acf_miami_diff <- acf(diff(ts_miami),nrow(num_tweets_miami_hourly))
pacf_miami_diff <- pacf(diff(ts_miami),nrow(num_tweets_miami_hourly))
acf(diff(diff(ts_miami)),nrow(num_tweets_miami_hourly))
pacf(diff(diff(ts_miami)),nrow(num_tweets_miami_hourly))

grid.arrange(ts_miami_plot,acf_miami,pacf_miami,acf_miami_diff, pacf_miami_diff, nrow = 3)

ts_miami_model1 <-arima((ts_miami), order=c(0,1,1))
ts_miami_model1
(1-pnorm(abs(ts_miami_model1$coef)/sqrt(diag(ts_miami_model1$var.coef))))*2

fore <- predict(ts_miami_model1,n.ahead=24) 
miami_pred_ts <- sum(fore$pred)

ts.plot(ts_miami_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)


# Naples 
ts_naples <- ts((num_tweets_naples_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_naples_plot <-plot(ts_naples)

acf(ts_naples,nrow(num_tweets_naples_hourly))
pacf(ts_naples,nrow(num_tweets_naples_hourly))
acf(diff(ts_naples),nrow(num_tweets_naples_hourly))
pacf(diff(ts_naples),nrow(num_tweets_naples_hourly))
acf(diff(diff(ts_naples)),nrow(num_tweets_naples_hourly))
pacf(diff(diff(ts_naples)),nrow(num_tweets_naples_hourly))

ts_naples_model1 <-arima((ts_naples), order=c(0,0,0))
ts_naples_model1
(1-pnorm(abs(ts_naples_model1$coef)/sqrt(diag(ts_naples_model1$var.coef))))*2

fore <- predict(ts_naples_model1,n.ahead=24) 
naples_pred_ts <- sum(fore$pred)

ts.plot(ts_naples_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)


ts_naples <- ts((num_tweets_naples_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_naples <-plot(ts_naples)

acf(ts_naples,nrow(num_tweets_naples_hourly))
pacf(ts_naples,nrow(num_tweets_naples_hourly))
acf(diff(ts_naples),nrow(num_tweets_naples_hourly))
pacf(diff(ts_naples),nrow(num_tweets_naples_hourly))
acf(diff(diff(ts_naples)),nrow(num_tweets_naples_hourly))
pacf(diff(diff(ts_naples)),nrow(num_tweets_naples_hourly))

ts_naples_model1 <-arima((ts_naples), order=c(0,0,0))
ts_naples_model1
(1-pnorm(abs(ts_naples_model1$coef)/sqrt(diag(ts_naples_model1$var.coef))))*2

fore <- predict(ts_naples_model1,n.ahead=24) 
naples_pred_ts <- sum(fore$pred)

ts.plot(ts_naples_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)


# Jacksonville 

ts_jack <- ts((num_tweets_jack_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_jack_plot <-plot(ts_jack)

acf(ts_jack,nrow(num_tweets_jack_hourly))
pacf(ts_jack,nrow(num_tweets_jack_hourly))
acf(diff(ts_jack),nrow(num_tweets_jack_hourly))
pacf(diff(ts_jack),nrow(num_tweets_jack_hourly))
acf(diff(diff(ts_jack)),nrow(num_tweets_jack_hourly))
pacf(diff(diff(ts_jack)),nrow(num_tweets_jack_hourly))

ts_jack_model1 <-arima((ts_jack), order=c(0,0,0))
ts_jack_model1
(1-pnorm(abs(ts_jack_model1$coef)/sqrt(diag(ts_jack_model1$var.coef))))*2

fore <- predict(ts_jack_model1,n.ahead=24) 
jack_pred_ts <- sum(fore$pred)

ts.plot(ts_jack_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)


# Tampa

ts_tampa <- ts((num_tweets_tampa_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_tampa_plot <-plot(ts_tampa)


acf(ts_tampa,nrow(num_tweets_tampa_hourly))
pacf(ts_tampa,nrow(num_tweets_tampa_hourly))
acf(diff(ts_tampa),nrow(num_tweets_tampa_hourly))
pacf(diff(ts_tampa),nrow(num_tweets_tampa_hourly))
acf(diff(diff(ts_tampa)),nrow(num_tweets_tampa_hourly))
pacf(diff(diff(ts_tampa)),nrow(num_tweets_tampa_hourly))

ts_tampa_model1 <-arima((ts_tampa), order=c(1,0,0))
ts_tampa_model1
(1-pnorm(abs(ts_tampa_model1$coef)/sqrt(diag(ts_tampa_model1$var.coef))))*2

#ts_tampa_model1 <-arima((ts_tampa), order=c(1,1,0),seasonal=list(order=c(1,0,0), period=14))
#ts_tampa_model1
#(1-pnorm(abs(ts_tampa_model1$coef)/sqrt(diag(ts_tampa_model1$var.coef))))*2

#ts_tampa_model1 <-arima((ts_tampa), order=c(1,0,0))
#ts_tampa_model1
#(1-pnorm(abs(ts_tampa_model1$coef)/sqrt(diag(ts_tampa_model1$var.coef))))*2

#ts_tampa_model1 <-arima((ts_tampa), order=c(0,0,1))
#ts_tampa_model1
#(1-pnorm(abs(ts_tampa_model1$coef)/sqrt(diag(ts_tampa_model1$var.coef))))*2


fore <- predict(ts_tampa_model1,n.ahead=24) 
tampa_pred_ts <- sum(fore$pred)

ts.plot(ts_tampa_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)


# Gainesville

ts_gaine <- ts((num_tweets_gaine_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_gaine_plot <-plot(ts_gaine)

acf(ts_gaine,nrow(num_tweets_gaine_hourly))
pacf(ts_gaine,nrow(num_tweets_gaine_hourly))
acf(diff(ts_gaine),nrow(num_tweets_gaine_hourly))
pacf(diff(ts_gaine),nrow(num_tweets_gaine_hourly))
acf(diff(diff(ts_gaine)),nrow(num_tweets_gaine_hourly))
pacf(diff(diff(ts_gaine)),nrow(num_tweets_gaine_hourly))


#ts_gaine_model1 <-arima((ts_gaine), order=c(1,0,0))
#ts_gaine_model1
#(1-pnorm(abs(ts_gaine_model1$coef)/sqrt(diag(ts_gaine_model1$var.coef))))*2

#ts_gaine_model1 <-arima((ts_gaine), order=c(0,0,1))
#ts_gaine_model1
#(1-pnorm(abs(ts_gaine_model1$coef)/sqrt(diag(ts_gaine_model1$var.coef))))*2

#ts_gaine_model1 <-arima((ts_gaine), order=c(1,1,0))
#ts_gaine_model1
#(1-pnorm(abs(ts_gaine_model1$coef)/sqrt(diag(ts_gaine_model1$var.coef))))*2

ts_gaine_model1 <-arima((ts_gaine), order=c(0,1,1))
ts_gaine_model1
(1-pnorm(abs(ts_gaine_model1$coef)/sqrt(diag(ts_gaine_model1$var.coef))))*2

fore <- predict(ts_gaine_model1,n.ahead=24) 
gaine_pred_ts <- sum(fore$pred)



ts.plot(ts_gaine_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)

# West Palm Beach

ts_palm <- ts((num_tweets_palm_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_palm_plot <-plot(ts_palm)

acf(ts_palm,nrow(num_tweets_palm_hourly))
pacf(ts_palm,nrow(num_tweets_palm_hourly))
acf(diff(ts_palm),nrow(num_tweets_palm_hourly))
pacf(diff(ts_palm),nrow(num_tweets_palm_hourly))
acf(diff(diff(ts_palm)),nrow(num_tweets_palm_hourly))
pacf(diff(diff(ts_palm)),nrow(num_tweets_palm_hourly))

ts_palm_model1 <-arima((ts_palm), order=c(0,1,1))
ts_palm_model1
(1-pnorm(abs(ts_palm_model1$coef)/sqrt(diag(ts_palm_model1$var.coef))))*2


#ts_palm_model1 <-arima((ts_palm), order=c(0,0,0))
#ts_palm_model1
#(1-pnorm(abs(ts_palm_model1$coef)/sqrt(diag(ts_palm_model1$var.coef))))*2

fore <- predict(ts_palm_model1,n.ahead=24) 
palm_pred_ts <- sum(fore$pred)

ts.plot(ts_palm_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)


# Orlando 

ts_orlando <- ts((num_tweets_orlando_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_orland_plot <-plot(ts_orlando)

acf(ts_orlando,nrow(num_tweets_orlando_hourly))
pacf(ts_orlando,nrow(num_tweets_orlando_hourly))
acf(diff(ts_orlando),nrow(num_tweets_orlando_hourly))
pacf(diff(ts_orlando),nrow(num_tweets_orlando_hourly))
acf(diff(diff(ts_orlando)),nrow(num_tweets_orlando_hourly))
pacf(diff(diff(ts_orlando)),nrow(num_tweets_orlando_hourly))

ts_orlando_model1 <-arima((ts_orlando), order=c(0,0,0))
ts_orlando_model1
(1-pnorm(abs(ts_orlando_model1$coef)/sqrt(diag(ts_orlando_model1$var.coef))))*2

fore <- predict(ts_orlando_model1,n.ahead=24) 
orlando_pred_ts <- sum(fore$pred)

ts.plot(ts_orlando_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)



# Tallahassee

ts_tal <- ts((num_tweets_tal_hourly %>% filter(DATE != "9/9/2017" ))$num_of_tweets)
ts_tal_plot <-plot(ts_tal)

acf(ts_tal,nrow(num_tweets_tal_hourly))
pacf(ts_tal,nrow(num_tweets_tal_hourly))
acf(diff(ts_tal),nrow(num_tweets_tal_hourly))
pacf(diff(ts_tal),nrow(num_tweets_tal_hourly))
acf(diff(diff(ts_tal)),nrow(num_tweets_tal_hourly))
pacf(diff(diff(ts_tal)),nrow(num_tweets_tal_hourly))

ts_tal_model1 <-arima((ts_tal), order=c(0,0,0))
ts_tal_model1
(1-pnorm(abs(ts_tal_model1$coef)/sqrt(diag(ts_tal_model1$var.coef))))*2

fore <- predict(ts_tal_model1,n.ahead=24) 
tal_pred_ts <- sum(fore$pred)

ts.plot(ts_tal_model1,fore$pred,col=1:2,ylab='num_of_tweets')
lines(fore$pred,type="p",col=4)