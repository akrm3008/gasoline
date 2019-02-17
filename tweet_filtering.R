# In this script I filter out tweets from the tweet data that are talking about gasoline on dates
# gasoline shortage was observed in Florida 

library(data.table)
library(dplyr)
library(tm)


# Getting Data
Path <- "/Users/abhinavkhare/Documents/Phd Project/Data/Data/tweets.csv"


df <- fread(Path)


# Adding column SNO.
df <- df %>% mutate(SNO= seq(from= 1,to=1048575))

# Retreiving the data for dates of hurricane onset, hurricane landfall and few days after disaster
dates <- unique(df$DATE) 
dates
df <-  df %>% filter(DATE %in% c("9/6/2017","9/7/2017" ,"9/8/2017","9/9/2017", "9/10/2017","9/11/2017","9/12/2017"
                                   ,"9/13/2017","9/14/2017","9/15/2017"))


# Filtering out gasoline-related tweets and hashtags i.e tweets and hashtags that talk about or mention
# "gasoline"

# Seeing frequency of hashtags
hashtag <- c(df$HASHTAG1, df$HASHTAG2,df$HASHTAG3,df$HASHTAG4)
hashtagfreq <- sort(table(hashtag), decreasing = TRUE)
length(unique(hashtagpre))
head(hashtagfreq,100)


# Looking for "gas" in hashtags
hashtag_gas <- grep("^gas", hashtag,value=TRUE )
hashtag_gas2 <- grep("gas", hashtag,value=TRUE )
other_hashtags <- c("nogas","outofgas","findgas")
hashtag_gas <- c(hashtag_gas,other_hashtags)

# Looking for "gas" in tweets

tweets_gas <- grep("\\bgas", df$TWEET_TEXT, value=TRUE)



# Filtering out data that has tweets or hashtags that are talking about "gasoline"
dfgas <- df %>% filter(HASHTAG1 %in% hashtag_gas | HASHTAG2 %in% hashtag_gas|
                            HASHTAG3 %in% hashtag_gas|HASHTAG4 %in% hashtag_gas |
                            HASHTAG5 %in% hashtag_gas |TWEET_TEXT %in% tweets_gas )


# Getiting a csv file to lable tweets that are about gasoline shortage 
gas_tweets <- dfgas %>% select(SNO,TWEET_TEXT)


setwd(Path2)
fwrite(gas_tweets, file = "gas_tweets_till15th.csv")

# Joining labeled tweets with the rest of the data
Path3 <- ''
labeled_data <- fread("/Users/abhinavkhare/Documents/Phd Project/Data/Data/gas_tweets_till15th.csv")
labeled_data <- labeled_data[,-2]
dfgas <- full_join(dfgas, labeled_data, by = "SNO")


# Extracitng tweets to further preprocess
gas_tweets <- dfgas$TWEET_TEXT

# Preprocessing tweets 
gas_tweets <- iconv(gas_tweets, to = "ASCII", sub = " ")
gas_tweets <- tolower(gas_tweets)
gas_tweets <- gsub("rt", " ", gas_tweets) 
gas_tweets <- gsub("@\\w+", " ", gas_tweets)  # Remove user names (all proper names if you're wise!)
gas_tweets <- gsub("http.+ |http.+$", " ", gas_tweets)  # Remove links
gas_tweets <- gsub("[[:punct:]]", " ", gas_tweets)  # Remove punctuation
gas_tweets <- gsub("[ |\t]{2,}", " ", gas_tweets)  # Remove tabs
gas_tweets <- gsub("amp", " ", gas_tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
gas_tweets <- gsub("^ ", "", gas_tweets)  # Leading blanks
gas_tweets <- gsub(" $", "", gas_tweets)  # Lagging blanks
gas_tweets <- gsub(" +", " ", gas_tweets) # General spaces (should just do all whitespaces no?)

# making corpus
corpus <- Corpus(VectorSource(gas_tweets)) 

# Remove English stop words.
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Stem the words
corpus <- tm_map(corpus, stemDocument)

# removing cursewords
corpus <- tm_map(corpus, removeWords, c("fuck"))

