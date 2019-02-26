# In this script I filter out tweets from the tweet data that are talking about gasoline on dates
# gasoline shortage was observed in Florida 

library(data.table)
library(dplyr)
library(tm)


# Getting Data

Path <- "/Users/abhinavkhare/Documents/github_tweets_to_gas_shortage/tweets.csv"


df <- fread(Path)


# Adding column SNO.
df <- df %>% mutate(doc_id = seq(from= 1,to=1048575))

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
gas_tweets <- dfgas %>% select(doc_id,TWEET_TEXT)

Path2 <-"/Users/abhinavkhare/Documents/Phd Project/Data/Data"
setwd(Path2)
fwrite(gas_tweets, file = "gas_tweets_till15th.csv")

# Joining labeled tweets with the rest of the data

labeled_data <- fread("/Users/abhinavkhare/Documents/Phd Project/Data/Data/gas_tweets_till15th_v2.csv")
labeled_data <- labeled_data[,-2]
colnames(labeled_data)[1] <- "doc_id"
dfgas <- full_join(dfgas, labeled_data, by = "doc_id")

#  Subsetting data for making cropus 
#dfgas <- dfgas[,-1]
gas_tweets <- dfgas %>% select(doc_id,TWEET_TEXT)
colnames(gas_tweets)[2] <- "text"


# Preprocessing tweets 
gas_tweets$text <- iconv(gas_tweets$text, to = "ASCII", sub = " ")
gas_tweets$text <- tolower(gas_tweets$text)
gas_tweets$text <- gsub("rt", " ", gas_tweets$text) 
gas_tweets$text <- gsub("@\\w+", " ", gas_tweets$text)  # Remove user names (all proper names if you're wise!)
gas_tweets$text <- gsub("http.+ |http.+$", " ", gas_tweets$text)  # Remove links
gas_tweets$text <- gsub("[[:punct:]]", " ", gas_tweets$text)  # Remove punctuation
gas_tweets$text <- gsub("[ |\t]{2,}", " ", gas_tweets$text)  # Remove tabs
gas_tweets$text <- gsub("amp", " ", gas_tweets$text)  # "&" is "&amp" in HTML, so after punctuation removed ...
gas_tweets$text<- gsub("^ ", "", gas_tweets$text)  # Leading blanks
gas_tweets$text <- gsub(" $", "", gas_tweets$text)  # Lagging blanks
gas_tweets$text <- gsub(" +", " ", gas_tweets$text) # General spaces (should just do all whitespaces no?)


# Changing columnnames to preserve  document id in corpus


# making corpus
corpus = VCorpus(DataframeSource(gas_tweets))
#corpus <- Corpus(VectorSource(gas_tweets)) 

# Remove English stop words.
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Stem the words
corpus <- tm_map(corpus, stemDocument)

# removing cursewords
corpus <- tm_map(corpus, removeWords, c("fuck"))

