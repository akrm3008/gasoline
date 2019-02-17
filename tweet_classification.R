# In this script I classify tweets I filtered using "tweets_filtering.R" and classify them 
# into tweets that are talking abput gasoline shortage  ("gasoline-shortage tweets") and those 
# that are not ("non-gasoline-shortage tweets")
# using an SVM classifier and unigrams and latent topics as features


library(data.table)
library(dplyr)
library(ggplot2)
library(tm)
library(ggmap)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)
library(e1071)



# Get the lengths and make sure we only create a DTM for tweets with non-zero content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
dtm <- DocumentTermMatrix(corpus)


# Making a Word Cloud to visuaise frequent words many of which will be features(unigrams)
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus, min.freq=5, max.words = 50, random.order = TRUE, col = pal)


# Making a document term matrix with frequent words 
# These frequent words will form our fetaures (unigrams)
# Vary frequency to vary the number of unigrams that wil be used as features 
freq_terms <- findFreqTerms(dtm,350)
length(freq_terms)
dtm2 <- DocumentTermMatrix(corpus,list(dictionary = freq_terms)) # optional that if you want to use in topic modeling 
rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2  <- dtm2[rowTotals> 0, ]           #remove all docs without words


# Topic Modeling using 4 different modeling techniques to extract latent topics to be used as features 
SEED = sample(1:1000000, 1)  
k = 5  # Searching for 5 topics in the tweet corpus
models <- list(
  CTM       = CTM(dtm2, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(dtm2, k = k, control = list(seed = SEED)),
  VEM_Fixed = LDA(dtm2, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(dtm2, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                thin = 100,    iter = 1000))
)

# Top 10 terms of each topic for each model
topic_words <- lapply(models, terms, 10)

# Topic Assignment to each tweet using the 4 models 
# These assignments will be used as features
assignments <- sapply(models, topics) 
assignments

# Building a features dataframe containing unigrams (as features)
wordMatrix <- as.data.frame((as.matrix(dtm2)))


# Making final dataframe with unigrams and assigned latent topics as features 
data <- cbind(wordMatrix,assignments,dfgas$label)


# Getting training and test data
smp_size <- floor(0.7 * nrow(data)) # train:train = 70:30 
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

# Running SVM

# Training model
model_svm <- svm(label ~ . , train)

# predictions on test data
pred <- predict(model_svm, test)

#Confusion matrix 
conf.mat <- table(pred = pred, true = test$label_ps)

# Precison, Recall, F-score
precision<- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
recall<-conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
f_score <- (2* precision* recall)/(precision + recall)





