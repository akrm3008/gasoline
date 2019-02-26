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
library(scales)


# Making document term matrix
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
#dtm <- DocumentTermMatrix(corpus)


# Making a Word Cloud to visuaise frequent words many of which will be features(unigrams)
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus, min.freq=5, max.words = 50, random.order = TRUE, col = pal)


# Feature type 1 -> Unigrams type -1  (using term frequency,tf)
# Making a document term matrix with term frequency (tf)
# These frequent words will form our fetaures (unigrams)
# Vary frequency to vary the number of unigrams that wil be used as features

tf_scores = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
tf_threshold <- 350  # Assign the least number of times the word appear in the document (the threshold for the term frequency we use as filter)
freq_terms <- findFreqTerms(dtm,n)  # find the frequent terms with this threshold (important - unigrams)
length(freq_terms) # number of frequent unigrams found 
#dtm <- DocumentTermMatrix(corpus,list(dictionary = freq_terms)) # optional that if you want to use in topic modeling 
#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
# dtm  <- dtm[rowTotals> 0, ]           #remove all docs without words
unigrams_type1 <- as.data.frame((as.matrix(dtm))) # getting unigrams as encoded data-frame, The elements of this dataframe are the term frequency for each document
unigrams_type1 <- unigrams_type1 %>% select(freq_terms)
# Have to use wightsmart tecnique in the future  


# Feature type  2 -> Unigrams type -2  (term frequency - inverse document frequency, tf-idf)
# Making a document term matrix with term frequency -inverse document frequency (tf-idf)
# These words with high tf-idf score will form our fetaures (unigrams)
# Vary tf-idf score to vary the number of unigrams
dtm2 <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) # constructed dtm with weighting as tf-idf scores
tf_idf_scores = data.frame(sort(colSums(as.matrix(dtm2)), decreasing=TRUE)) # Sorted the terms according to their tf-idf score
sorted_tf_idf <- rownames(tf_idf_scores) 
n <- 5  # choose the numebr of terms you want to use as unigrams 
tf_idf_freq_terms <- sorted_tf_idf[1:n]
# dtm2 <- DocumentTermMatrix(corpus,control= list(dictionary = tf_idf_freq_terms, weighting = weightTfIdf)) # this changes the tf-idf score so not useful
unigrams_type2 <- as.data.frame((as.matrix(dtm2)))
unigrams_type2 <- unigrams_type2 %>% select(tf_idf_freq_terms)
# Have to use wightsmart tecnique in the future  

#Comparing tf and tf_idf scores 


# Feature type 3 - Abstract/Latent Topics using tf in dtm 
# Topic Modeling using 4 different modeling techniques to extract latent topics to be used as features 
#doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
#dtm_finite_len <- DocumentTermMatrix(corpus[doc.lengths > 0]) # Each row of the input matrix needs to contain at least one non-zero entry for topic modeling 
SEED = 123 # for reproducibility 
n <- nrow(dtm)
splitter <- sample(1:n, round(n * 0.75))  # Splitter to divide data into train and valdation set
train_set <- dtm[splitter, ]  
valid_set <- dtm[-splitter, ]

# In the follwing code we do model selection 
# We vary the the number of topics, k and the topic modeling techniques for each k
# We measure the perplexity of each model on a hold-out validation set 
# We gather our result in a table - result table and also plot the perplexities of different models
k = c(2, 4, 5, 8, 10, 12, 15, 20, 40, 50, 100)  # vector containing number of topics , k
perplexity = c(0,0,0,0,0,0,0,0,0,0,0)  
results = data.frame(number_of_topics = k, perplexity_CTM = perplexity, perplexity_VEM = perplexity, 
                     perplexity_VEM_Fixed = perplexity, perplexity_Gibbs = perplexity)  # initialising a result table

# The experiment
for (val in k){

models <- list(
  CTM       = CTM(train_set, k = val, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(train_set, k = val, control = list(seed = SEED)),
  VEM_Fixed = LDA(train_set, k = val, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(train_set, k = val, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                thin = 100,    iter = 1000))
)

results[results$number_of_topics == val,]$perplexity_CTM = perplexity(models$CTM, newdata = valid_set)
results[results$number_of_topics == val,]$perplexity_VEM = perplexity(models$VEM, newdata = valid_set)
results[results$number_of_topics == val,]$perplexity_VEM_Fixed = perplexity(models$VEM_Fixed, newdata = valid_set)
results[results$number_of_topics == val,]$perplexity_Gibbs = perplexity(models$Gibbs, newdata = valid_set)

}

# Plotting the perplexities

ggplot(results, aes(x= number_of_topics,  y= perplexity_CTM))  +
  geom_line(colour = "CTM")

ggplot(results, aes(number_of_topics)) + 
  geom_line(aes(y = perplexity_VEM, colour = "VEM")) +
  geom_line(aes(y = perplexity_VEM_Fixed, colour = "VEM_Fixed")) 
  

ggplot(results, aes(x =number_of_topics,  y= perplexity_Gibbs))  +
  geom_line(colour = "blue")



# From the results we see 15 topics with LDA with gibbs is the best model with smallest perplexity value
# Now getting topics as features for classfication
val = 15
Gibbs  = LDA(dtm, k = val, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                       thin = 100,    iter = 1000))

# These assignments will be used as features
topics_assignment <- topics(Gibbs)

# Making final dataframe with unigrams and assigned latent topics as features and label column (probable shortage)
topics_assignment <- as.data.frame(topics_assignment) 
topics_df <- topics_assignment %>% mutate(doc_id = as.numeric(Docs(dtm)))   
unigrams_type1 <- as.data.frame(unigrams_type1)
unigrams_df <- unigrams_type1 %>% mutate(doc_id = as.numeric(Docs(dtm)))
dfgas2 <- right_join(dfgas, unigrams_df, by = "doc_id")
dfgas2 <- right_join(dfgas2, topics_df, by = "doc_id")

data <- dfgas2 %>% select(c(colnames(topics_df), colnames(unigrams_df), label = `Probable Shortage`)) 
data <- data %>% select(-doc_id)
data <- data %>% mutate_if(is.numeric, as.factor)

# Getting training and test data
smp_size <- floor(0.7 * nrow(data)) # train:train = 70:30 
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

# Running SVM

# Training model
model_svm <- svm(label ~. , train)

# predictions on test data
pred <- predict(model_svm, test)

# Confusion matrix 
conf.mat <- table(pred = pred, true = test$label)

# Precison, Recall, F-score
precision <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
recall <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
f_score <- (2* precision* recall)/(precision + recall)


# Using 15 topics from LDA with Gibbs and top 5 unigrams type 1, we achieve
# precision = 0.9686684, recall = 0.663092,  F-score = 0.7872679
# Now we vary the number of topics and fix the term frequncy threshold (fixing number of unigrams)
# and see how the SVM model performace on varying the number of topics


k = c(2, 4, 5, 8, 10, 12, 15, 20, 40)  # vector containing number of topics , k
number_of_unigrams = c(5,5,5,5,5,5,5,5,5)
term_frequency_threshold = c(350, 350, 350, 350, 350, 350, 350, 350 , 350)
precision  = c(0,0,0,0,0,0,0,0,0)  
recall = c(0,0,0,0,0,0,0,0,0) 
f_score = c(0,0,0,0,0,0,0,0,0) 
SVM_table1 = data.frame(number_of_topics = k, number_of_unigrams = number_of_unigrams, term_frequency_threshold = term_frequency_threshold,
                     Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table

# The experiment
for (val in k){
Gibbs = LDA(dtm, k = val, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                         thin = 100,    iter = 1000))
topics_assignment <- topics(Gibbs)  
topics_assignment <- as.data.frame(topics_assignment) 
topics_df <- topics_assignment %>% mutate(doc_id = as.numeric(Docs(dtm)))   
unigrams_type1 <- as.data.frame(unigrams_type1)
unigrams_df <- unigrams_type1 %>% mutate(doc_id = as.numeric(Docs(dtm)))
dfgas2 <- right_join(dfgas, unigrams_df, by = "doc_id")
dfgas2 <- right_join(dfgas2, topics_df, by = "doc_id")
data <- dfgas2 %>% select(c(colnames(topics_df), colnames(unigrams_df), label = `Probable Shortage`)) 
data <- data %>% select(-doc_id)
data <- data %>% mutate_if(is.numeric, as.factor)
data <- data[complete.cases(data), ] 
smp_size <- floor(0.7 * nrow(data))  
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
model_svm <- svm(label ~ . , train)
pred <- predict(model_svm, test)
conf.mat <- table(pred = pred, true = test$label)
precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])

SVM_table1[SVM_table1$number_of_topics == val,]$Precision = precision1
SVM_table1[SVM_table1$number_of_topics == val,]$Recall = recall1
SVM_table1[SVM_table1$number_of_topics == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}


# Now we will fix the number of topics and vary the term_frequency threshold/ number_of_unigrams

term_frequency_threshold <- c(350, 300, 250, 200 , 100 , 50)
k <- c(15,15,15,15,15,15)
number_of_unigrams <- c(0,0,0,0,0,0)
precision <-  c(0,0,0,0,0,0)  
recall <- c(0,0,0,0,0,0) 
f_score <- c(0,0,0,0,0,0) 
SVM_table2 <- data.frame(number_of_topics = k, number_of_unigrams = number_of_unigrams, term_frequency_threshold = term_frequency_threshold,
                        Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table


for (val in term_frequency_threshold){
freq_terms <- findFreqTerms(dtm,val) 
number_of_unigrams <- length(freq_terms)
unigrams_type1 <- as.data.frame((as.matrix(dtm))) # getting unigrams as encoded data-frame, The elements of this dataframe are the term frequency for each document
unigrams_type1 <- unigrams_type1 %>% select(freq_terms)


unigrams_type1 <- as.data.frame(unigrams_type1)
unigrams_df <- unigrams_type1 %>% mutate(doc_id = as.numeric(Docs(dtm)))
dfgas2 <- right_join(dfgas, unigrams_df, by = "doc_id")
dfgas2 <- right_join(dfgas2, topics_df, by = "doc_id")
data <- dfgas2 %>% select(c(colnames(topics_df), colnames(unigrams_df), label = `Probable Shortage`)) 
data <- data %>% select(-doc_id)
data <- data %>% mutate_if(is.numeric, as.factor)
data <- data[complete.cases(data), ]
smp_size <- floor(0.7 * nrow(data))  
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
model_svm <- svm(label ~ . , train)
pred <- predict(model_svm, test)
conf.mat <- table(pred = pred, true = test$label)
precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])

SVM_table2[SVM_table2$term_frequency_threshold == val,]$number_of_unigrams = length(freq_terms)
SVM_table2[SVM_table2$term_frequency_threshold == val,]$Precision = precision1
SVM_table2[SVM_table2$term_frequency_threshold == val,]$Recall = recall1
SVM_table2[SVM_table2$term_frequency_threshold == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}

# Testing with just unigrams as features 
set.seed(123)
term_frequency_threshold <- c(350, 300, 250, 200 , 100 , 50)
number_of_unigrams <- c(0,0,0,0,0,0)
precision <-  c(0,0,0,0,0,0)  
recall <- c(0,0,0,0,0,0) 
f_score <- c(0,0,0,0,0,0) 
SVM_table3 <- data.frame( number_of_unigrams = number_of_unigrams, term_frequency_threshold = term_frequency_threshold,
                         Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table


for (val in term_frequency_threshold){
  freq_terms <- findFreqTerms(dtm,val) 
  number_of_unigrams <- length(freq_terms)
  unigrams_type1 <- as.data.frame((as.matrix(dtm))) # getting unigrams as encoded data-frame, The elements of this dataframe are the term frequency for each document
  unigrams_type1 <- unigrams_type1 %>% select(freq_terms)
  
  
  unigrams_type1 <- as.data.frame(unigrams_type1)
  unigrams_df <- unigrams_type1 %>% mutate(doc_id = as.numeric(Docs(dtm)))
  dfgas2 <- right_join(dfgas, unigrams_df, by = "doc_id")
  dfgas2 <- right_join(dfgas2, topics_df, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(unigrams_df), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate_if(is.numeric, as.factor)
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  
  SVM_table3[SVM_table3$term_frequency_threshold == val,]$number_of_unigrams = length(freq_terms)
  SVM_table3[SVM_table3$term_frequency_threshold == val,]$Precision = precision1
  SVM_table3[SVM_table3$term_frequency_threshold == val,]$Recall = recall1
  SVM_table3[SVM_table3$term_frequency_threshold == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}



## Testing with just topics as features 

k = c(2, 4, 5, 8, 10, 12, 15, 20, 40)  # vector containing number of topics , k
precision  = c(0,0,0,0,0,0,0,0,0)  
recall = c(0,0,0,0,0,0,0,0,0) 
f_score = c(0,0,0,0,0,0,0,0,0) 
SVM_table4 = data.frame(number_of_topics = k, 
                        Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table

# The experiment
for (val in k){
  Gibbs = LDA(dtm, k = val, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                             thin = 100,    iter = 1000))
  topics_assignment <- topics(Gibbs)  
  topics_assignment <- as.data.frame(topics_assignment) 
  topics_df <- topics_assignment %>% mutate(doc_id = as.numeric(Docs(dtm)))   
  dfgas2 <- right_join(dfgas, topics_df, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(topics_df), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate_if(is.numeric, as.factor)
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  
  SVM_table4[SVM_table4$number_of_topics == val,]$Precision = precision1
  SVM_table4[SVM_table4$number_of_topics == val,]$Recall = recall1
  SVM_table4[SVM_table4$number_of_topics == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}


# Testing with just CTM assignment as features 


k = c(2, 5, 8, 10, 15, 20, 40)  # vector containing number of topics , k
precision  = c(0,0,0,0,0,0,0)  
recall = c(0,0,0,0,0,0,0) 
f_score = c(0,0,0,0,0,0,0) 
SVM_table5 = data.frame(number_of_topics = k, 
                        Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table

# The experiment
for (val in k){
  CTM = CTM(dtm, k = val, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
  topics_assignment <- topics(CTM)  
  topics_assignment <- as.data.frame(topics_assignment) 
  topics_df <- topics_assignment %>% mutate(doc_id = as.numeric(Docs(dtm)))   
  dfgas2 <- right_join(dfgas, topics_df, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(topics_df), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate_if(is.numeric, as.factor)
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  
  SVM_table5[SVM_table5$number_of_topics == val,]$Precision = precision1
  SVM_table5[SVM_table5$number_of_topics == val,]$Recall = recall1
  SVM_table5[SVM_table5$number_of_topics == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}


# Testing with just VEM assignment as features 


k = c(2, 5, 10, 15, 20, 40)  # vector containing number of topics , k
precision  = c(0,0,0,0,0,0)  
recall = c(0,0,0,0,0,0) 
f_score = c(0,0,0,0,0,0) 
SVM_table6 = data.frame(number_of_topics = k, 
                        Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table

# The experiment
for (val in k){
  VEM = LDA(dtm, k = val, control = list(seed = SEED))
  topics_assignment <- topics(VEM)  
  topics_assignment <- as.data.frame(topics_assignment) 
  topics_df <- topics_assignment %>% mutate(doc_id = as.numeric(Docs(dtm)))   
  dfgas2 <- right_join(dfgas, topics_df, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(topics_df), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate_if(is.numeric, as.factor)
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  
  SVM_table6[SVM_table6$number_of_topics == val,]$Precision = precision1
  SVM_table6[SVM_table6$number_of_topics == val,]$Recall = recall1
  SVM_table6[SVM_table6$number_of_topics == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}



# Testing with topic assignment from all topics and unigrams as features

k = c(2, 4, 5, 8, 10, 12, 15, 20, 40)  # vector containing number of topics , k
number_of_unigrams = c(5,5,5,5,5,5,5,5,5)
term_frequency_threshold = c(350, 350, 350, 350, 350, 350, 350, 350 , 350)
precision  = c(0,0,0,0,0,0,0,0,0)  
recall = c(0,0,0,0,0,0,0,0,0) 
f_score = c(0,0,0,0,0,0,0,0,0) 
SVM_table7 = data.frame(number_of_topics = k, number_of_unigrams = number_of_unigrams, term_frequency_threshold = term_frequency_threshold,
                        Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table

# The experiment
for (val in k){
  models <- list(
    CTM       = CTM(dtm, k = val, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
    VEM       = LDA(dtm, k = val, control = list(seed = SEED)),
    VEM_Fixed = LDA(dtm, k = val, control = list(estimate.alpha = FALSE, seed = SEED)),
    Gibbs     = LDA(dtm, k = val, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                         thin = 100,    iter = 1000))
  )                                                       
  topics_assignment <- sapply(models, topics)  
  topics_assignment <- as.data.frame(topics_assignment)
  topics_assignment$VEM <- topics_assignment$VEM + 5
  topics_assignment$VEM_Fixed <- topics_assignment$VEM_Fixed + 10
  topics_assignment$Gibbs <- topics_assignment$Gibbs + 15
  topics_df <- topics_assignment %>% mutate(doc_id = as.numeric(Docs(dtm)))   
  unigrams_type1 <- as.data.frame(unigrams_type1)
  unigrams_df <- unigrams_type1 %>% mutate(doc_id = as.numeric(Docs(dtm)))
  dfgas2 <- right_join(dfgas, unigrams_df, by = "doc_id")
  dfgas2 <- right_join(dfgas2, topics_df, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(topics_df), colnames(unigrams_df), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate_if(is.numeric, as.factor)
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  
  SVM_table7[SVM_table7$number_of_topics == val,]$Precision = precision1
  SVM_table7[SVM_table7$number_of_topics == val,]$Recall = recall1
  SVM_table7[SVM_table7$number_of_topics == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}



# Testing with unugrams type-2 as features

dtm2 <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) # constructed dtm with weighting as tf-idf scores
tf_idf_scores = data.frame(sort(colSums(as.matrix(dtm2)), decreasing=TRUE)) # Sorted the terms according to their tf-idf score
sorted_tf_idf <- rownames(tf_idf_scores) 
# dtm2 <- DocumentTermMatrix(corpus,control= list(dictionary = tf_idf_freq_terms, weighting = weightTfIdf)) # this changes the tf-idf score so not useful



number_of_unigrams <- c(5,11,15,18,20,25,30,50,100)
precision <-  c(0,0,0,0,0,0,0,0,0)  
recall <- c(0,0,0,0,0,0,0,0,0) 
f_score <- c(0,0,0,0,0,0,0,0,0) 
SVM_table8 <- data.frame(number_of_unigrams = number_of_unigrams, 
                          Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table


for (val in number_of_unigrams){
  tf_idf_freq_terms <- sorted_tf_idf[1:val]
  unigrams_type2 <- as.data.frame((as.matrix(dtm2)))
  unigrams_type2 <- unigrams_type2 %>% select(tf_idf_freq_terms)
  unigrams_df2 <- unigrams_type2 %>% mutate(doc_id = as.numeric(Docs(dtm2)))
  dfgas2 <- right_join(dfgas, unigrams_df2, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(unigrams_df2), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate(label= as.factor(label))
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  SVM_table8[SVM_table8$number_of_unigrams == val,]$Precision = precision1
  SVM_table8[SVM_table8$number_of_unigrams == val,]$Recall = recall1
  SVM_table8[SVM_table8$number_of_unigrams == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}


# Testing with unugrams type-2 and LDA Gibbs topic assignment as features

dtm2 <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) # constructed dtm with weighting as tf-idf scores
tf_idf_scores = data.frame(sort(colSums(as.matrix(dtm2)), decreasing=TRUE)) # Sorted the terms according to their tf-idf score
sorted_tf_idf <- rownames(tf_idf_scores) 
# dtm2 <- DocumentTermMatrix(corpus,control= list(dictionary = tf_idf_freq_terms, weighting = weightTfIdf)) # this changes the tf-idf score so not useful



number_of_unigrams <- c(5,11,15,18,20,25,30,50,100)
k <- c(15,15,15,15,15,15,15,15,15)
precision <-  c(0,0,0,0,0,0,0,0,0)  
recall <- c(0,0,0,0,0,0,0,0,0)  
f_score <- c(0,0,0,0,0,0,0,0,0) 
SVM_table8 <- data.frame(number_of_topics = k, number_of_unigrams = number_of_unigrams, 
                         Precision = precision, Recall = recall,F_score= f_score )  # initialising a result table


for (val in number_of_unigrams){
  tf_idf_freq_terms <- sorted_tf_idf[1:val]
  unigrams_type2 <- as.data.frame((as.matrix(dtm2)))
  unigrams_type2 <- unigrams_type2 %>% select(tf_idf_freq_terms)
  unigrams_df2 <- unigrams_type2 %>% mutate(doc_id = as.numeric(Docs(dtm2)))
  dfgas2 <- right_join(dfgas, unigrams_df2, by = "doc_id")
  data <- dfgas2 %>% select(c(colnames(unigrams_df2), label = `Probable Shortage`)) 
  data <- data %>% select(-doc_id)
  data <- data %>% mutate(label= as.factor(label))
  data <- data[complete.cases(data), ]
  smp_size <- floor(0.7 * nrow(data))  
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model_svm <- svm(label ~ . , train)
  pred <- predict(model_svm, test)
  conf.mat <- table(pred = pred, true = test$label)
  precision1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])
  recall1 <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
  SVM_table8[SVM_table8$number_of_unigrams == val,]$Precision = precision1
  SVM_table8[SVM_table8$number_of_unigrams == val,]$Recall = recall1
  SVM_table8[SVM_table8$number_of_unigrams == val,]$F_score = (2* precision1* recall1)/(precision1 + recall1)
}