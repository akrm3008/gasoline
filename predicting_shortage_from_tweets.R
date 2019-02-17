# In this script we predict number of stations out of gas in a city using forecasted tweets

truth <- fread("/Users/abhinavkhare/Documents/Phd Project/Results/results_with_final_data/ground_truth_test3.csv")
test <- fread("/Users/abhinavkhare/Documents/Phd Project/Data/Data/tweet_test.csv")

# Getting number of gas stations that will be empty using percentage of gas station with shortage and number of gas stations
truth <- truth %>% mutate(num_of_gas_stations_empty = avg_shortage_day*number_of_gas_stations) %>% select(-avg_shortage_day)
test <- test %>% mutate(num_of_gas_stations_empty = avg_shortage_day*number_of_gas_stations) %>% select(-avg_shortage_day)

# Columns/variables are removed after model selection
truth <- truth[, c(1:6,8,10,13,16)]
truth <- truth[, c(1:6,8,10,13,16)]

# Fiitng model 

model <- glm(num_of_gas_stations_empty ~. ,family = poisson(link=log), data=truth_new1)


# replacing number of tweets (original) by forecasted tweets in previous stage

test$num_of_tweets <- tweet_pred[,1]
test$shortage_pred <- predict(model, newdata=test, type= "response")

# Comparing result to truth
test$num_of_gas_stations_empty
test$shortage_pred

