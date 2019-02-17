# In this script I foreast number of tweets on the next day for all cities (9th Sept 2017) using 3 methods:
# a) Poisson Regression 
# b) Time Series
# c) A hybrid loss function method (HLF) that combines Poisson Regression and Time Series 

library(standardize)


# a) Poisson Regression

train <- fread("/Users/abhinavkhare/Documents/Phd Project/Results/results_with_final_data/num_of_tweets_next_day2.csv")
test <- fread("/Users/abhinavkhare/Documents/Phd Project/Results/results_with_final_data/tweet_test2.csv")

# In the following code the columns/variables has been removed after model selection/fitting model multiple times 
train <- train[,-1]
train<- train[, 2:17]
train <- train[,-16]
train <- train[,-c(12:13)]
train <- train[,-c(7:8)]
train <- train[,c(1:8,10)]

test <- test[,-1]
test <- test[, 2:17]
test <- test[,-16]
test  <- test[,-c(12:13)]
test <- test[,-c(7:8)]
test <- test[,c(1:8,10)]

# Best model

model_tweet_forecast <- glm(num_of_tweets ~.,family = "poisson", data =train)
tweet_pred_pr <- predict(model_tweet_forecast, newdata=test[,-3], type= "response")



# b) Time series

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


# c) A hybrid loss function method (HLF) that combines Poisson Regression and Time Series 

# Preprocessig data for gradient descent 
x <- data.frame(model.matrix(num_of_tweets ~. , data=train))
x$avg_shortage_day <- (x$avg_shortage_day - mean(x$avg_shortage_day))/sd(x$avg_shortage_day)
x$number_of_gas_stations <- (x$number_of_gas_stations - mean(x$number_of_gas_stations))/sd(x$number_of_gas_stations)
x$X.days.to.arrival. <- (x$X.days.to.arrival. - mean(x$X.days.to.arrival.))/sd(x$X.days.to.arrival.)
x$X.Wind.Speeds. <- (x$X.Wind.Speeds. - mean(x$X.Wind.Speeds.))/sd(x$X.Wind.Speeds.)
x <- as.matrix(x)
y <- as.matrix(train$num_of_tweets)


# Following codes is to cretate different train and test sets for the model to be tested

#  Testing on Gainesville and Miami
x1 <- x[1:14,]
x2 <- x[15:16,]


y1 <- y[1:14,]
y2 <- y[15:16,]

y_ts <- as.matrix(c(miami_pred_ts,gaine_pred_ts))


# For Testing on Tampa and Naples

x1 <- as.matrix(rbind(x[1:12,],x[15:16,]))
x2 <- x[13:14,]


y1 <-  c(y[1:12,],y[15:16,])
y2 <- y[13:14,]

y_ts <- as.matrix(c(naples_pred_ts,tampa_pred_ts))


# For Testing West Palm Beach and Jack 


x1 <- as.matrix(rbind(x[1:10,],x[15:16,]))
x2 <- x[11:12,]


y1 <-  c(y[1:10,],y[15:16,])
y2 <- y[11:12,]

y_ts <- as.matrix(c(palm_pred_ts,jack_pred_ts))

# For Testing on Orlando and Talahassee

x1 <- as.matrix(rbind(x[1:8,],x[15:16,]))
x2 <- x[9:10,]


y1 <-  c(y[1:8,],y[15:16,])
y2 <- y[9:10,]

y_ts <- as.matrix(c(orlando_pred_ts,tal_pred_ts))


# define the gradient function 
# in matrix form this is as follows:
grad <- function(x1, y1,x2, y_hat, theta,y_ts, lambda1, lambda2) {
  gradient_theta<- -1*((t(x1) %*% ((y1 - exp(x1%*% t(theta))))) + lambda1*(t(x2) %*% ((y_hat - exp(x2%*% t(theta))))))
  gradient_yhat <- -1*(lambda1*(x2%*% t(theta))) + 2*lambda2*(y_hat-y_ts)
  gradients = list(t(gradient_theta),gradient_yhat)
  return(gradients)
}

# define gradient descent update algorithm
grad.descent <- function(x1,y1,x2,y_ts, maxit){
  theta <- matrix(rep(0,ncol(x1)), nrow=1) # Initialize the parameters
  y_hat <-  matrix(rep(0,nrow(x2)))
  
  alpha = 10^-5 # set learning rate
  beta = 10^-5 # set learning rate
  lambda1 = 1 # setting regularization parameter 1
  lambda2 = 1 # setting regularization parameter 2
  for (i in 1:maxit) {
    theta <- theta - alpha*t(as.matrix(unlist(grad(x1, y1,x2, y_hat, theta,y_ts, lambda1, lambda2)[1])))  
    y_hat <- y_hat - beta*as.matrix(unlist(grad(x1, y1,x2, y_hat, theta,y_ts, lambda1, lambda2)[2]))
  }
  return(y_hat)
}

# results 
print(grad.descent(x1,y1,x2,y_ts,20000))
tweet_pred <- grad.descent(x1,y1,x2,y_ts,20000)



