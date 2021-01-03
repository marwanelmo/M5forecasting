setwd('C:/Users/rayaa/OneDrive/Bureaublad/UvA/Forecasting in complex systems/Final Assignment')

######libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(fpp2)
library(zoo)
library(tibble)

###
test<- read.csv('testR.csv')
sample_s <- read.csv('sample_submission_afcs2020.csv')

##### univariate time series

# A function that creates an arima forecast on a vector
arimafromvector <- function(vec){
  "This is a function that takes in a vector and fits an arima model on last 100 days 
  and predict 14 days ahead." 
  hobbies <- vec
  startdata <- as.Date("2011-01-11")
  myts <- ts(hobbies,    
             start = c(2014, as.numeric(format(startdata, "%j"))),
             frequency = 365)
  
  data_zoo <- as.zoo(myts)
  last_zoo <- tail(data_zoo, 100)
  myts<- as.ts(last_zoo)
  
  fit <- auto.arima(myts) 
  df<- fit %>% forecast(h=28)
  return(df$mean %>% as.numeric())
}

# A function that creates an NN forecast on a vector
NNfromvector <- function(vec, extra){
  
  "This is a function that takes in a vector and fits an arima model on last 100 days 
  and predict 14 days ahead." 
  hobbies <- vec
  startdata <- as.Date("2011-01-11")
  myts <- ts(hobbies,    
             start = c(2014, as.numeric(format(startdata, "%j"))),
             frequency = 365)
  
  data_zoo <- as.zoo(myts)
  last_zoo <- tail(data_zoo, 1000)
  myts<- as.ts(last_zoo)
  
  fit <- nnetar(myts, xreg = extra) 
  df<- fit %>% forecast(h=28)
  return(df$mean %>% as.numeric())
}


NNfromvector(test$HOBBIES_2_001, pcats)

test_iter <- test[,3:151]
#test dataframe
#test_iter <- test[,3:22]


pb = txtProgressBar(min = 0, max = 149, initial = 0) 
stepi = 1
# A function that loops over dataframe column, and uses arimafromvector on all columns and saves it in dataframe
loopieloop <- function(dataframe){
  predictions<- matrix(ncol=28,nrow=ncol(dataframe))
  naam <- names(dataframe)
  for(i in 1:ncol(test_iter)){
    pr <- NNfromvector(test_iter[, i]) %>% as.numeric()
    predictions[i, ] <- pr
    stepi = stepi + 1
    setTxtProgressBar(pb,stepi)
  }  
  predictions <- data.frame(predictions)
  row.names(predictions) <- paste0(naam, '_CA_3_validation')
  predictions <- rownames_to_column(predictions, 'F')
  names(predictions) <- names(sample_s)
  return(predictions)
}

# Function is used on a testdataframe
pred <- loopieloop(test_iter)

pred2 <- pred
pred2[pred<0] <- 0

#Function that takes prediction and turns it in right format
write.csv(pred2, 'NN.csv', row.names = F)



