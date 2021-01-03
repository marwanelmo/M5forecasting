# This script is used to generate dynamic arima forecasts.

library(mltools)
library(data.table)

### read sample data and sample submission
test<- read.csv('testR.csv')
test_iter <- test[,3:22]

##### Feature Engineering
testview <- head(test, 30)

# construct number and one hot datasets
test_number <- test[,3:151]
test_nonumber <- test %>% select(weekday, event_name_1, event_type_1)
test_nonumber <- one_hot(test_nonumber  %>% as.data.table())

#perform pca on one hot encoded variables
pca_onehot <- prcomp(test_nonumber, scale=F, tol = 0.1)
pca <- pca_onehot$x[,1:4] #%>% tail(n=100)

testmodel <- cbind(test_number, pca) 

pcats <- pca %>% ts(start = c(2014, as.numeric(format(as.Date("2011-01-11"), "%j"))),
                    frequency = 365)%>% as.zoo() %>% tail(1000) %>% as.ts()

remove(test_number, test_nonumber,pca_onehot, testview)




 
#Dynamic Regression
arimafromvector <- function(vec){
  "This is a function that takes in a vector and fits an arima model on last 100 days 
  and predict 14 days ahead." 
  hobbies <- vec
  hobbies <- test$HOBBIES_2_001
  startdata <- as.Date("2011-01-11")
  myts <- ts(hobbies,    
             start = c(2014, as.numeric(format(startdata, "%j"))),
             frequency = 365)
  #take last 100 obsevations
  data_zoo <- as.zoo(myts)
  last_zoo <- tail(data_zoo, 100)
  myts<- as.ts(last_zoo)
  
  #PCA into time serie
  myts2 <- ts(pca,    
             start = c(2014, as.numeric(format(startdata, "%j"))),
             frequency = 365)
  #take last 100 obsevations
  data_zoo2 <- as.zoo(myts2)
  last_zoo2 <- tail(data_zoo2, 100)
  myts2<- as.ts(last_zoo2)
  
  #fit an arima model and forecast 28 days ahead
  fit <- auto.arima(myts, xreg =myts2) 
  return(fit)
}

arimafromvector(test_iter$HOBBIES_2_003)



loopieloopdynamicarima <- function(dataframe, pca_mat){
  predictions<- matrix(ncol=28,nrow=ncol(dataframe))
  naam <- names(dataframe)
  
  for(i in 1:ncol(test_iter)){
    pr <- arimafromvector(test_iter[, i]) %>% as.numeric()
    predictions[i, ] <- pr
    
  }  
  predictions <- data.frame(predictions)
  row.names(predictions) <- paste0(naam, '_CA_3_validation')
  predictions <- rownames_to_column(predictions, 'F')
  names(predictions) <- names(sample_s)
  return(predictions)
}


loopieloopdynamicarima(test_iter)
