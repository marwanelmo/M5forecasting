#### This script is used to generate graphs related to future work and the cumulative approach

setwd('C:/Users/rayaa/OneDrive/Bureaublad/UvA/Forecasting in complex systems/Final Assignment')

######libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(fpp2)
library(zoo)
library(tibble)
library(forecast)

###
test<- read.csv('testR.csv')

###### cumulative
cumsales <- test$HOBBIES_2_007 %>% tail(30) %>% cumsum() 
initalsupply <- rep(13, 30) 
supply = initalsupply-cumsales

sal <- data.frame(d = seq(1:30),s= supply)

ggplot(sal,aes(x=d, y=s))+ geom_line() + ylab('Supply left') + xlab('Days after t0') + 
  theme_bw()+ geom_hline(aes(yintercept=0), linetype='dashed')


##### forecasting
sal$s %>% head(14) %>% ts() %>% rwf(h=10, drift=T) %>% autoplot() + autolayer(sal$s %>% ts, show.legend=F)+
  theme_bw()+ geom_hline(aes(yintercept=0), linetype='dashed') + ylab('Supply left') + xlab('Days after t0')


