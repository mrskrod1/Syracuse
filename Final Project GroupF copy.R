#Read in data
hotels <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

#Load in packages
library(readr)
library(tidyverse)
library(ggplot2)
library(RCurl)
library(ggmap)
library(dplyr)

#Subset the data
hotelAnalysis<-hotels

#check the structure
str(hotelsAnalysis) #119,390 observations, 32 attributes

#check the column names to figure out which ones to keep
colnames(hotelsAnalysis)

#Converting variables into factors
hotelsAnalysis<- hotelsAnalysis%>%
  mutate(hotel=as.factor(hotel),
         is_canceled=as.factor(is_canceled),
         meal=as.factor(meal), country=as.factor(country),
         market_segment=as.factor(market_segment),
         distribution_channel=as.factor(distribution_channel),
         is_repeated_guest=as.factor(is_repeated_guest),
         reserved_room_type=as.factor(reserved_room_type),
         assigned_room_type=as.factor(assigned_room_type),
         deposit_type=as.factor(deposit_type),
         customer_type=as.factor(customer_type),
         reservation_status=as.factor(reservation_status),
         agent=as.factor(agent),company=as.factor(company),
        arrival_date_month=as.factor(arrival_date_month),
        arrival_date_year=as.factor(arrival_date_year))

#Combine children and babies to make kids column 
hotelsAnalysis$kids <- hotelsAnalysis$children + hotelsAnalysis$babies

#Types of hotel (City vs Resort)
hotelsAnalysis %>% group_by(hotel) %>% summarize(count=n()) #79330 bookings for City, 40,060 bookings for Resort

#Leadtime (number of days between the booking entering date and the arrival date)
mean(hotelsAnalysis$lead_time) #on average, booked 104.01days in advance
range(hotelsAnalysis$lead_time) #range between 0 and 737 days
hotelsAnalysis %>% group_by(hotel) %>% summarize(mean(lead_time), count=n()) 
#City Hotel on average booked 110 days in advance, Resort 92.7 days

ggplot(data=hotelsAnalysis) + geom_point(aes(x= kids, y=lead_time)) 
#surprisingly, the fewer kids, the longer lead time. I would think if you had more children 
#you're more likely to plan ahead, but data says otherwise. 

#arrival month
ggplot(data = hotelsAnalysis) + geom_bar(aes(x=arrival_date_month)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) 
#most popular month is August, least popular January

ggplot(data = hotelsAnalysis) + geom_bar(aes(x=market_segment)) + facet_wrap(~hotel) + theme(axis.text.x = element_text(angle=90)) 
#most common is online TA for city hotel and second common is online TA for resort hotels

#Regression model for Average daily and Arrival Date Month
model <-lm(adr ~ arrival_date_month, data=hotelsAnalysis)

summary(model)

#Create two new columns to calculate total number of days stayed and
# total cost

hotelsAnalysis$stay_in_nights_total<- hotelsAnalysis$stays_in_weekend_nights + hotelsAnalysis$stays_in_week_nights

hotelsAnalysis<- hotelsAnalysis %>% mutate(stay_in_nights_total= stays_in_weekend_nights +
            stays_in_week_nights, stay_cost_total = adr * stay_in_nights_total)
summary(hotelsAnalysis$stay_in_nights_total)
hotelsAnalysis$stay_cost_total<- hotelsAnalysis$adr * hotelsAnalysis$stay_in_nights_total
summary(hotelsAnalysis$stay_cost_total)


#Scatter plot with total nights and total cost

ggplot(hotelsAnalysis, aes(x=stay_in_nights_total, y= stay_cost_total, shape=hotel, 
                           color=adr)) +
  geom_point(alpha=1)


#Exploring data across different market segments
ggplot(hotelsAnalysis, aes(x=stay_in_nights_total, y=stay_cost_total, shape=hotel,
                           color=adr)) +
  geom_point() +
  facet_wrap(~market_segment)

#Comparing Year of arrival versus month of arrival, 2016 was the year with
#the most bookings and cancellations

ggplot(hotelsAnalysis,aes(x=arrival_date_year, y=adr, shape=hotel,
                          color=arrival_date_month)) +
  geom_col()

#Box Plot of Hotel Types

hotelsAnalysis%>%
  ggplot(aes(x=hotel, fill=adr)) +
  geom_bar()

#Distribution Channel
hotelsAnalysis%>%
  ggplot(aes(x=distribution_channel, fill= arrival_date_month)) +
  geom_bar()

#Histogram illustrating days in waiting list and Arrival date month

hotelsAnalysis%>%
  ggplot(aes(x=days_in_waiting_list, fill=arrival_date_month)) +
  geom_histogram(binwidth = 20)

#Deposit Type

hotelsAnalysis%>% 
  ggplot(aes(x=deposit_type, fill=arrival_date_month)) +
  geom_bar()

#Lead time and arrival date month

hotelsAnalysis%>%
  ggplot(aes(x=lead_time, fill= arrival_date_month)) +
  geom_histogram(binwidth = 15, position= "stack")


#Data Modeling
set.seed(1) #set a random seed
index<- sample(nrow(hotelsAnalysis), nrow(hotelsAnalysis)*0.3) #random selection of indices

hotelsAnalysis<- hotelsAnalysis%>% filter(market_segment!='Undefined')

test<-hotelsAnalysis[index,]  #save 30% as test data
training<-hotelsAnalysis[-index, ] #save the rest as training

#Initial training with 20 variables

training_1<-training[c('hotel', 'is_canceled', 'lead_time', 'adults', 'kids', 'meal',
                       'market_segment', 'distribution_channel', 'is_repeated_guest',
                       'previous_cancellations', 'previous_bookings_not_canceled',
                       'reserved_room_type', 'deposit_type', 'days_in_waiting_list',
                       'arrival_date_month', 'customer_type', 'adr', 'stay_nights_total',
                       'stay_cost_total', 'required_car_parking_spaces')]

#logit training model
training_1$arrival_date_month<- as.factor(training_1$arrival_date_month)
logit_training_model<- glm(as.factor(arrival_date_month) ~ ., family="binomial", data=training_1)
summary(logit_training_model)

#Predictions for logistic Regression
test$logit_pred_prob<-predict(logit_training_model, test, type="response")
test$logit_pred_class<-ifelse(test$logit_pred_prob>0.5,"1","0" )
table(test$arrival_date_month==test$logit_pred_class)

#Confusion Matrix
table(test$logit_pred_class, test$arrival_date_month, dnn=c("predicted", "actual"))







