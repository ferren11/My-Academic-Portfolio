# flight price
library(readr)
flight <- read_csv("Clean_Dataset.csv")
View(flight)
summary(flight)

# make categorical variables into factor form
flight$airline = as.factor(flight$airline)
flight$source_city = as.factor(flight$source_city)
flight$departure_time = as.factor(flight$departure_time)
flight$stops = as.factor(flight$stops)
flight$arrival_time = as.factor(flight$arrival_time)
flight$destination_city = as.factor(flight$destination_city)
flight$class = as.factor(flight$class)

# copy data
flight_cpy = flight

# create dummy variable into data
# library(fastDummies)
# flight_price_cpy = dummy_cols(flight_price, 
#                              select_columns = c('class', 'stops'))

# stops:        d1  d2
# zero          0   0
# one           0   1
# two_or_more   1   1
d1_stops = ifelse(flight_cpy$stops == 'two_or_more', 1, 0)
d2_stops = ifelse(flight_cpy$stops == 'zero', 0, 1)
flight_cpy = cbind(flight_cpy, d1_stops)
flight_cpy = cbind(flight_cpy, d2_stops)

# class:
# economy = 0; business = 1
# flight_cpy$class = ifelse(flight_cpy$class == 'Business', 1, 0)
d_class = ifelse(flight_cpy$class == 'Business', 1, 0)
flight_cpy = cbind(flight_cpy, d_class)

View(flight_cpy)

# test = subset(flight_cpy, class == 0)

# using multiple linear regression
mlr_model = lm(price ~ duration+days_left, data = flight_cpy)
summary(mlr_model)

# using dummy regression
dummy_model = lm(price ~ duration+days_left+d_class+d1_stops+d2_stops, data = flight_cpy)
summary(dummy_model)

library(broom)
as.data.frame(glance(dummy_model)) 
