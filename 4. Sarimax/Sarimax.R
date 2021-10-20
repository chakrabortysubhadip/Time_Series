#loading data
data = read.csv("DHS_Daily_Report.csv")

#Tranforming Date Variable
data$Date = strptime(data$Date, "%m/%d/%Y")
data$Date = format(data$Date, "%Y-%m-%d")
data$Date = as.Date(data$Date)

#selecting variables
library(dplyr)
data = data %>% select(Date,
                       Total.Individuals.in.Shelter,
                       Easter,
                       Thanksgiving,
                       Christmas)

#transforming Y variable
colnames(data)[2] = "y"

#creating dataframes
future <- subset(data, data$Date > '2020-11-11')
dataset <- subset(data, data$Date <= '2020-11-11')

#tranforming the Y variables into Timeseries
library(lubridate)
dataset$y <- ts(dataset$y, 
                frequency = 365,
                start =c(2013, yday(head(dataset$Date, 1))))
#tranforming the Y variables into Timeseries
future$y <- ts(future$y, 
               frequency = 365,
               start = c(2020, yday(head(future$Date, 1))))

#visualization
plot.ts(dataset$y,
        ylab = "Demand")

####################################################################################

#training and test set
training_set = subset(dataset, dataset$Date <= '2020-09-30')
test_set = subset(dataset, dataset$Date > '2020-09-30')

#tranforming the Y variables into Timeseries
training_set$y <- ts(training_set$y, 
                     frequency = 365,
                     start = c(2013, yday(head(training_set$Date, 1))))
test_set$y <- ts(test_set$y, 
                 frequency = 365,
                 start = c(2020, yday(head(test_set$Date, 1))))

#Stationarity check
#install.packages("forecast")
library(forecast)
ndiffs(training_set$Y, alpha = 0.05, test = c("adf"))

#getting the regressors
training_reg = as.matrix(training_set[,3:5])
test_reg = as.matrix(test_set[,3:5])

#Modelling
sarimax_model = auto.arima(training_set$y, 
                           xreg = training_reg)
summary(sarimax_model)

#forecasting
predictions_sarimax = forecast(sarimax_model, xreg = test_reg)

#Plotting
plot(predictions_sarimax)

#accuracy
accuracy(predictions_sarimax$mean, test_set$y)

###########################################################################

#getting the regressors
training_reg = as.matrix(dataset[,3:5])
test_reg = as.matrix(future[,3:5])

#Modelling
sarimax_model = auto.arima(dataset$y, 
                           xreg = training_reg)
summary(sarimax_model)

#forecasting
future_sarimax = forecast(sarimax_model, xreg = test_reg)

#Plotting
plot(future_sarimax)

##########################################################################

#save forecasts
write.csv(predictions_sarimax$mean, 
          file = "R/Forecast/predictions_sarimax_r.csv",
          row.names = FALSE)
write.csv(future_sarimax$mean, 
          file = "R/Future/future_sarimax_r.csv",
          row.names = FALSE)
          
          
          
          
          
          
          
          
          
          
          
          
