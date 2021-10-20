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

#################################################################

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

#Exponential smoothing
hw_model = HoltWinters(training_set$y, 
                       seasonal = "multiplicative")

#forecasting
library(forecast)
predictions_hw = forecast(hw_model, h = nrow(test_set))

#Plotting
plot(predictions_hw,
     ylab = "Holt-Winters model")

#accuracy
accuracy(predictions_hw$mean, test_set$y)

#################################################################################

#Exponential smoothing
hw_model = HoltWinters(dataset$y, 
                       seasonal = "multiplicative")

#forecasting
library(forecast)
future_hw = forecast(hw_model, h = nrow(future))

#Plotting
plot(future_hw)

##################################################################################

#save forecasts
write.csv(predictions_hw$mean, 
          file = "R/Forecast/predictions_hw_r.csv", 
          row.names = FALSE)
write.csv(future_hw$mean, 
          file = "R/Future/future_hw_r.csv",
          row.names = FALSE)